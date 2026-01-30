use std::collections::HashMap;

pub mod keywords;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Number(u16),
    String(String),
    Comma,
    OpenParen,
    CloseParen,
    Plus,
    Minus,
    Colon,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Register(String),          // A, B, HL, IX...
    Immediate(u16),            // 1234
    IndirectImmediate(u16),    // (1234)
    IndirectRegister(String),  // (HL), (BC), (IX)
    IndirectIndex(String, i8), // (IX+d), (IY+d)
    Condition(String),         // NZ, Z, NC...
    Label(String),
    IndirectLabel(String),
    StringLiteral(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolType {
    Label, // Code label
    Byte,  // DB/DEFB
    Word,  // DW/DEFW
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Symbol {
    pub address: u16,
    pub kind: SymbolType,
}

pub fn assemble(code: &str) -> Result<(Vec<u8>, HashMap<String, Symbol>), String> {
    let mut labels = HashMap::new();
    let mut current_pc = 0u16;
    let mut instructions = Vec::new();

    // Pass 1: Build Symbol Table
    for (line_idx, line) in code.lines().enumerate() {
        let clean_line = line.split(';').next().unwrap_or("").trim();
        if clean_line.is_empty() {
            continue;
        }

        let mut tokens =
            tokenize(clean_line).map_err(|e| format!("Line {}: {}", line_idx + 1, e))?;

        if tokens.is_empty() {
            continue;
        }

        // Label Def
        if tokens.len() >= 2 {
            if let (Token::Identifier(name), Token::Colon) = (&tokens[0], &tokens[1]) {
                if labels.contains_key(name) {
                    return Err(format!("Line {}: Duplicate label '{}'", line_idx + 1, name));
                }

                // Determine symbol type based on what follows
                let mut sym_kind = SymbolType::Label;
                if tokens.len() > 2 {
                    if let Token::Identifier(next_mnemonic) = &tokens[2] {
                        match next_mnemonic.as_str() {
                            "DB" | "DEFB" => sym_kind = SymbolType::Byte,
                            "DW" | "DEFW" => sym_kind = SymbolType::Word,
                            _ => {}
                        }
                    }
                }

                labels.insert(
                    name.clone(),
                    Symbol {
                        address: current_pc,
                        kind: sym_kind,
                    },
                );
                tokens.drain(0..2);
            }
        }

        if tokens.is_empty() {
            continue;
        }

        let bytes = parse_instruction(&tokens, current_pc, &HashMap::new(), true)
            .map_err(|e| format!("Line {}: {}", line_idx + 1, e))?;

        instructions.push((line_idx, current_pc, tokens));
        current_pc += bytes.len() as u16;
    }

    // Pass 2: Code Gen
    let mut output = Vec::new();
    // In Pass 2 we need a map of String -> u16 for parse_instruction to work.
    // We can just project our Symbol map.
    let label_addresses: HashMap<String, u16> =
        labels.iter().map(|(k, v)| (k.clone(), v.address)).collect();

    for (line_idx, pc, tokens) in instructions {
        let bytes = parse_instruction(&tokens, pc, &label_addresses, false)
            .map_err(|e| format!("Line {}: {}", line_idx + 1, e))?;
        output.extend(bytes);
    }
    Ok((output, labels))
}

fn tokenize(text: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = text.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ':' => {
                tokens.push(Token::Colon);
                chars.next();
            }
            '"' => {
                chars.next(); // consume "
                let mut s = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '"' {
                        chars.next();
                        break;
                    }
                    s.push(c);
                    chars.next();
                }
                tokens.push(Token::String(s));
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            '(' => {
                tokens.push(Token::OpenParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::CloseParen);
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '-' => {
                tokens.push(Token::Minus);
                chars.next();
            }
            c if c.is_whitespace() => {
                chars.next();
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' || c == '\'' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Identifier(ident.to_ascii_uppercase()));
            }
            c if c.is_ascii_digit() => {
                let mut num_str = String::new();
                let mut is_hex = false;

                // Check for 0x prefix
                if c == '0' {
                    chars.next(); // consume 0
                    if let Some(&nc) = chars.peek() {
                        if nc == 'x' || nc == 'X' {
                            chars.next(); // consume x
                            is_hex = true;
                        } else {
                            num_str.push('0'); // restore 0 if not hex prefix
                        }
                    } else {
                        num_str.push('0');
                    }
                }

                // Allow standard hex digits if not prefix (handling implicit hex if needed, but sticking to 0x for clarity unless we see chars)
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_hexdigit() {
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                // Check for 'h' suffix
                if !is_hex && !num_str.is_empty() {
                    if let Some(&nc) = chars.peek() {
                        if nc == 'H' || nc == 'h' {
                            is_hex = true;
                            chars.next();
                        }
                    }
                }

                let val = if is_hex {
                    u16::from_str_radix(&num_str, 16).map_err(|_| "Invalid hex number")?
                } else {
                    num_str.parse::<u16>().map_err(|_| "Invalid number")?
                };
                tokens.push(Token::Number(val));
            }
            // Handle simple hex like $FF
            '$' => {
                chars.next();
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_hexdigit() {
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let val = u16::from_str_radix(&num_str, 16).map_err(|_| "Invalid hex number")?;
                tokens.push(Token::Number(val));
            }
            _ => return Err(format!("Unexpected character: {}", c)),
        }
    }
    Ok(tokens)
}

fn parse_operands(tokens: &[Token]) -> Result<Vec<Operand>, String> {
    let mut operands = Vec::new();
    if tokens.is_empty() {
        return Ok(operands);
    }

    let mut i = 0;
    while i < tokens.len() {
        if i > 0 {
            if let Token::Comma = tokens[i] {
                i += 1;
            } else {
                return Err("Expected comma".to_string());
            }
        }
        if i >= tokens.len() {
            break;
        }

        match &tokens[i] {
            Token::Minus => {
                i += 1;
                if i >= tokens.len() {
                    return Err("Expected number after minus".to_string());
                }
                if let Token::Number(n) = tokens[i] {
                    let val = -(n as i16) as u16;
                    operands.push(Operand::Immediate(val));
                    i += 1;
                } else {
                    return Err("Expected number after minus".to_string());
                }
            }
            Token::Identifier(r) => {
                if is_condition(r) {
                    operands.push(Operand::Condition(r.clone()));
                } else if is_register(r) {
                    operands.push(Operand::Register(r.clone()));
                } else {
                    operands.push(Operand::Label(r.clone()));
                }
                i += 1;
            }
            Token::String(s) => {
                operands.push(Operand::StringLiteral(s.clone()));
                i += 1;
            }
            Token::Number(n) => {
                operands.push(Operand::Immediate(*n));
                i += 1;
            }
            Token::OpenParen => {
                i += 1;
                if i >= tokens.len() {
                    return Err("Unexpected end in parens".to_string());
                }

                match &tokens[i] {
                    Token::Number(n) => {
                        operands.push(Operand::IndirectImmediate(*n));
                        i += 1;
                    }
                    Token::Identifier(r) => {
                        if is_register(r) {
                            let reg = r.clone();
                            i += 1;
                            if i < tokens.len() && matches!(tokens[i], Token::Plus | Token::Minus) {
                                // (IX+d)
                                let sign = match tokens[i] {
                                    Token::Plus => 1,
                                    Token::Minus => -1,
                                    _ => 1,
                                };
                                i += 1;
                                if i >= tokens.len() {
                                    return Err("Expected offset".to_string());
                                }
                                if let Token::Number(offset) = tokens[i] {
                                    let final_offset = (offset as i16 * sign as i16) as i8;
                                    operands.push(Operand::IndirectIndex(reg, final_offset));
                                    i += 1;
                                } else {
                                    return Err("Expected number offset".to_string());
                                }
                            } else {
                                operands.push(Operand::IndirectRegister(reg));
                            }
                        } else {
                            operands.push(Operand::IndirectLabel(r.clone()));
                            i += 1;
                        }
                    }
                    _ => return Err("Invalid start of indirect operand".to_string()),
                }

                if i >= tokens.len() || tokens[i] != Token::CloseParen {
                    return Err("Expected )".to_string());
                }
                i += 1;
            }
            _ => return Err("Invalid operand".to_string()),
        }
    }
    Ok(operands)
}

fn is_condition(s: &str) -> bool {
    matches!(s, "NZ" | "Z" | "NC" | "PO" | "PE" | "P" | "M")
}

fn is_register(s: &str) -> bool {
    keywords::REGISTERS.contains(&s)
}

fn resolve_immediate(
    op: &Operand,
    labels: &HashMap<String, u16>,
    is_dry_run: bool,
) -> Result<u16, String> {
    match op {
        Operand::Immediate(n) => Ok(*n),
        Operand::Label(s) => {
            if is_dry_run {
                Ok(0)
            } else {
                labels
                    .get(s)
                    .cloned()
                    .ok_or_else(|| format!("Label not found: {}", s))
            }
        }
        _ => Err("Not an immediate".to_string()),
    }
}

fn resolve_indirect(
    op: &Operand,
    labels: &HashMap<String, u16>,
    is_dry_run: bool,
) -> Result<u16, String> {
    match op {
        Operand::IndirectImmediate(n) => Ok(*n),
        Operand::IndirectLabel(s) => {
            if is_dry_run {
                Ok(0)
            } else {
                labels
                    .get(s)
                    .cloned()
                    .ok_or_else(|| format!("Label not found: {}", s))
            }
        }
        _ => Err("Not an indirect address".to_string()),
    }
}

fn parse_instruction(
    tokens: &[Token],
    pc: u16,
    labels: &HashMap<String, u16>,
    is_dry_run: bool,
) -> Result<Vec<u8>, String> {
    let mnemonic = match &tokens[0] {
        Token::Identifier(m) => m,
        _ => return Err("Expected mnemonic".to_string()),
    };
    let operands = parse_operands(&tokens[1..])?;

    match mnemonic.as_str() {
        "LD" => encode_ld(&operands, labels, is_dry_run),
        "INC" => encode_alu_uni(0x04, &operands),
        "DEC" => encode_alu_uni(0x05, &operands),
        "ADD" => encode_add(&operands, labels, is_dry_run),
        "ADC" => encode_alu_bin(0x88, 0xCE, &operands, labels, is_dry_run),
        "SUB" => encode_alu_bin(0x90, 0xD6, &operands, labels, is_dry_run),
        "SBC" => encode_sbc(&operands, labels, is_dry_run),
        "AND" => encode_alu_bin(0xA0, 0xE6, &operands, labels, is_dry_run),
        "XOR" => encode_alu_bin(0xA8, 0xEE, &operands, labels, is_dry_run),
        "OR" => encode_alu_bin(0xB0, 0xF6, &operands, labels, is_dry_run),
        "CP" => encode_alu_bin(0xB8, 0xFE, &operands, labels, is_dry_run),

        "HALT" => Ok(vec![0x76]),
        "NOP" => Ok(vec![0x00]),
        "DI" => Ok(vec![0xF3]),
        "EI" => Ok(vec![0xFB]),
        "EX" => encode_ex(&operands),
        "EXX" => Ok(vec![0xD9]),
        "DAA" => Ok(vec![0x27]),
        "CPL" => Ok(vec![0x2F]),
        "CCF" => Ok(vec![0x3F]),
        "SCF" => Ok(vec![0x37]),
        "RLA" => Ok(vec![0x17]),
        "RRA" => Ok(vec![0x1F]),
        "RLCA" => Ok(vec![0x07]),
        "RRCA" => Ok(vec![0x0F]),

        "JP" => encode_jp(&operands, labels, is_dry_run),
        "JR" => encode_jr(&operands, pc, labels, is_dry_run),
        "DJNZ" => encode_djnz(&operands, pc, labels, is_dry_run),
        "CALL" => encode_call(&operands, labels, is_dry_run),
        "RET" => encode_ret(&operands),
        "RST" => encode_rst(&operands),

        "PUSH" => encode_push(&operands),
        "POP" => encode_pop(&operands),

        "IN" => encode_in(&operands),
        "OUT" => encode_out(&operands),

        "ORG" => {
            if operands.len() != 1 {
                return Err("ORG expects 1 operand".to_string());
            }
            let target = resolve_immediate(&operands[0], labels, is_dry_run)?;
            if target < pc {
                // In dry run (Pass 1), we might have temporary 0 labels producing bad targets,
                // but ORG usually uses constants.
                // If using labels in ORG (advanced), Pass 1 might fail.
                // We assume ORG uses constants or pre-defined symbols.
                return Err("ORG cannot go backwards".to_string());
            }
            Ok(vec![0; (target - pc) as usize])
        }
        "DB" | "DEFB" => {
            let mut bytes = Vec::new();
            for op in operands {
                match op {
                    Operand::Immediate(n) => {
                        if n > 255 {
                            // If literal is hex 0x12, it's u16.
                            // But DB 0x12 is valid.
                            // Check if fits in u8?
                            // Yes.
                            return Err(format!("Value {} too large for DB", n));
                        }
                        bytes.push(n as u8);
                    }
                    Operand::StringLiteral(s) => {
                        bytes.extend_from_slice(s.as_bytes());
                    }
                    Operand::Label(l) => {
                        let val = if is_dry_run {
                            0
                        } else {
                            *labels.get(&l).unwrap_or(&0)
                        };
                        // Use just the low byte? Or error if > 255?
                        // Usually DB Label puts the low byte.
                        bytes.push((val & 0xFF) as u8);
                    }
                    _ => return Err("Invalid DB operand".to_string()),
                }
            }
            Ok(bytes)
        }
        "DW" | "DEFW" => {
            let mut bytes = Vec::new();
            for op in operands {
                match op {
                    Operand::Immediate(n) => {
                        bytes.push((n & 0xFF) as u8);
                        bytes.push((n >> 8) as u8);
                    }
                    Operand::Label(l) => {
                        let val = if is_dry_run {
                            0
                        } else {
                            *labels.get(&l).unwrap_or(&0)
                        };
                        bytes.push((val & 0xFF) as u8);
                        bytes.push((val >> 8) as u8);
                    }
                    _ => return Err("Invalid DW operand".to_string()),
                }
            }
            Ok(bytes)
        }

        "LDI" => Ok(vec![0xED, 0xA0]),
        "LDIR" => Ok(vec![0xED, 0xB0]),
        "CPI" => Ok(vec![0xED, 0xA1]),
        "CPIR" => Ok(vec![0xED, 0xB1]),

        _ => Err(format!("Unknown mnemonic: {}", mnemonic)),
    }
}

// --- Encoders ---

fn get_r_code(reg: &str) -> Option<u8> {
    match reg {
        "B" => Some(0),
        "C" => Some(1),
        "D" => Some(2),
        "E" => Some(3),
        "H" => Some(4),
        "L" => Some(5),
        "A" => Some(7),
        _ => None,
    }
}

fn get_rp_code(reg: &str) -> Option<u8> {
    match reg {
        "BC" => Some(0),
        "DE" => Some(1),
        "HL" => Some(2),
        "SP" => Some(3),
        _ => None,
    }
}

fn get_rp2_code(reg: &str) -> Option<u8> {
    match reg {
        "BC" => Some(0),
        "DE" => Some(1),
        "HL" => Some(2),
        "AF" => Some(3),
        _ => None,
    }
}

fn encode_ld(ops: &[Operand], labels: &HashMap<String, u16>, dry: bool) -> Result<Vec<u8>, String> {
    if ops.len() != 2 {
        return Err("LD requires 2 ops".to_string());
    }
    match (&ops[0], &ops[1]) {
        (Operand::Register(d), Operand::Register(s)) => {
            if let (Some(dc), Some(sc)) = (get_r_code(d), get_r_code(s)) {
                return Ok(vec![0x40 | (dc << 3) | sc]);
            }
            if d == "SP" {
                if s == "HL" {
                    return Ok(vec![0xF9]);
                }
                if s == "IX" {
                    return Ok(vec![0xDD, 0xF9]);
                }
                if s == "IY" {
                    return Ok(vec![0xFD, 0xF9]);
                }
            }
            if d == "I" && s == "A" {
                return Ok(vec![0xED, 0x47]);
            }
            if d == "R" && s == "A" {
                return Ok(vec![0xED, 0x4F]);
            }
            if d == "A" && s == "I" {
                return Ok(vec![0xED, 0x57]);
            }
            if d == "A" && s == "R" {
                return Ok(vec![0xED, 0x5F]);
            }
            Err("Invalid LD Register combination".to_string())
        }
        (Operand::Register(r), op2) if matches!(op2, Operand::Immediate(_) | Operand::Label(_)) => {
            let n = resolve_immediate(op2, labels, dry)?;
            if let Some(rc) = get_r_code(r) {
                return Ok(vec![0x06 | (rc << 3), n as u8]);
            }
            if let Some(rpc) = get_rp_code(r) {
                return Ok(vec![0x01 | (rpc << 4), (n & 0xFF) as u8, (n >> 8) as u8]);
            }
            if r == "IX" {
                return Ok(vec![0xDD, 0x21, (n & 0xFF) as u8, (n >> 8) as u8]);
            }
            if r == "IY" {
                return Ok(vec![0xFD, 0x21, (n & 0xFF) as u8, (n >> 8) as u8]);
            }
            Err("Invalid LD Register Immediate".to_string())
        }
        (Operand::Register(r), Operand::IndirectRegister(ir)) => {
            if let Some(rc) = get_r_code(r) {
                if ir == "HL" {
                    return Ok(vec![0x46 | (rc << 3)]);
                }
                if r == "A" && ir == "BC" {
                    return Ok(vec![0x0A]);
                }
                if r == "A" && ir == "DE" {
                    return Ok(vec![0x1A]);
                }
            }
            Err("Invalid LD r, (reg)".to_string())
        }
        (Operand::IndirectRegister(ir), Operand::Register(r)) => {
            if let Some(rc) = get_r_code(r) {
                if ir == "HL" {
                    return Ok(vec![0x70 | rc]);
                }
                if r == "A" && ir == "BC" {
                    return Ok(vec![0x02]);
                }
                if r == "A" && ir == "DE" {
                    return Ok(vec![0x12]);
                }
            }
            Err("Invalid LD (reg), r".to_string())
        }
        (Operand::IndirectRegister(ir), op2)
            if ir == "HL" && matches!(op2, Operand::Immediate(_) | Operand::Label(_)) =>
        {
            let n = resolve_immediate(op2, labels, dry)?;
            Ok(vec![0x36, n as u8])
        }
        (Operand::Register(r), Operand::IndirectIndex(idx, d)) => {
            if let Some(rc) = get_r_code(r) {
                let prefix = if idx == "IX" { 0xDD } else { 0xFD };
                return Ok(vec![prefix, 0x46 | (rc << 3), *d as u8]);
            }
            Err("Invalid LD r, (idx+d)".to_string())
        }
        (Operand::IndirectIndex(idx, d), Operand::Register(r)) => {
            if let Some(rc) = get_r_code(r) {
                let prefix = if idx == "IX" { 0xDD } else { 0xFD };
                return Ok(vec![prefix, 0x70 | rc, *d as u8]);
            }
            Err("Invalid LD (idx+d), r".to_string())
        }
        (Operand::IndirectIndex(idx, d), op2)
            if matches!(op2, Operand::Immediate(_) | Operand::Label(_)) =>
        {
            let n = resolve_immediate(op2, labels, dry)?;
            let prefix = if idx == "IX" { 0xDD } else { 0xFD };
            Ok(vec![prefix, 0x36, *d as u8, n as u8])
        }
        (Operand::Register(r), op2)
            if matches!(
                op2,
                Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
            ) =>
        {
            let nn = resolve_indirect(op2, labels, dry)?;
            let low = (nn & 0xFF) as u8;
            let high = (nn >> 8) as u8;
            if r == "A" {
                return Ok(vec![0x3A, low, high]);
            }
            if r == "HL" {
                return Ok(vec![0x2A, low, high]);
            }
            if r == "BC" || r == "DE" || r == "SP" {
                let rpc = get_rp_code(r).unwrap();
                return Ok(vec![0xED, 0x4B | (rpc << 4), low, high]);
            }
            if r == "IX" {
                return Ok(vec![0xDD, 0x2A, low, high]);
            }
            if r == "IY" {
                return Ok(vec![0xFD, 0x2A, low, high]);
            }
            Err("Invalid LD r, (nn)".to_string())
        }
        (op1, Operand::Register(r))
            if matches!(
                op1,
                Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
            ) =>
        {
            let nn = resolve_indirect(op1, labels, dry)?;
            let low = (nn & 0xFF) as u8;
            let high = (nn >> 8) as u8;
            if r == "A" {
                return Ok(vec![0x32, low, high]);
            }
            if r == "HL" {
                return Ok(vec![0x22, low, high]);
            }
            if r == "BC" || r == "DE" || r == "SP" {
                let rpc = get_rp_code(r).unwrap();
                return Ok(vec![0xED, 0x43 | (rpc << 4), low, high]);
            }
            if r == "IX" {
                return Ok(vec![0xDD, 0x22, low, high]);
            }
            if r == "IY" {
                return Ok(vec![0xFD, 0x22, low, high]);
            }
            Err("Invalid LD (nn), r".to_string())
        }
        _ => Err("Unsupported instruction".to_string()),
    }
}

fn encode_alu_uni(base: u8, ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("Needs 1 Op".to_string());
    }
    match &ops[0] {
        Operand::Register(r) => {
            if let Some(rc) = get_r_code(r) {
                return Ok(vec![base | (rc << 3)]);
            }
            if let Some(rpc) = get_rp_code(r) {
                let val = if base == 0x04 { 0x03 } else { 0x0B };
                return Ok(vec![val | (rpc << 4)]);
            }
            if r == "IX" {
                let val = if base == 0x04 { 0x23 } else { 0x2B };
                return Ok(vec![0xDD, val]);
            }
            if r == "IY" {
                let val = if base == 0x04 { 0x23 } else { 0x2B };
                return Ok(vec![0xFD, val]);
            }
            Err("Invalid register for INC/DEC".to_string())
        }
        Operand::IndirectRegister(r) if r == "HL" => Ok(vec![base | (6 << 3)]),
        Operand::IndirectIndex(idx, d) => {
            let prefix = if idx == "IX" { 0xDD } else { 0xFD };
            Ok(vec![prefix, base | (6 << 3), *d as u8])
        }
        _ => Err("Invalid INC/DEC".to_string()),
    }
}

fn encode_add(
    ops: &[Operand],
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    if ops.len() == 2 {
        if let Operand::Register(dest) = &ops[0] {
            if dest == "HL" {
                if let Operand::Register(src) = &ops[1] {
                    if let Some(rpc) = get_rp_code(src) {
                        return Ok(vec![0x09 | (rpc << 4)]);
                    }
                }
            }
            if dest == "IX" || dest == "IY" {
                let prefix = if dest == "IX" { 0xDD } else { 0xFD };
                if let Operand::Register(src) = &ops[1] {
                    let pp = match src.as_str() {
                        "BC" => 0,
                        "DE" => 1,
                        "IX" | "IY" => 2,
                        "SP" => 3,
                        _ => return Err("Invalid operand for ADD index".to_string()),
                    };
                    return Ok(vec![prefix, 0x09 | (pp << 4)]);
                }
            }
        }
    }
    if ops.len() == 2 {
        if let Operand::Register(r) = &ops[0] {
            if r == "A" {
                return encode_alu_bin(0x80, 0xC6, &ops[1..], labels, dry);
            }
        }
    } else if ops.len() == 1 {
        return encode_alu_bin(0x80, 0xC6, ops, labels, dry);
    }
    Err("Invalid ADD inputs".to_string())
}

fn encode_sbc(
    ops: &[Operand],
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    if ops.len() == 2 {
        if let Operand::Register(dest) = &ops[0] {
            if dest == "HL" {
                if let Operand::Register(src) = &ops[1] {
                    if let Some(rpc) = get_rp_code(src) {
                        return Ok(vec![0xED, 0x42 | (rpc << 4)]);
                    }
                }
            }
        }
    }
    if ops.len() == 2 {
        if let Operand::Register(r) = &ops[0] {
            if r == "A" {
                return encode_alu_bin(0x98, 0xDE, &ops[1..], labels, dry);
            }
        }
    }
    encode_alu_bin(0x98, 0xDE, ops, labels, dry)
}

fn encode_alu_bin(
    base_r: u8,
    base_n: u8,
    ops: &[Operand],
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("ALU ops count error".to_string());
    }
    match &ops[0] {
        Operand::Register(r) => {
            if let Some(rc) = get_r_code(r) {
                Ok(vec![base_r | rc])
            } else if r == "IXH" {
                Ok(vec![0xDD, base_r | 4])
            } else if r == "IXL" {
                Ok(vec![0xDD, base_r | 5])
            } else {
                Err("Invalid ALU register".to_string())
            }
        }
        op if matches!(op, Operand::Immediate(_) | Operand::Label(_)) => {
            let n = resolve_immediate(op, labels, dry)?;
            Ok(vec![base_n, n as u8])
        }
        Operand::IndirectRegister(r) if r == "HL" => Ok(vec![base_r | 6]),
        Operand::IndirectIndex(idx, d) => {
            let prefix = if idx == "IX" { 0xDD } else { 0xFD };
            Ok(vec![prefix, base_r | 6, *d as u8])
        }
        _ => Err("Invalid ALU operand".to_string()),
    }
}

fn get_condition_code(s: &str) -> Option<u8> {
    match s {
        "NZ" => Some(0),
        "Z" => Some(1),
        "NC" => Some(2),
        "C" => Some(3),
        "PO" => Some(4),
        "PE" => Some(5),
        "P" => Some(6),
        "M" => Some(7),
        _ => None,
    }
}

fn encode_jp(ops: &[Operand], labels: &HashMap<String, u16>, dry: bool) -> Result<Vec<u8>, String> {
    match ops.len() {
        1 => match &ops[0] {
            op if matches!(
                op,
                Operand::Immediate(_)
                    | Operand::Label(_)
                    | Operand::IndirectImmediate(_)
                    | Operand::IndirectLabel(_)
            ) =>
            {
                let nn = if matches!(
                    op,
                    Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
                ) {
                    resolve_indirect(op, labels, dry)?
                } else {
                    resolve_immediate(op, labels, dry)?
                };
                Ok(vec![0xC3, (nn & 0xFF) as u8, (nn >> 8) as u8])
            }
            Operand::IndirectRegister(r) | Operand::Register(r) if r == "HL" => Ok(vec![0xE9]),
            Operand::IndirectRegister(r) | Operand::Register(r) if r == "IX" => {
                Ok(vec![0xDD, 0xE9])
            }
            Operand::IndirectRegister(r) | Operand::Register(r) if r == "IY" => {
                Ok(vec![0xFD, 0xE9])
            }
            _ => Err("Invalid JP target".to_string()),
        },
        2 => {
            let cond = match &ops[0] {
                Operand::Condition(c) => Some(c.as_str()),
                Operand::Register(r) if r == "C" => Some("C"),
                _ => None,
            };
            if let Some(c) = cond {
                if let Some(cc) = get_condition_code(c) {
                    let nn = if matches!(
                        &ops[1],
                        Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
                    ) {
                        resolve_indirect(&ops[1], labels, dry)?
                    } else {
                        resolve_immediate(&ops[1], labels, dry)?
                    };
                    return Ok(vec![0xC2 | (cc << 3), (nn & 0xFF) as u8, (nn >> 8) as u8]);
                }
            }
            Err("Invalid JP condition/target".to_string())
        }
        _ => Err("Invalid JP args".to_string()),
    }
}

fn encode_jr(
    ops: &[Operand],
    pc: u16,
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    // JR d / JR C, d
    // Opcode size is 2 bytes. Offset is relative to PC+2.
    // Target = (PC + 2) + offset (signed i8)
    // Offset = Target - (PC + 2)
    match ops.len() {
        1 => {
            // JR d
            let target = resolve_immediate(&ops[0], labels, dry)?;
            let offset_val = (target as i32) - ((pc as i32) + 2);
            if !dry && (offset_val < -128 || offset_val > 127) {
                return Err("JR offset out of range".to_string());
            }
            Ok(vec![0x18, offset_val as i8 as u8])
        }
        2 => {
            let cond = match &ops[0] {
                Operand::Condition(c) => Some(c.as_str()),
                Operand::Register(r) if r == "C" => Some("C"),
                _ => None,
            };
            if let Some(c) = cond {
                if let Some(cc) = get_condition_code(c) {
                    if cc > 3 {
                        return Err("Invalid JR condition".to_string());
                    }
                    let target = resolve_immediate(&ops[1], labels, dry)?;
                    let offset_val = (target as i32) - ((pc as i32) + 2);
                    if !dry && (offset_val < -128 || offset_val > 127) {
                        return Err("JR offset out of range".to_string());
                    }
                    return Ok(vec![0x20 | (cc << 3), offset_val as i8 as u8]);
                }
            }
            Err("Invalid JR args".to_string())
        }
        _ => Err("Invalid JR args".to_string()),
    }
}

fn encode_djnz(
    ops: &[Operand],
    pc: u16,
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("DJNZ 1 op".to_string());
    }
    let target = resolve_immediate(&ops[0], labels, dry)?;
    let offset_val = (target as i32) - ((pc as i32) + 2);
    if !dry && (offset_val < -128 || offset_val > 127) {
        return Err("DJNZ offset out of range".to_string());
    }
    Ok(vec![0x10, offset_val as i8 as u8])
}

fn encode_call(
    ops: &[Operand],
    labels: &HashMap<String, u16>,
    dry: bool,
) -> Result<Vec<u8>, String> {
    match ops.len() {
        1 => {
            let nn = match &ops[0] {
                op if matches!(
                    op,
                    Operand::Immediate(_)
                        | Operand::IndirectImmediate(_)
                        | Operand::Label(_)
                        | Operand::IndirectLabel(_)
                ) =>
                {
                    if matches!(
                        op,
                        Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
                    ) {
                        resolve_indirect(op, labels, dry)?
                    } else {
                        resolve_immediate(op, labels, dry)?
                    }
                }
                _ => return Err("CALL needs address".to_string()),
            };
            Ok(vec![0xCD, (nn & 0xFF) as u8, (nn >> 8) as u8])
        }
        2 => {
            let cond = match &ops[0] {
                Operand::Condition(c) => Some(c.as_str()),
                Operand::Register(r) if r == "C" => Some("C"),
                _ => None,
            };
            if let Some(c) = cond {
                if let Some(cc) = get_condition_code(c) {
                    let nn = match &ops[1] {
                        op if matches!(
                            op,
                            Operand::Immediate(_)
                                | Operand::IndirectImmediate(_)
                                | Operand::Label(_)
                                | Operand::IndirectLabel(_)
                        ) =>
                        {
                            if matches!(
                                op,
                                Operand::IndirectImmediate(_) | Operand::IndirectLabel(_)
                            ) {
                                resolve_indirect(op, labels, dry)?
                            } else {
                                resolve_immediate(op, labels, dry)?
                            }
                        }
                        _ => return Err("CALL needs address".to_string()),
                    };
                    Ok(vec![0xC4 | (cc << 3), (nn & 0xFF) as u8, (nn >> 8) as u8])
                } else {
                    Err("Bad cond".to_string())
                }
            } else {
                Err("Call format error".to_string())
            }
        }
        _ => Err("CALL args".to_string()),
    }
}

fn encode_ret(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.is_empty() {
        return Ok(vec![0xC9]);
    }
    if ops.len() == 1 {
        let cond = match &ops[0] {
            Operand::Condition(c) => Some(c.as_str()),
            Operand::Register(r) if r == "C" => Some("C"),
            _ => None,
        };
        if let Some(c) = cond {
            if let Some(cc) = get_condition_code(c) {
                return Ok(vec![0xC0 | (cc << 3)]);
            }
        }
    }
    Err("RET args".to_string())
}

fn encode_rst(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("RST 1 op".to_string());
    }
    if let Operand::Immediate(n) = &ops[0] {
        if *n & 0xC7 != 0 {
            return Err("Invalid RST address".to_string());
        }
        return Ok(vec![0xC7 | (*n as u8)]);
    }
    Err("RST invalid".to_string())
}

fn encode_push(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("PUSH 1 op".to_string());
    }
    if let Operand::Register(r) = &ops[0] {
        if let Some(c) = get_rp2_code(r) {
            return Ok(vec![0xC5 | (c << 4)]);
        }
        if r == "IX" {
            return Ok(vec![0xDD, 0xE5]);
        }
        if r == "IY" {
            return Ok(vec![0xFD, 0xE5]);
        }
    }
    Err("PUSH invalid".to_string())
}
fn encode_pop(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 1 {
        return Err("POP 1 op".to_string());
    }
    if let Operand::Register(r) = &ops[0] {
        if let Some(c) = get_rp2_code(r) {
            return Ok(vec![0xC1 | (c << 4)]);
        }
        if r == "IX" {
            return Ok(vec![0xDD, 0xE1]);
        }
        if r == "IY" {
            return Ok(vec![0xFD, 0xE1]);
        }
    }
    Err("POP invalid".to_string())
}

fn encode_ex(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 2 {
        return Err("EX 2 ops".to_string());
    }
    match (&ops[0], &ops[1]) {
        (Operand::Register(r1), Operand::Register(r2)) => {
            if r1 == "DE" && r2 == "HL" {
                return Ok(vec![0xEB]);
            }
            if r1 == "AF" && (r2 == "AF'" || r2 == "AF") {
                return Ok(vec![0x08]);
            }
            Err("Invalid EX".to_string())
        }
        (Operand::IndirectRegister(r1), Operand::Register(r2)) if r1 == "SP" => {
            if r2 == "HL" {
                return Ok(vec![0xE3]);
            }
            if r2 == "IX" {
                return Ok(vec![0xDD, 0xE3]);
            }
            if r2 == "IY" {
                return Ok(vec![0xFD, 0xE3]);
            }
            Err("Invalid EX (SP)".to_string())
        }
        _ => Err("Invalid EX combo".to_string()),
    }
}

fn encode_in(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 2 {
        return Err("IN 2 ops".to_string());
    }
    match (&ops[0], &ops[1]) {
        (Operand::Register(r), Operand::IndirectImmediate(n)) if r == "A" => {
            Ok(vec![0xDB, *n as u8])
        }
        (Operand::Register(r), Operand::IndirectRegister(ir)) if ir == "C" => {
            if let Some(rc) = get_r_code(r) {
                Ok(vec![0xED, 0x40 | (rc << 3)])
            } else {
                Err("Invalid IN reg".to_string())
            }
        }
        _ => Err("Invalid IN form".to_string()),
    }
}
fn encode_out(ops: &[Operand]) -> Result<Vec<u8>, String> {
    if ops.len() != 2 {
        return Err("OUT 2 ops".to_string());
    }
    match (&ops[0], &ops[1]) {
        (Operand::IndirectImmediate(n), Operand::Register(r)) if r == "A" => {
            Ok(vec![0xD3, *n as u8])
        }
        (Operand::IndirectRegister(ir), Operand::Register(r)) if ir == "C" => {
            if let Some(rc) = get_r_code(r) {
                Ok(vec![0xED, 0x41 | (rc << 3)])
            } else {
                Err("Invalid OUT reg".to_string())
            }
        }
        _ => Err("Invalid OUT form".to_string()),
    }
}

#[cfg(test)]
mod tests;
