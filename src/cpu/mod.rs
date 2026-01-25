mod alu;

use std::cell::RefCell;
#[cfg(test)]
use std::collections::VecDeque;
use std::rc::Rc;
use std::usize;

use crate::{
    cpu::alu::{
        add_16,
        alu_op::{self, add},
        bit, dec, inc, res,
        rot::{self, RotOperation},
        set, sub_16,
    },
    traits::{MemoryMapper, SyncronousComponent},
};

#[cfg(test)]
macro_rules! test_log {
    ($self:expr, $fmt:expr $(, $($arg:tt)+)? ) => {
        $self.test_callback.1(&format!($fmt $(, $($arg)+)?), &mut $self.test_callback.0)
    };
}

#[cfg(not(test))]
macro_rules! test_log {
    ($self:expr, $fmt:expr $(, $($arg:tt)+)? ) => {
        ()
    };
}

mod flags {
    pub const CARRY: u8 = 0b00000001;
    pub const ADD_SUB: u8 = 0b00000010;
    pub const PARITY_OVERFLOW: u8 = 0b00000100;
    pub const X: u8 = 0b00001000;
    pub const HALF_CARRY: u8 = 0b00010000;
    pub const Y: u8 = 0b00100000;
    pub const ZERO: u8 = 0b01000000;
    pub const SIGN: u8 = 0b10000000;
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum GPR {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum RegisterPair {
    BC,
    DE,
    HL,
    AF,
    SP,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Flag {
    C,
    N,
    PV,
    Y,
    H,
    X,
    Z,
    S,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum SystemRegister {
    PC,
    I,
    R,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum IndexRegister {
    IX,
    IY,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum IndexRegisterPart {
    IXH,
    IXL,
    IYH,
    IYL,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum AddressingMode {
    Register(GPR),
    RegisterIndirect(RegisterPair),
    Indexed(IndexRegister, i8),
    Immediate(u8),
    Absolute(u16),
    System(SystemRegister),
    IndexRegisterPart(IndexRegisterPart),
    RegisterPair(RegisterPair),
    IndexRegister(IndexRegister),
    ImmediateExtended(u16),
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Condition {
    NZ,
    Z,
    NC,
    C,
    PO,
    PE,
    P,
    M,
}

enum BlockInstruction {
    LDI,
    LDD,
    LDIR,
    LDDR,
    CPI,
    CPD,
    CPIR,
    CPDR,
    INI,
    IND,
    INIR,
    INDR,
    OUTI,
    OUTD,
    OTIR,
    OTDR,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum PrefixAddressing {
    HL,
    IX,
    IY,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum ALUOperation {
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
}

#[derive(Default, Clone, Copy, Debug)]
struct AFSet {
    a: u8,
    f: u8,
}

#[derive(Default, Clone, Copy, Debug)]
struct GeneralSet {
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
}

fn decode_opcode(opcode: u8) -> (u8, u8, u8, u8, bool) // x, y, z, p, q
    /* See http://www.z80.info/decoding.htm for more information
    x = the opcode's 1st octal digit (i.e. bits 7-6)
    y = the opcode's 2nd octal digit (i.e. bits 5-3)
    z = the opcode's 3rd octal digit (i.e. bits 2-0)
    p = y rightshifted one position (i.e. bits 5-4)
    q = y modulo 2 (i.e. bit 3) 
    */ {
    let x = (opcode >> 6) & 0b11;
    let y = (opcode >> 3) & 0b111;
    let z = opcode & 0b111;
    let p = (y >> 1) & 0b11;
    let q = (y & 0b1) == 1;
    (x, y, z, p, q)
}

pub struct Z80A {
    // registers
    af_registers: [AFSet; 2],
    active_af: usize,

    general_registers: [GeneralSet; 2],
    active_general: usize,

    // special registers
    PC: u16,
    SP: u16,
    IX: u16,
    IY: u16,
    I: u8,
    R: u8,

    memory: Rc<RefCell<dyn MemoryMapper>>,

    cycles: u64,
    halted: bool,

    #[cfg(test)]
    test_callback: (
        VecDeque<String>,
        Box<dyn FnMut(&str, &mut VecDeque<String>)>,
    ),
}

impl Z80A {
    pub fn get_pc(&self) -> u16 {
        self.PC
    }

    pub fn get_sp(&self) -> u16 {
        self.SP
    }

    pub fn get_ix(&self) -> u16 {
        self.IX
    }

    pub fn get_iy(&self) -> u16 {
        self.IY
    }

    pub fn is_halted(&self) -> bool {
        self.halted
    }

    pub fn set_halted(&mut self, halted: bool) {
        self.halted = halted;
    }

    pub fn new(memory: Rc<RefCell<dyn MemoryMapper>>) -> Self {
        Z80A {
            af_registers: [AFSet::default(); 2],
            active_af: 0,
            general_registers: [GeneralSet::default(); 2],
            active_general: 0,
            PC: 0,
            SP: 0,
            IX: 0,
            IY: 0,
            I: 0,
            R: 0,
            memory,
            cycles: 0,
            halted: false,

            #[cfg(test)]
            test_callback: (
                VecDeque::new(),
                Box::new(|s, q| q.push_front(s.to_string())),
            ),
        }
    }

    fn fetch(&mut self) -> u8 {
        let data = self.memory.borrow().read(self.PC);
        self.PC = self.PC.wrapping_add(1);
        data
    }

    fn fetch_word(&mut self) -> u16 {
        let low = self.fetch() as u16;
        let high = self.fetch() as u16;
        (high << 8) | low
    }

    fn fetch_displacement(&mut self) -> i8 {
        self.fetch() as i8
    }

    fn ex_af_af_prime(&mut self) {
        self.active_af = 1 - self.active_af;
    }

    fn exx(&mut self) {
        self.active_general = 1 - self.active_general;
    }

    fn set_register(&mut self, reg: GPR, value: u8) {
        match reg {
            GPR::A => self.af_registers[self.active_af].a = value,
            GPR::F => self.af_registers[self.active_af].f = value,
            GPR::B => self.general_registers[self.active_general].b = value,
            GPR::C => self.general_registers[self.active_general].c = value,
            GPR::D => self.general_registers[self.active_general].d = value,
            GPR::E => self.general_registers[self.active_general].e = value,
            GPR::H => self.general_registers[self.active_general].h = value,
            GPR::L => self.general_registers[self.active_general].l = value,
        }
    }

    pub fn get_register(&self, reg: GPR) -> u8 {
        match reg {
            GPR::A => self.af_registers[self.active_af].a,
            GPR::F => self.af_registers[self.active_af].f,
            GPR::B => self.general_registers[self.active_general].b,
            GPR::C => self.general_registers[self.active_general].c,
            GPR::D => self.general_registers[self.active_general].d,
            GPR::E => self.general_registers[self.active_general].e,
            GPR::H => self.general_registers[self.active_general].h,
            GPR::L => self.general_registers[self.active_general].l,
        }
    }

    pub fn get_register_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::AF => {
                let regs = &self.af_registers[self.active_af];
                ((regs.a as u16) << 8) | (regs.f as u16)
            }
            RegisterPair::BC => {
                let regs = &self.general_registers[self.active_general];
                ((regs.b as u16) << 8) | (regs.c as u16)
            }
            RegisterPair::DE => {
                let regs = &self.general_registers[self.active_general];
                ((regs.d as u16) << 8) | (regs.e as u16)
            }
            RegisterPair::HL => {
                let regs = &self.general_registers[self.active_general];
                ((regs.h as u16) << 8) | (regs.l as u16)
            }
            RegisterPair::SP => self.SP,
        }
    }

    fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
        match pair {
            RegisterPair::AF => {
                let regs = &mut self.af_registers[self.active_af];
                regs.a = (value >> 8) as u8;
                regs.f = (value & 0xFF) as u8;
            }
            RegisterPair::BC => {
                let regs = &mut self.general_registers[self.active_general];
                regs.b = (value >> 8) as u8;
                regs.c = (value & 0xFF) as u8;
            }
            RegisterPair::DE => {
                let regs = &mut self.general_registers[self.active_general];
                regs.d = (value >> 8) as u8;
                regs.e = (value & 0xFF) as u8;
            }
            RegisterPair::HL => {
                let regs = &mut self.general_registers[self.active_general];
                regs.h = (value >> 8) as u8;
                regs.l = (value & 0xFF) as u8;
            }
            RegisterPair::SP => self.SP = value,
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        let f = self.af_registers[self.active_af].f;
        match flag {
            Flag::C => f & flags::CARRY != 0,
            Flag::N => f & flags::ADD_SUB != 0,
            Flag::PV => f & flags::PARITY_OVERFLOW != 0,
            Flag::Y => f & flags::X != 0,
            Flag::H => f & flags::HALF_CARRY != 0,
            Flag::X => f & flags::Y != 0,
            Flag::Z => f & flags::ZERO != 0,
            Flag::S => f & flags::SIGN != 0,
        }
    }

    pub fn set_flag(&mut self, value: bool, flag: Flag) {
        let f = &mut self.af_registers[self.active_af].f;
        if value {
            match flag {
                Flag::C => *f |= flags::CARRY,
                Flag::N => *f |= flags::ADD_SUB,
                Flag::PV => *f |= flags::PARITY_OVERFLOW,
                Flag::Y => *f |= flags::X,
                Flag::H => *f |= flags::HALF_CARRY,
                Flag::X => *f |= flags::Y,
                Flag::Z => *f |= flags::ZERO,
                Flag::S => *f |= flags::SIGN,
            }
        } else {
            match flag {
                Flag::C => *f &= !flags::CARRY,
                Flag::N => *f &= !flags::ADD_SUB,
                Flag::PV => *f &= !flags::PARITY_OVERFLOW,
                Flag::Y => *f &= !flags::X,
                Flag::H => *f &= !flags::HALF_CARRY,
                Flag::X => *f &= !flags::Y,
                Flag::Z => *f &= !flags::ZERO,
                Flag::S => *f &= !flags::SIGN,
            }
        }
    }

    pub fn get_flag_i(&self, usize: usize) -> bool {
        self.af_registers[self.active_af].f & (1 << usize) != 0
    }

    fn set_system_register(&mut self, reg: SystemRegister, value: u16) {
        match reg {
            SystemRegister::PC => self.PC = value,
            SystemRegister::I => self.I = value as u8,
            SystemRegister::R => self.R = value as u8,
        }
    }

    fn get_system_register(&self, reg: SystemRegister) -> u16 {
        match reg {
            SystemRegister::PC => self.PC,
            SystemRegister::I => self.I as u16,
            SystemRegister::R => self.R as u16,
        }
    }

    fn set_index_register_part(&mut self, reg: IndexRegisterPart, value: u8) {
        match reg {
            IndexRegisterPart::IXH => self.IX = (self.IX & 0x00FF) | ((value as u16) << 8),
            IndexRegisterPart::IXL => self.IX = (self.IX & 0xFF00) | (value as u16),
            IndexRegisterPart::IYH => self.IY = (self.IY & 0x00FF) | ((value as u16) << 8),
            IndexRegisterPart::IYL => self.IY = (self.IY & 0xFF00) | (value as u16),
        }
    }

    fn get_index_register_part(&self, reg: IndexRegisterPart) -> u8 {
        match reg {
            IndexRegisterPart::IXH => (self.IX >> 8) as u8,
            IndexRegisterPart::IXL => (self.IX & 0x00FF) as u8,
            IndexRegisterPart::IYH => (self.IY >> 8) as u8,
            IndexRegisterPart::IYL => (self.IY & 0x00FF) as u8,
        }
    }

    fn read_8(&mut self, mode: AddressingMode) -> u8 {
        match mode {
            AddressingMode::Register(r) => self.get_register(r),
            AddressingMode::RegisterIndirect(rp) => {
                let addr = self.get_register_pair(rp);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add_signed(disp as i16);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Immediate(n) => n,
            AddressingMode::Absolute(addr) => self.memory.borrow().read(addr),
            AddressingMode::System(r) => (self.get_system_register(r) & 0xFF) as u8,
            AddressingMode::IndexRegisterPart(r) => self.get_index_register_part(r),
            _ => panic!("Invalid addressing mode for read_8."),
        }
    }

    fn write_8(&mut self, mode: AddressingMode, value: u8) {
        match mode {
            AddressingMode::Register(r) => self.set_register(r, value),
            AddressingMode::RegisterIndirect(rp) => {
                let addr = self.get_register_pair(rp);
                self.memory.borrow_mut().write(addr, value);
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add_signed(disp as i16);
                self.memory.borrow_mut().write(addr, value);
            }
            AddressingMode::Absolute(addr) => self.memory.borrow_mut().write(addr, value),
            AddressingMode::System(r) => self.set_system_register(r, value as u16),
            AddressingMode::IndexRegisterPart(r) => self.set_index_register_part(r, value),
            _ => panic!("Invalid addressing mode for write_8."),
        }
    }

    fn read_16(&mut self, mode: AddressingMode) -> u16 {
        match mode {
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::IndexRegister(idx) => self.get_index_register(idx),
            AddressingMode::System(r) => self.get_system_register(r),
            AddressingMode::ImmediateExtended(nn) => nn,
            AddressingMode::Absolute(addr) => self.memory.borrow().read_word(addr),
            _ => panic!("Invalid addressing mode for read_16."),
        }
    }

    fn write_16(&mut self, mode: AddressingMode, value: u16) {
        match mode {
            AddressingMode::RegisterPair(rp) => self.set_register_pair(rp, value),
            AddressingMode::IndexRegister(idx) => self.set_index_register(idx, value),
            AddressingMode::System(r) => self.set_system_register(r, value),
            AddressingMode::Absolute(addr) => self.memory.borrow_mut().write_word(addr, value),
            _ => panic!("Invalid addressing mode for write_16."),
        }
    }

    fn get_index_register(&self, index: IndexRegister) -> u16 {
        match index {
            IndexRegister::IX => self.IX,
            IndexRegister::IY => self.IY,
        }
    }

    fn set_index_register(&mut self, index: IndexRegister, value: u16) {
        match index {
            IndexRegister::IX => self.IX = value,
            IndexRegister::IY => self.IY = value,
        }
    }

    fn push(&mut self, value: u16) {
        self.SP = self.SP.wrapping_sub(2);
        self.memory.borrow_mut().write_word(self.SP, value);
    }

    fn pop(&mut self) -> u16 {
        let value = self.memory.borrow().read_word(self.SP);
        self.SP = self.SP.wrapping_add(2);
        value
    }

    // TABLES FROM http://www.z80.info/decoding.htm

    fn table_r(&mut self, p: u8) -> AddressingMode {
        match p {
            0 => {
                test_log!(self, "B");
                AddressingMode::Register(GPR::B)
            }
            1 => {
                test_log!(self, "C");
                AddressingMode::Register(GPR::C)
            }
            2 => {
                test_log!(self, "D");
                AddressingMode::Register(GPR::D)
            }
            3 => {
                test_log!(self, "E");
                AddressingMode::Register(GPR::E)
            }
            4 => {
                test_log!(self, "H");
                AddressingMode::Register(GPR::H)
            }
            5 => {
                test_log!(self, "L");
                AddressingMode::Register(GPR::L)
            }
            6 => {
                test_log!(self, "(HL)");
                AddressingMode::RegisterIndirect(RegisterPair::HL)
            }
            7 => {
                test_log!(self, "A");
                AddressingMode::Register(GPR::A)
            }
            _ => unreachable!("Invalid p value"), // should never happen
        }
    }

    fn table_alu(&mut self, y: u8) -> ALUOperation {
        match y {
            0 => {
                test_log!(self, "ADD A");
                ALUOperation::ADD
            }
            1 => {
                test_log!(self, "ADC A");
                ALUOperation::ADC
            }
            2 => {
                test_log!(self, "SUB A");
                ALUOperation::SUB
            }
            3 => {
                test_log!(self, "SBC A");
                ALUOperation::SBC
            }
            4 => {
                test_log!(self, "AND A");
                ALUOperation::AND
            }
            5 => {
                test_log!(self, "XOR A");
                ALUOperation::XOR
            }
            6 => {
                test_log!(self, "OR A");
                ALUOperation::OR
            }
            7 => {
                test_log!(self, "CP A");
                ALUOperation::CP
            }
            _ => unreachable!("Invalid y value"), // should never happen
        }
    }

    fn alu_op(&mut self, op: ALUOperation, value: u8) {
        match op {
            ALUOperation::ADD => {
                let (result, flags) = alu_op::add(self.get_register(GPR::A), value, false);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::ADC => {
                let carry = self.get_flag(Flag::C);
                let (result, flags) = alu_op::add(self.get_register(GPR::A), value, carry);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::SUB => {
                let (result, flags) = alu_op::sub(self.get_register(GPR::A), value, false);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::SBC => {
                let carry = self.get_flag(Flag::C);
                let (result, flags) = alu_op::sub(self.get_register(GPR::A), value, carry);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::AND => {
                let (result, flags) = alu_op::and(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::OR => {
                let (result, flags) = alu_op::or(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::XOR => {
                let (result, flags) = alu_op::xor(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.af_registers[self.active_af].f = flags;
            }
            ALUOperation::CP => {
                let (_, flags) = alu_op::sub(self.get_register(GPR::A), value, false);
                // result is ignored for CP
                self.af_registers[self.active_af].f = flags;
            }
        }
    }

    fn inc_op(&mut self, dest: AddressingMode) {
        let value = self.read_8(dest);
        let (result, flags) = inc(value);

        // Preserve Carry flag
        let current_carry = self.get_flag(Flag::C) as u8;
        self.af_registers[self.active_af].f = flags | current_carry;

        self.write_8(dest, result);
    }

    fn dec_op(&mut self, dest: AddressingMode) {
        let value = self.read_8(dest);
        let (result, flags) = dec(value);

        // Preserve Carry flag
        let current_carry = self.af_registers[self.active_af].f & flags::CARRY;
        self.af_registers[self.active_af].f = flags | current_carry;

        self.write_8(dest, result);
    }

    fn inc_16_op(&mut self, dest: AddressingMode) {
        let val = self.read_16(dest);
        let result = val.wrapping_add(1);
        self.write_16(dest, result);
    }

    fn dec_16_op(&mut self, dest: AddressingMode) {
        let val = self.read_16(dest);
        let result = val.wrapping_sub(1);
        self.write_16(dest, result);
    }

    fn add_16_op(&mut self, dest: AddressingMode, src: AddressingMode, use_carry: bool) {
        let val_dest = self.read_16(dest);
        let val_src = self.read_16(src);

        let (result, flags) = add_16(
            val_dest,
            val_src,
            self.af_registers[self.active_af].f,
            use_carry,
        );
        self.af_registers[self.active_af].f = flags;

        self.write_16(dest, result);
    }

    fn sub_16_op(&mut self, dest: AddressingMode, src: AddressingMode, use_carry: bool) {
        let val_dest = self.read_16(dest);
        let val_src = self.read_16(src);

        let (result, flags) = sub_16(
            val_dest,
            val_src,
            self.af_registers[self.active_af].f,
            use_carry,
        );
        self.af_registers[self.active_af].f = flags | flags::ADD_SUB;

        self.write_16(dest, result);
    }

    fn table_rp(&mut self, p: u8) -> AddressingMode {
        match p {
            0 => {
                test_log!(self, "BC");
                AddressingMode::RegisterPair(RegisterPair::BC)
            }
            1 => {
                test_log!(self, "DE");
                AddressingMode::RegisterPair(RegisterPair::DE)
            }
            2 => {
                test_log!(self, "HL");
                AddressingMode::RegisterPair(RegisterPair::HL)
            }
            3 => {
                test_log!(self, "SP");
                AddressingMode::RegisterPair(RegisterPair::SP)
            }
            _ => panic!("Invalid p value"), // should never happen
        }
    }

    fn table_rp2(&mut self, p: u8) -> AddressingMode {
        match p {
            0 => {
                test_log!(self, "BC");
                AddressingMode::RegisterPair(RegisterPair::BC)
            }
            1 => {
                test_log!(self, "DE");
                AddressingMode::RegisterPair(RegisterPair::DE)
            }
            2 => {
                test_log!(self, "HL");
                AddressingMode::RegisterPair(RegisterPair::HL)
            }
            3 => {
                test_log!(self, "AF");
                AddressingMode::RegisterPair(RegisterPair::AF)
            }
            _ => unreachable!("Invalid p value"), // should never happen
        }
    }

    fn table_cc(&mut self, y: u8) -> Condition {
        match y {
            0 => {
                test_log!(self, "NZ");
                Condition::NZ
            }
            1 => {
                test_log!(self, "Z");
                Condition::Z
            }
            2 => {
                test_log!(self, "NC");
                Condition::NC
            }
            3 => {
                test_log!(self, "C");
                Condition::C
            }
            4 => {
                test_log!(self, "PO");
                Condition::PO
            }
            5 => {
                test_log!(self, "PE");
                Condition::PE
            }
            6 => {
                test_log!(self, "P");
                Condition::P
            }
            7 => {
                test_log!(self, "M");
                Condition::M
            }
            _ => unreachable!("Invalid y value"), // should never happen
        }
    }

    fn table_bli(&mut self, a: u8, b: u8) -> BlockInstruction {
        match (a, b) {
            (4, 0) => {
                test_log!(self, "LDI");
                BlockInstruction::LDI
            }
            (4, 1) => {
                test_log!(self, "CPI");
                BlockInstruction::CPI
            }
            (4, 2) => {
                test_log!(self, "INI");
                BlockInstruction::INI
            }
            (4, 3) => {
                test_log!(self, "OUTI");
                BlockInstruction::OUTI
            }
            (5, 0) => {
                test_log!(self, "LDD");
                BlockInstruction::LDD
            }
            (5, 1) => {
                test_log!(self, "CPD");
                BlockInstruction::CPD
            }
            (5, 2) => {
                test_log!(self, "IND");
                BlockInstruction::IND
            }
            (5, 3) => {
                test_log!(self, "OUTD");
                BlockInstruction::OUTD
            }
            (6, 0) => {
                test_log!(self, "LDIR");
                BlockInstruction::LDIR
            }
            (6, 1) => {
                test_log!(self, "CPIR");
                BlockInstruction::CPIR
            }
            (6, 2) => {
                test_log!(self, "INIR");
                BlockInstruction::INIR
            }
            (6, 3) => {
                test_log!(self, "OTIR");
                BlockInstruction::OTIR
            }
            (7, 0) => {
                test_log!(self, "LDDR");
                BlockInstruction::LDDR
            }
            (7, 1) => {
                test_log!(self, "CPDR");
                BlockInstruction::CPDR
            }
            (7, 2) => {
                test_log!(self, "INDR");
                BlockInstruction::INDR
            }
            (7, 3) => {
                test_log!(self, "OTDR");
                BlockInstruction::OTDR
            }
            _ => unreachable!("Invalid a,b values"), // should never happen
        }
    }

    fn transform_register(
        &mut self,
        reg: AddressingMode,
        addressing: PrefixAddressing,
    ) -> AddressingMode {
        match addressing {
            PrefixAddressing::HL => reg,
            PrefixAddressing::IX => match reg {
                AddressingMode::Register(GPR::H) => {
                    test_log!(self, "IXH");
                    AddressingMode::IndexRegisterPart(IndexRegisterPart::IXH)
                }
                AddressingMode::Register(GPR::L) => {
                    test_log!(self, "IXL");
                    AddressingMode::IndexRegisterPart(IndexRegisterPart::IXL)
                }
                AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                    test_log!(self, "(IX + d)");
                    AddressingMode::Indexed(IndexRegister::IX, self.fetch_displacement())
                }
                AddressingMode::RegisterPair(RegisterPair::HL) => {
                    test_log!(self, "IX");
                    AddressingMode::IndexRegister(IndexRegister::IX)
                }
                _ => reg,
            },
            PrefixAddressing::IY => match reg {
                AddressingMode::Register(GPR::H) => {
                    test_log!(self, "IYH");
                    AddressingMode::IndexRegisterPart(IndexRegisterPart::IYH)
                }
                AddressingMode::Register(GPR::L) => {
                    test_log!(self, "IYL");
                    AddressingMode::IndexRegisterPart(IndexRegisterPart::IYL)
                }
                AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                    test_log!(self, "(IY + d)");
                    AddressingMode::Indexed(IndexRegister::IY, self.fetch_displacement())
                }
                AddressingMode::RegisterPair(RegisterPair::HL) => {
                    test_log!(self, "IY");
                    AddressingMode::IndexRegister(IndexRegister::IY)
                }
                _ => reg,
            },
        }
    }

    fn evaluate_condition(&mut self, condition: Condition) -> bool {
        match condition {
            Condition::NZ => !self.get_flag(Flag::Z),
            Condition::Z => self.get_flag(Flag::Z),
            Condition::NC => !self.get_flag(Flag::C),
            Condition::C => self.get_flag(Flag::C),
            Condition::PO => !self.get_flag(Flag::PV),
            Condition::PE => self.get_flag(Flag::PV),
            Condition::P => !self.get_flag(Flag::S),
            Condition::M => self.get_flag(Flag::S),
        }
    }

    fn decode_unprefixed(&mut self, opcode: u8, addressing: PrefixAddressing) -> () {
        //test_log!(self, "decode_unprefixed");
        match addressing {
            PrefixAddressing::HL => test_log!(self, "decode_unprefixed"),
            PrefixAddressing::IX => test_log!(self, "decode_dd"),
            PrefixAddressing::IY => test_log!(self, "decode_fd"),
        }

        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => match z {
                0 => match y {
                    // Relative jumps and assorted ops
                    0 => test_log!(self, "NOP"), // NOP

                    1 => {
                        test_log!(self, "EX AF, AF'");
                        self.ex_af_af_prime();
                    }
                    2 => {
                        // DJNZ d
                        test_log!(self, "DJNZ d");

                        let d = self.fetch_displacement();
                        let b = self.get_register(GPR::B).wrapping_sub(1);
                        self.set_register(GPR::B, b);
                        if b != 0 {
                            self.PC = self.PC.wrapping_add(d as i16 as u16);
                        }
                    }
                    3 => {
                        // JR d
                        test_log!(self, "JR d");
                        let d = self.fetch_displacement();
                        self.PC = self.PC.wrapping_add(d as i16 as u16);
                    }
                    4..=7 => {
                        // JR cc[y-4], d
                        test_log!(self, "JR cc[y-4], d");
                        let condition = self.table_cc(y - 4);
                        let d = self.fetch_displacement();
                        if self.evaluate_condition(condition) {
                            self.PC = self.PC.wrapping_add(d as i16 as u16);
                        }
                    }
                    _ => unreachable!("Invalid y value"), // should never happen
                },

                1 => {
                    if q {
                        test_log!(self, "ADD HL/IX/IY, rp[p]");
                        let dest = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );
                        let rp = self.table_rp(p);
                        let src = self.transform_register(rp, addressing);
                        self.add_16_op(dest, src, false);
                    } else {
                        // 16-bit load immediate/add
                        // LD rp[p], nn
                        test_log!(self, "LD rp[p], nn");
                        let nn = self.fetch_word();
                        let rp = self.table_rp(p);
                        let src = self.transform_register(rp, addressing);
                        self.ld_16(src, AddressingMode::ImmediateExtended(nn));
                    }
                }

                2 => match (q, p) {
                    // Indirect loading
                    (false, 0) => {
                        test_log!(self, "LD (BC), A");
                        self.ld(
                            AddressingMode::RegisterIndirect(RegisterPair::BC),
                            AddressingMode::Register(GPR::A),
                        )
                    } // LD (BC), A
                    (false, 1) => {
                        test_log!(self, "LD (DE), A");
                        self.ld(
                            AddressingMode::RegisterIndirect(RegisterPair::DE),
                            AddressingMode::Register(GPR::A),
                        )
                    } // LD (DE), A
                    (false, 2) => {
                        test_log!(self, "LD (nn), HL/IX/IY");
                        let addr = self.fetch_word();
                        let transformed = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );
                        self.ld_16(AddressingMode::Absolute(addr), transformed)
                    } // LD (nn), HL or LD (nn), IX/IY
                    (false, 3) => {
                        test_log!(self, "LD (nn), A");
                        let addr = self.fetch_word();
                        self.ld(
                            AddressingMode::Absolute(addr),
                            AddressingMode::Register(GPR::A),
                        )
                    } // LD (nn), A
                    (true, 0) => {
                        test_log!(self, "LD A, (BC)");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::RegisterIndirect(RegisterPair::BC),
                        )
                    } // LD A, (BC)
                    (true, 1) => {
                        test_log!(self, "LD A, (DE)");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::RegisterIndirect(RegisterPair::DE),
                        )
                    } // LD A, (DE)
                    (true, 2) => {
                        test_log!(self, "LD HL/IX/IY, (nn)");
                        let addr = self.fetch_word();
                        let transformed = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );
                        self.ld_16(transformed, AddressingMode::Absolute(addr))
                    } // LD HL, (nn) or LD IX/IY, (nn)
                    (true, 3) => {
                        test_log!(self, "LD A, (nn)");
                        let addr = self.fetch_word();
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::Absolute(addr),
                        )
                    } // LD A, (nn)
                    _ => unreachable!("Invalid q, p values"), // should never happen
                },

                3 => {
                    if !q {
                        // 16-bit INC/DEC
                        test_log!(self, "INC rp[p]");
                        let rp = self.table_rp(p);
                        let dest = self.transform_register(rp, addressing);
                        self.inc_16_op(dest);
                    } else {
                        test_log!(self, "DEC rp[p]");
                        let rp = self.table_rp(p);
                        let dest = self.transform_register(rp, addressing);
                        self.dec_16_op(dest);
                    }
                }

                4 => {
                    test_log!(self, "INC r[y]");
                    let reg = self.table_r(y);
                    let dest = self.transform_register(reg, addressing);
                    self.inc_op(dest);
                }
                5 => {
                    test_log!(self, "DEC r[y]");
                    let reg = self.table_r(y);
                    let dest = self.transform_register(reg, addressing);
                    self.dec_op(dest);
                }
                6 => {
                    test_log!(self, "LD r[y], n");
                    let n = self.fetch();
                    let reg = self.table_r(y);
                    let dest = self.transform_register(reg, addressing);
                    self.ld(dest, AddressingMode::Immediate(n))
                } //  LD r[y], n (still have to transform r[y] if IX/IY prefixed)
                7 => {
                    // Assorted operations on accumulator/flags
                    match y {
                        0 => {
                            // RLCA
                            test_log!(self, "RLCA");
                            let a = self.get_register(GPR::A);
                            let (res, f) = rot::rlc(a);
                            self.set_register(GPR::A, res);

                            self.set_flag((f & flags::CARRY) != 0, Flag::C);
                            self.set_flag(false, Flag::N);
                            self.set_flag(false, Flag::H);
                        }
                        1 => {
                            // RRCA
                            test_log!(self, "RRCA");
                            let a = self.get_register(GPR::A);
                            let (res, f) = rot::rrc(a);
                            self.set_register(GPR::A, res);

                            self.set_flag((f & flags::CARRY) != 0, Flag::C);
                            self.set_flag(false, Flag::N);
                            self.set_flag(false, Flag::H);
                        }
                        2 => {
                            // RLA
                            test_log!(self, "RLA");
                            let a = self.get_register(GPR::A);
                            let carry = self.get_flag(Flag::C);
                            let (res, f) = rot::rl(a, carry);
                            self.set_register(GPR::A, res);

                            self.set_flag((f & flags::CARRY) != 0, Flag::C);
                            self.set_flag(false, Flag::N);
                            self.set_flag(false, Flag::H);
                        }
                        3 => {
                            // RRA
                            test_log!(self, "RRA");
                            let a = self.get_register(GPR::A);
                            let carry = self.get_flag(Flag::C);
                            let (res, f) = rot::rr(a, carry);
                            self.set_register(GPR::A, res);

                            self.set_flag((f & flags::CARRY) != 0, Flag::C);
                            self.set_flag(false, Flag::N);
                            self.set_flag(false, Flag::H);
                        }
                        4 => {
                            // DAA
                            test_log!(self, "DAA");
                            self.daa();
                        }
                        5 => {
                            // CPL
                            test_log!(self, "CPL");

                            let a = self.get_register(GPR::A);
                            let result = a ^ 0xFF;
                            self.set_register(GPR::A, result);
                            self.set_flag(true, Flag::N);
                            self.set_flag(true, Flag::H);
                            // undocummented x and y flags
                            // set bit 3 and 5 according to result
                            self.set_flag((result & flags::X) != 0, Flag::X);
                            self.set_flag((result & flags::Y) != 0, Flag::Y);
                        }
                        6 => {
                            //  SCF
                            test_log!(self, "SCF");

                            self.set_flag(true, Flag::C);
                            self.set_flag(false, Flag::N);
                            self.set_flag(false, Flag::H);
                            // undocummented x and y flags
                            let a = self.get_register(GPR::A);
                            self.set_flag((a & flags::X) != 0, Flag::X);
                            self.set_flag((a & flags::Y) != 0, Flag::Y);
                        }
                        7 => {
                            // CCF
                            test_log!(self, "CCF");

                            let current_carry = self.get_flag(Flag::C);
                            self.set_flag(!current_carry, Flag::C);
                            self.set_flag(false, Flag::N);
                            // NOTE: H flag is set to previous C flag
                            self.set_flag(current_carry, Flag::H);

                            // undocummented x and y flags
                            let a = self.get_register(GPR::A);
                            self.set_flag((a & flags::X) != 0, Flag::X);
                            self.set_flag((a & flags::Y) != 0, Flag::Y);
                        }
                        _ => unreachable!("Invalid y value"), // should never happen
                    }
                }
                _ => unreachable!("Invalid z value"), // should never happen
            },
            1 => {
                if (z == 6) && (y == 6) {
                    test_log!(self, "HALT");
                    self.halted = true;
                } else {
                    test_log!(self, "LD r[y], r[z]");
                    let reg = self.table_r(y);
                    let dest = self.transform_register(reg, addressing);
                    let reg = self.table_r(z);
                    let src = self.transform_register(reg, addressing);
                    self.ld(dest, src); // LD r[y], r[z] (still have to transform r[y] and r[z] if IX/IY prefixed)
                }
            }

            2 => {
                // ALU[y] r[z]
                test_log!(self, "ALU[y] r[z]"); // TODO: exhaustive tests for these
                let alu_op = self.table_alu(y);
                let reg = self.table_r(z);
                let src = self.transform_register(reg, addressing);
                let value = self.read_8(src);
                self.alu_op(alu_op, value);
            }
            3 => match z {
                0 => {
                    // RET cc[y]
                    test_log!(self, "RET cc[y]");
                    let condition = self.table_cc(y);
                    if self.evaluate_condition(condition) {
                        let ret_addr = self.pop();
                        self.PC = ret_addr;
                    }
                }
                1 => match (q, p) {
                    // POP & various ops
                    (false, _) => {
                        // POP rp2[p]
                        test_log!(self, "POP rp2[p]");
                        let rp = self.table_rp2(p);
                        let dest = self.transform_register(rp, addressing);
                        let value = self.pop();
                        self.ld_16(dest, AddressingMode::ImmediateExtended(value));
                    }
                    (true, 0) => {
                        // RET
                        test_log!(self, "RET");

                        let ret_addr = self.pop();
                        self.PC = ret_addr;
                    }
                    (true, 1) => {
                        test_log!(self, "EXX");
                        self.exx();
                    }
                    (true, 2) => {
                        // JP HL/IX/IY
                        test_log!(self, "JP HL");
                        let src = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );
                        let addr = self.read_16(src);
                        self.PC = addr;
                    }
                    (true, 3) => {
                        // LD SP, HL (or LD SP, IX/IY if prefixed)
                        test_log!(self, "LD SP, HL");
                        let src = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );
                        self.ld_16(AddressingMode::RegisterPair(RegisterPair::SP), src);
                    }
                    _ => unreachable!("Invalid q, p values"), // should never happen
                },
                2 => {
                    // JP cc[y], nn
                    test_log!(self, "JP cc[y], nn");
                    let condition = self.table_cc(y);
                    let addr = self.fetch_word();
                    if self.evaluate_condition(condition) {
                        self.PC = addr;
                    }
                }
                3 => match y {
                    // Assorted operations
                    0 => {
                        // JP nn
                        test_log!(self, "JP nn");
                        let addr = self.fetch_word();
                        self.PC = addr;
                    }
                    1 => unreachable!("CB prefix, should be handled separately"), // should never reach this
                    2 => test_log!(self, "OUT (n), A"), // TODO: OUT (n), A
                    3 => test_log!(self, "IN A, (n)"),  // TODO: IN A, (n)
                    4 => {
                        // EX (SP), HL (or EX (SP), IX/IY if prefixed)
                        test_log!(self, "EX (SP), HL/IX/IY");
                        let temp_l = self.memory.borrow().read(self.SP);
                        let temp_h = self.memory.borrow().read(self.SP.wrapping_add(1));
                        let register_pair = self.transform_register(
                            AddressingMode::RegisterPair(RegisterPair::HL),
                            addressing,
                        );

                        let rp = self.read_16(register_pair);
                        self.memory.borrow_mut().write(self.SP, (rp & 0xFF) as u8);
                        self.memory
                            .borrow_mut()
                            .write(self.SP.wrapping_add(1), (rp >> 8) as u8);

                        self.write_16(register_pair, ((temp_h as u16) << 8) | (temp_l as u16));
                    }
                    5 => {
                        test_log!(self, "EX DE, HL");
                        let de = self.get_register_pair(RegisterPair::DE);
                        let hl = self.get_register_pair(RegisterPair::HL);
                        self.set_register_pair(RegisterPair::DE, hl);
                        self.set_register_pair(RegisterPair::HL, de);
                    }
                    // TODO: will do interrupts later
                    6 => test_log!(self, "DI"),           // TODO: DI
                    7 => test_log!(self, "EI"),           // TODO: EI
                    _ => unreachable!("Invalid y value"), // should never happen
                },
                4 => {
                    // CALL cc[y], nn
                    test_log!(self, "CALL cc[y], nn");
                    let condition = self.table_cc(y);
                    let addr = self.fetch_word();
                    if self.evaluate_condition(condition) {
                        self.push(self.PC);
                        self.PC = addr;
                    }
                }
                5 => match (q, p) {
                    // PUSH & various ops
                    (false, _) => {
                        // PUSH rp2[p]
                        test_log!(self, "PUSH rp2[p]");
                        let rp = self.table_rp2(p);
                        let src = self.transform_register(rp, addressing);
                        let value = self.read_16(src);
                        self.push(value);
                    }
                    (true, 0) => {
                        // CALL nn
                        test_log!(self, "CALL nn");
                        let addr = self.fetch_word();
                        self.push(self.PC);
                        self.PC = addr;
                    }
                    (true, 1) => unreachable!("Shouldn't reach ED prefix here"), // should never reach this
                    (true, 2) => unreachable!("Shouldn't reach DD prefix here"), // should never reach this
                    (true, 3) => unreachable!("Shouldn't reach FD prefix here"), // should never reach this
                    _ => unreachable!("Invalid q, p values"), // should never happen
                },
                6 => {
                    // ALU[y] n
                    test_log!(self, "ALU[y] n");
                    let alu_op = self.table_alu(y);
                    let n = self.fetch();
                    self.alu_op(alu_op, n);
                }
                7 => {
                    // RST y*8
                    test_log!(self, "RST y*8");
                    let rst_addr = (y as u16) * 8;
                    test_log!(self, "{:02X}h", rst_addr);
                    self.push(self.PC);
                    self.PC = rst_addr;
                }
                _ => unreachable!("Invalid z value"), // should never happen
            },
            _ => unreachable!("Invalid x value"), // should never happen
        }
    }

    fn decode_cb(&mut self, opcode: u8) -> () {
        test_log!(self, "decode_cb");
        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => {
                test_log!(self, "rot[y] r[z]");
                self.rot(y, z) // NOTE: rot[y] r[z]
            }
            1 => {
                test_log!(self, "BIT y, r[z]");
                test_log!(self, "{}", y);
                let reg = self.table_r(z);
                self.bit(y, reg)
            } // NOTE: BIT y, r[z]
            2 => {
                test_log!(self, "RES y, r[z]");
                test_log!(self, "{}", y);
                let reg = self.table_r(z);
                self.res(y, reg)
            } // NOTE: RES y, r[z]
            3 => {
                test_log!(self, "SET y, r[z]");
                test_log!(self, "{}", y);
                let reg = self.table_r(z);
                self.set(y, reg)
            } // NOTE: SET y, r[z]
            _ => unreachable!("Invalid x value"), // should never happen
        }
    }

    fn decode_ed(&mut self, opcode: u8) -> () {
        test_log!(self, "decode_ed");
        let (x, y, z, p, q) = decode_opcode(opcode);

        /*
        Lots of NONI's here. Some of these are instructions for the Z180, will not be implemented.
        (at least for now)
         */

        match x {
            0 | 3 => test_log!(self, "NONI"), // NOTE: NONI
            1 => match z {
                0 => {
                    if y == 6 {
                        test_log!(self, "IN (C)"); // TODO: IN (C)
                    } else if y < 8 {
                        test_log!(self, "IN r[y], (C)"); // TODO: IN r[y], (C)
                    } else {
                        unreachable!("Invalid y value") // should never happen
                    }
                }
                1 => {
                    if y == 6 {
                        test_log!(self, "OUT (C), 0"); // TODO: OUT(C), 0
                    } else if y < 8 {
                        test_log!(self, "OUT (C), r[y]"); // TODO: OUT (C), r[y]
                    } else {
                        unreachable!("Invalid y value") // should never happen
                    }
                }
                2 => {
                    if !q {
                        // SBC HL, rp[p]
                        test_log!(self, "SBC HL, rp[p]");
                        let dest = AddressingMode::RegisterPair(RegisterPair::HL);
                        let rp = self.table_rp(p);
                        self.sub_16_op(dest, rp, true) // SBC HL, rp[p]
                    } else {
                        // ADC HL, rp[p]
                        test_log!(self, "ADC HL, rp[p]");
                        let dest = AddressingMode::RegisterPair(RegisterPair::HL);
                        let rp = self.table_rp(p);
                        self.add_16_op(dest, rp, true) // ADC HL, rp[p]
                    }
                }
                3 => {
                    if !q {
                        test_log!(self, "LD (nn), rp[p]");
                        let addr = self.fetch_word();
                        let src = self.table_rp(p);
                        self.ld_16(AddressingMode::Absolute(addr), src) // LD (nn), rp[p]
                    } else {
                        test_log!(self, "LD rp[p], (nn)");
                        let addr = self.fetch_word();
                        let dest = self.table_rp(p);
                        self.ld_16(dest, AddressingMode::Absolute(addr)) // LD rp[p], (nn)
                    }
                }
                /*
                NOTE: in the original table it says that z = 4 => NEG, but its only true for 0x44
                (y = 0), otherwise its NONI
                 */
                4 => {
                    if y == 0 {
                        // NEG
                        test_log!(self, "NEG");
                        let a = self.get_register(GPR::A);
                        let (result, flags) = alu::alu_op::sub(0, a, false);
                        self.set_register(GPR::A, result);
                        self.af_registers[self.active_af].f = flags | flags::ADD_SUB;
                    } else {
                        test_log!(self, "NONI"); // NOTE: NONI
                    }
                }
                5 => {
                    if y == 0 {
                        // theres supposed to be just retn, but manual says noni
                        test_log!(self, "RETN"); // TODO: RETN
                    } else if y == 1 {
                        test_log!(self, "RETI"); // TODO: RETI
                    } else if y < 8 {
                        test_log!(self, "NONI"); // NOTE: NONI
                    } else {
                        unreachable!("Invalid y value") // should never happen
                    }
                }
                /*
                NOTE: in the manual there is only im 0, im 1 and im 2, the rest are NONI's,
                but the og page says otherwise, i'll go against the og page for now
                 */
                6 => {
                    match y {
                        0 => {
                            test_log!(self, "IM 0"); // TODO: IM 0
                        }
                        2 => {
                            test_log!(self, "IM 1"); // TODO: IM 1
                        }
                        3 => {
                            test_log!(self, "IM 2"); // TODO: IM 2
                        }
                        _ => {
                            test_log!(self, "NONI"); // NOTE: NONI
                        }
                    }
                }
                7 => match y {
                    0 => {
                        test_log!(self, "LD I, A");
                        self.ld(
                            AddressingMode::System(SystemRegister::I),
                            AddressingMode::Register(GPR::A),
                        )
                    } //  LD I, A
                    1 => {
                        test_log!(self, "LD R, A");
                        self.ld(
                            AddressingMode::System(SystemRegister::R),
                            AddressingMode::Register(GPR::A),
                        )
                    } //  LD R, A
                    2 => {
                        test_log!(self, "LD A, I");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::System(SystemRegister::I),
                        )
                    } //  LD A, I
                    3 => {
                        test_log!(self, "LD A, R");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::System(SystemRegister::R),
                        )
                    } //  LD A, R
                    4 => {
                        // RRD
                        test_log!(self, "RRD");
                        let a = self.get_register(GPR::A);
                        let ah = a & 0xF0;
                        let al = a & 0x0F;
                        let hl_addr = self.get_register_pair(RegisterPair::HL);
                        let value = self.memory.borrow().read(hl_addr);
                        let mh = value & 0xF0;
                        let ml = value & 0x0F;

                        let new_a = ah | ml;
                        let new_hl = (al << 4) | (mh >> 4);

                        let s = (new_a & flags::SIGN) != 0;
                        let z = new_a == 0;
                        let p = new_a.count_ones() % 2 == 0;
                        let x = (new_a & flags::X) != 0;
                        let y = (new_a & flags::Y) != 0;

                        self.set_register(GPR::A, new_a);
                        self.memory.borrow_mut().write(hl_addr, new_hl);

                        self.set_flag(s, Flag::S);
                        self.set_flag(z, Flag::Z);
                        self.set_flag(p, Flag::PV);
                        self.set_flag(false, Flag::H);
                        self.set_flag(false, Flag::N);
                        self.set_flag(x, Flag::X);
                        self.set_flag(y, Flag::Y);
                    }
                    5 => {
                        test_log!(self, "RLD");
                        let a = self.get_register(GPR::A);
                        let ah = a & 0xF0;
                        let al = a & 0x0F;
                        let hl_addr = self.get_register_pair(RegisterPair::HL);
                        let value = self.memory.borrow().read(hl_addr);
                        let mh = value & 0xF0;
                        let ml = value & 0x0F;

                        let new_a = ah | (mh >> 4);
                        let new_hl = (ml << 4) | al;

                        let s = (new_a & flags::SIGN) != 0;
                        let z = new_a == 0;
                        let p = new_a.count_ones() % 2 == 0;
                        let x = (new_a & flags::X) != 0;
                        let y = (new_a & flags::Y) != 0;

                        self.set_register(GPR::A, new_a);
                        self.memory.borrow_mut().write(hl_addr, new_hl);

                        self.set_flag(s, Flag::S);
                        self.set_flag(z, Flag::Z);
                        self.set_flag(p, Flag::PV);
                        self.set_flag(false, Flag::H);
                        self.set_flag(false, Flag::N);
                        self.set_flag(x, Flag::X);
                        self.set_flag(y, Flag::Y);
                    }
                    6 => test_log!(self, "NONI"),         // NOTE: NONI
                    7 => test_log!(self, "NONI"),         // NOTE: NONI
                    _ => unreachable!("Invalid y value"), // should never happen
                },
                _ => unreachable!("Invalid z value"), // should never happen
            },
            2 => {
                if z <= 3 && y >= 4 {
                    test_log!(self, "bli[y, z]") // TODO: bli[y,z]
                } else {
                    test_log!(self, "NONI"); // NOTE: NONI
                }
            }
            _ => unreachable!("Invalid x value"), // should never happen
        }
    }

    fn decode_dd(&mut self, opcode: u8) {
        //test_log!(self, "decode_dd");
        match opcode {
            0xDD | 0xED | 0xFD => {
                // Current DD is ignored (NONI), fetch next byte and process it
                test_log!(self, "DD NONI");
                let next_opcode = self.fetch();
                self.decode(next_opcode)
            }
            0xCB => {
                test_log!(self, "CB prefix");
                let displacement = self.fetch();
                let op = self.fetch();
                self.decode_ddcb_fdcb(op, IndexRegister::IX, displacement)
            }
            _ => self.decode_unprefixed(opcode, PrefixAddressing::IX),
        }
    }

    fn decode_fd(&mut self, opcode: u8) {
        //test_log!(self, "decode_fd");
        match opcode {
            0xDD | 0xED | 0xFD => {
                // Current FD is ignored (NONI), fetch next byte and process it
                test_log!(self, "FD NONI");
                let next_opcode = self.fetch();
                self.decode(next_opcode)
            }
            0xCB => {
                test_log!(self, "CB prefix");
                let displacement = self.fetch();
                let op = self.fetch();
                self.decode_ddcb_fdcb(op, IndexRegister::IY, displacement)
            }
            _ => self.decode_unprefixed(opcode, PrefixAddressing::IY),
        }
    }

    fn decode_ddcb_fdcb(&mut self, opcode: u8, indexing: IndexRegister, displacement: u8) {
        match indexing {
            IndexRegister::IX => test_log!(self, "decode_ddcb"),
            IndexRegister::IY => test_log!(self, "decode_fdcb"),
        }
        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => {
                if z == 6 {
                    test_log!(self, "rot[y] (IX+d)"); // TODO: rot[y] (IX+d)
                } else if z < 8 {
                    test_log!(self, "LD r[z], rot[y] (IX+d)"); // TODO: LD r[z], rot[y] (IX+d)
                } else {
                    unreachable!("Invalid z value") // should never happen
                }
            }
            1 => test_log!(self, "BIT y, (IX+d)"), // TODO: BIT y, (IX+d)
            2 => {
                if z == 6 {
                    test_log!(self, "RES y, (IX+d)"); // TODO: RES y, (IX+d)
                } else if z < 8 {
                    test_log!(self, "LD r[z], RES y, (IX+d)"); // TODO: LD r[z], RES y, (IX+d)
                } else {
                    unreachable!("Invalid z value") // should never happen
                }
            }
            3 => {
                if z == 6 {
                    test_log!(self, "SET y, (IX+d)"); // TODO: SET y, (IX+d)
                } else if z < 8 {
                    test_log!(self, "LD r[z], SET y, (IX+d)"); // TODO: LD r[z], SET y, (IX+d)
                } else {
                    unreachable!("Invalid z value") // should never happen
                }
            }
            _ => unreachable!("Invalid x value"), // should never happen
        }
    }

    fn decode(&mut self, opcode: u8) -> () {
        match opcode {
            0xCB => {
                let op = self.fetch();
                self.decode_cb(op)
            }
            0xED => {
                let op = self.fetch();
                self.decode_ed(op)
            }

            0xDD => {
                let op = self.fetch();
                self.decode_dd(op)
            }
            0xFD => {
                let op = self.fetch();
                self.decode_fd(op)
            }

            _ => self.decode_unprefixed(opcode, PrefixAddressing::HL),
        }
    }

    fn execute_instruction(&mut self, opcode: u8) {
        self.decode(opcode);
    }

    // register ops

    fn ld(&mut self, dest: AddressingMode, src: AddressingMode) {
        let value = self.read_8(src);
        self.write_8(dest, value);
    }

    fn ld_16(&mut self, dest: AddressingMode, src: AddressingMode) {
        let value = self.read_16(src);
        self.write_16(dest, value);
    }

    fn table_rot(&mut self, y: u8) -> RotOperation {
        match y {
            0 => {
                test_log!(self, "RLC");
                RotOperation::RLC
            }
            1 => {
                test_log!(self, "RRC");
                RotOperation::RRC
            }
            2 => {
                test_log!(self, "RL");
                RotOperation::RL
            }
            3 => {
                test_log!(self, "RR");
                RotOperation::RR
            }
            4 => {
                test_log!(self, "SLA");
                RotOperation::SLA
            }
            5 => {
                test_log!(self, "SRA");
                RotOperation::SRA
            }
            6 => {
                test_log!(self, "SLL");
                RotOperation::SLL
            }
            7 => {
                test_log!(self, "SRL");
                RotOperation::SRL
            }
            _ => unreachable!("Invalid y value"), // should never happen
        }
    }

    fn rot(&mut self, y: u8, z: u8) {
        let operation = self.table_rot(y);
        let reg = self.table_r(z);
        let value = self.read_8(reg);

        let (result, flags) = match operation {
            RotOperation::RLC => rot::rlc(value),
            RotOperation::RRC => rot::rrc(value),
            RotOperation::RL => rot::rl(value, self.get_flag(Flag::C)),
            RotOperation::RR => rot::rr(value, self.get_flag(Flag::C)),
            RotOperation::SLA => rot::sla(value),
            RotOperation::SRA => rot::sra(value),
            RotOperation::SLL => rot::sll(value),
            RotOperation::SRL => rot::srl(value),
        };
        self.set_register(GPR::F, flags);
        self.write_8(reg, result);
    }

    fn bit(&mut self, y: u8, reg: AddressingMode) {
        let value = self.read_8(reg);

        let prev_c = self.get_flag(Flag::C);
        self.set_register(GPR::F, bit(value, y));

        self.set_flag(prev_c, Flag::C); // C flag is not affected
    }

    fn res(&mut self, y: u8, reg: AddressingMode) {
        let value = self.read_8(reg);
        let result = res(value, y);
        self.write_8(reg, result);
    }

    fn set(&mut self, y: u8, reg: AddressingMode) {
        let value = self.read_8(reg);
        let result = set(value, y);
        self.write_8(reg, result);
    }

    fn daa(&mut self) {
        let mut t: u8 = 0;
        let mut a = self.get_register(GPR::A);

        if self.get_flag(Flag::H) || ((a & 0xf) > 9) {
            t = t.wrapping_add(1);
        }

        if self.get_flag(Flag::C) || (a > 0x99) {
            t = t.wrapping_add(2);
            self.set_flag(true, Flag::C);
        }

        if self.get_flag(Flag::N) && !self.get_flag(Flag::H) {
            self.set_flag(false, Flag::H);
        } else {
            if self.get_flag(Flag::N) && self.get_flag(Flag::H) {
                self.set_flag((a & 0x0f) < 6, Flag::H);
            } else {
                self.set_flag((a & 0x0f) >= 0x0a, Flag::H);
            }
        }

        if t == 1 {
            a = a.wrapping_add(if self.get_flag(Flag::N) { 0xFA } else { 0x06 })
        } else if t == 2 {
            a = a.wrapping_add(if self.get_flag(Flag::N) { 0xA0 } else { 0x60 })
        } else if t == 3 {
            a = a.wrapping_add(if self.get_flag(Flag::N) { 0x9A } else { 0x66 })
        }

        self.set_register(GPR::A, a);

        self.set_flag((a & flags::SIGN) == flags::SIGN, Flag::S);
        self.set_flag(a == 0, Flag::Z);
        self.set_flag(a.count_ones() % 2 == 0, Flag::PV);
        self.set_flag((a & flags::X) == flags::X, Flag::X);
        self.set_flag((a & flags::Y) == flags::Y, Flag::Y);
    }

    fn jmp(&mut self, addr: u16) {
        self.PC = addr;
    }
}

impl SyncronousComponent for Z80A {
    fn tick(&mut self) {
        if self.halted {
            return;
        }
        /*
        self.cycles -= 1;
            if self.cycles == 0 {
                let opcode = self.fetch();
            // decode and execute
            self.execute_instruction(opcode);
        }
        */
        let opcode = self.fetch();
        self.execute_instruction(opcode);
    }
}

#[cfg(test)]
mod tests;
