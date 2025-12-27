mod alu;

use std::cell::RefCell;
#[cfg(test)]
use std::collections::VecDeque;
use std::rc::Rc;
use std::usize;

use crate::{
    cpu::alu::{
        add_16, alu_op, bit, dec, inc, res,
        rot::{self, RotOperation},
        set,
    },
    traits::{MemoryMapper, SyncronousComponent},
};

#[cfg(test)]
macro_rules! test_log {
    ($self:expr, $msg:expr) => {
        $self.test_callback.1($msg, &mut $self.test_callback.0)
    };
}

#[cfg(not(test))]
macro_rules! test_log {
    ($self:expr, $msg:expr) => {
        ()
    };
}

#[derive(PartialEq, Clone, Copy)]
enum GPR {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(PartialEq, Clone, Copy)]
enum RegisterPair {
    BC,
    DE,
    HL,
    AF,
    SP,
}

#[derive(PartialEq, Clone, Copy)]
enum RegSet {
    Main,
    Alt,
}

#[derive(PartialEq, Clone, Copy)]
enum IndexRegister {
    IX,
    IY,
}

#[derive(PartialEq, Clone, Copy)]
enum SpecialRegister {
    PC,
    SP,
    IX,
    IY,
    I,
    R,
    A,
    IXH,
    IXL,
    IYH,
    IYL,
}

#[derive(PartialEq, Clone, Copy)]
enum AddressingMode {
    Immediate(u8),
    ImmediateExtended(u16),
    Absolute(u16),
    ZeroPage,
    Relative(u8),
    Extended,
    Indexed(IndexRegister, i8),
    Register(GPR),
    Special(SpecialRegister),
    Implied,
    RegisterIndirect(RegisterPair),
    RegisterPair(RegisterPair),
}

#[derive(PartialEq, Clone, Copy)]
enum Flag {
    C,
    N,
    PV,
    Y,
    H,
    X,
    Z,
    S,
}

#[derive(Clone, Copy)]
enum PrefixAddressing {
    HL, // HL, H, L
    IX, // IX, IXH, IXL
    IY, // IY, IYH, IYL
}

enum ALUOperation {
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    OR,
    XOR,
    CP,
}

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

struct RegisterSet {
    // z80 has two register sets, so doing this for easier access
    pub A: u8,
    pub F: u8,
    pub B: u8,
    pub C: u8,
    pub D: u8,
    pub E: u8,
    pub H: u8,
    pub L: u8,
}

impl RegisterSet {
    pub fn get_register(&self, reg: GPR) -> u8 {
        match reg {
            GPR::A => self.A,
            GPR::F => self.F,
            GPR::B => self.B,
            GPR::C => self.C,
            GPR::D => self.D,
            GPR::E => self.E,
            GPR::H => self.H,
            GPR::L => self.L,
        }
    }

    pub fn set_register(&mut self, reg: GPR, value: u8) {
        match reg {
            GPR::A => self.A = value,
            GPR::F => self.F = value,
            GPR::B => self.B = value,
            GPR::C => self.C = value,
            GPR::D => self.D = value,
            GPR::E => self.E = value,
            GPR::H => self.H = value,
            GPR::L => self.L = value,
        }
    }

    pub fn get_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::AF => ((self.A as u16) << 8) | (self.F as u16),
            RegisterPair::BC => ((self.B as u16) << 8) | (self.C as u16),
            RegisterPair::DE => ((self.D as u16) << 8) | (self.E as u16),
            RegisterPair::HL => ((self.H as u16) << 8) | (self.L as u16),
            RegisterPair::SP => panic!("SP is not part of RegisterSet"),
        }
    }

    pub fn set_pair(&mut self, pair: RegisterPair, value: u16) {
        match pair {
            RegisterPair::AF => {
                self.A = (value >> 8) as u8;
                self.F = (value & 0xFF) as u8;
            }
            RegisterPair::BC => {
                self.B = (value >> 8) as u8;
                self.C = (value & 0xFF) as u8;
            }
            RegisterPair::DE => {
                self.D = (value >> 8) as u8;
                self.E = (value & 0xFF) as u8;
            }
            RegisterPair::HL => {
                self.H = (value >> 8) as u8;
                self.L = (value & 0xFF) as u8;
            }
            RegisterPair::SP => panic!("SP is not part of RegisterSet"),
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        let flags = self.F;
        match flag {
            Flag::C => flags & 0b00000001 != 0,
            Flag::N => flags & 0b00000010 != 0,
            Flag::PV => flags & 0b00000100 != 0,
            Flag::Y => flags & 0b00001000 != 0,
            Flag::H => flags & 0b00010000 != 0,
            Flag::X => flags & 0b00100000 != 0,
            Flag::Z => flags & 0b01000000 != 0,
            Flag::S => flags & 0b10000000 != 0,
        }
    }

    pub fn set_flag(&mut self, value: bool, flag: Flag) {
        let flags = &mut self.F;
        if value {
            match flag {
                Flag::C => *flags |= 0b00000001,
                Flag::N => *flags |= 0b00000010,
                Flag::PV => *flags |= 0b00000100,
                Flag::Y => *flags |= 0b00001000,
                Flag::H => *flags |= 0b00010000,
                Flag::X => *flags |= 0b00100000,
                Flag::Z => *flags |= 0b01000000,
                Flag::S => *flags |= 0b10000000,
            }
        } else {
            match flag {
                Flag::C => *flags &= !0b00000001,
                Flag::N => *flags &= !0b00000010,
                Flag::PV => *flags &= !0b00000100,
                Flag::Y => *flags &= !0b00001000,
                Flag::H => *flags &= !0b00010000,
                Flag::X => *flags &= !0b00100000,
                Flag::Z => *flags &= !0b01000000,
                Flag::S => *flags &= !0b10000000,
            }
        }
    }

    pub fn get_flag_i(&self, usize: usize) -> bool {
        self.F & (1 << usize) != 0
    }
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
    main_set: RegisterSet,
    alt_set: RegisterSet,

    // special registers
    PC: u16,
    SP: u16,
    IX: u16,
    IY: u16,
    I: u8,
    R: u8,

    memory: Rc<RefCell<dyn MemoryMapper>>,

    cycles: u64,

    #[cfg(test)]
    test_callback: (
        VecDeque<String>,
        Box<dyn FnMut(&str, &mut VecDeque<String>)>,
    ),
}

impl Z80A {
    pub fn new(memory: Rc<RefCell<dyn MemoryMapper>>) -> Self {
        Z80A {
            main_set: RegisterSet {
                A: 0,
                F: 0,
                B: 0,
                C: 0,
                D: 0,
                E: 0,
                H: 0,
                L: 0,
            },
            alt_set: RegisterSet {
                A: 0,
                F: 0,
                B: 0,
                C: 0,
                D: 0,
                E: 0,
                H: 0,
                L: 0,
            },
            PC: 0,
            SP: 0,
            IX: 0,
            IY: 0,
            I: 0,
            R: 0,
            memory,
            cycles: 0,

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

    fn swap_registers(
        &mut self,
        origin: RegisterPair,
        origin_set: RegSet,
        dest: RegisterPair,
        dest_set: RegSet,
    ) {
        let origin_value = match origin_set {
            RegSet::Main => self.main_set.get_pair(origin),
            RegSet::Alt => self.alt_set.get_pair(origin),
        };

        match dest_set {
            RegSet::Main => self.main_set.set_pair(dest, origin_value),
            RegSet::Alt => self.alt_set.set_pair(dest, origin_value),
        }
    }

    fn set_register(&mut self, reg: GPR, value: u8) {
        self.main_set.set_register(reg, value);
    }

    fn get_register(&self, reg: GPR) -> u8 {
        self.main_set.get_register(reg)
    }

    fn get_register_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::SP => self.SP,
            _ => self.main_set.get_pair(pair),
        }
    }

    fn set_special_register(&mut self, reg: SpecialRegister, value: u16) {
        match reg {
            SpecialRegister::PC => self.PC = value,
            SpecialRegister::SP => self.SP = value,
            SpecialRegister::IX => self.IX = value,
            SpecialRegister::IY => self.IY = value,
            SpecialRegister::A => self.main_set.A = value as u8,
            SpecialRegister::I => self.I = value as u8,
            SpecialRegister::R => self.R = value as u8,
            SpecialRegister::IXH => self.IX = (self.IX & 0x00FF) | ((value as u16) << 8),
            SpecialRegister::IXL => self.IX = (self.IX & 0xFF00) | (value as u16),
            SpecialRegister::IYH => self.IY = (self.IY & 0x00FF) | ((value as u16) << 8),
            SpecialRegister::IYL => self.IY = (self.IY & 0xFF00) | (value as u16),
        }
    }

    fn get_special_register(&self, reg: SpecialRegister) -> u16 {
        match reg {
            SpecialRegister::PC => self.PC,
            SpecialRegister::SP => self.SP,
            SpecialRegister::IX => self.IX,
            SpecialRegister::IY => self.IY,
            SpecialRegister::A => self.main_set.A as u16,
            SpecialRegister::I => self.I as u16,
            SpecialRegister::R => self.R as u16,
            SpecialRegister::IXH => (self.IX >> 8) as u16,
            SpecialRegister::IXL => (self.IX & 0x00FF) as u16,
            SpecialRegister::IYH => (self.IY >> 8) as u16,
            SpecialRegister::IYL => (self.IY & 0x00FF) as u16,
        }
    }

    fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
        match pair {
            RegisterPair::SP => self.SP = value,
            _ => self.main_set.set_pair(pair, value),
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
                self.main_set.F = flags;
            }
            ALUOperation::ADC => {
                let carry = self.main_set.get_flag(Flag::C);
                let (result, flags) = alu_op::add(self.get_register(GPR::A), value, carry);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::SUB => {
                let (result, flags) = alu_op::sub(self.get_register(GPR::A), value, false);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::SBC => {
                let carry = self.main_set.get_flag(Flag::C);
                let (result, flags) = alu_op::sub(self.get_register(GPR::A), value, carry);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::AND => {
                let (result, flags) = alu_op::and(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::OR => {
                let (result, flags) = alu_op::or(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::XOR => {
                let (result, flags) = alu_op::xor(self.get_register(GPR::A), value);
                self.set_register(GPR::A, result);
                self.main_set.F = flags;
            }
            ALUOperation::CP => {
                let (_, flags) = alu_op::sub(self.get_register(GPR::A), value, false);
                // result is ignored for CP
                self.main_set.F = flags;
            }
        }
    }

    fn inc_op(&mut self, dest: AddressingMode) {
        let value = match dest {
            AddressingMode::Register(r) => self.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let addr = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add(disp as i16 as u16);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Special(r) => (self.get_special_register(r) & 0xFF) as u8,
            _ => panic!("Invalid addressing mode for INC"),
        };

        let (result, flags) = inc(value);

        // Preserve Carry flag
        let current_carry = self.main_set.get_flag(Flag::C) as u8;
        self.main_set.F = flags | current_carry;

        match dest {
            AddressingMode::Register(r) => self.set_register(r, result),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let addr = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow_mut().write(addr, result);
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add(disp as i16 as u16);
                self.memory.borrow_mut().write(addr, result);
            }
            AddressingMode::Special(r) => {
                self.set_special_register(r, result as u16);
            }
            _ => panic!("Invalid addressing mode for INC writeback"),
        }
    }

    fn dec_op(&mut self, dest: AddressingMode) {
        let value = match dest {
            AddressingMode::Register(r) => self.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let addr = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add(disp as i16 as u16);
                self.memory.borrow().read(addr)
            }
            AddressingMode::Special(r) => (self.get_special_register(r) & 0xFF) as u8,
            _ => panic!("Invalid addressing mode for DEC"),
        };

        let (result, flags) = dec(value);

        // Preserve Carry flag
        let current_carry = self.main_set.F & 0x01;
        self.main_set.F = flags | current_carry;

        match dest {
            AddressingMode::Register(r) => self.set_register(r, result),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let addr = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow_mut().write(addr, result);
            }
            AddressingMode::Indexed(idx, disp) => {
                let base = self.get_index_register(idx);
                let addr = base.wrapping_add(disp as i16 as u16);
                self.memory.borrow_mut().write(addr, result);
            }
            AddressingMode::Special(r) => {
                self.set_special_register(r, result as u16);
            }
            _ => panic!("Invalid addressing mode for DEC writeback"),
        }
    }

    fn inc_16_op(&mut self, dest: AddressingMode) {
        let val = match dest {
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::Special(r) => self.get_special_register(r),
            _ => panic!("Invalid addressing mode for INC 16"),
        };

        let result = val.wrapping_add(1);

        match dest {
            AddressingMode::RegisterPair(rp) => self.set_register_pair(rp, result),
            AddressingMode::Special(r) => self.set_special_register(r, result),
            _ => panic!("Invalid addressing mode for INC 16 writeback"),
        }
    }

    fn dec_16_op(&mut self, dest: AddressingMode) {
        let val = match dest {
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::Special(r) => self.get_special_register(r),
            _ => panic!("Invalid addressing mode for DEC 16"),
        };

        let result = val.wrapping_sub(1);

        match dest {
            AddressingMode::RegisterPair(rp) => self.set_register_pair(rp, result),
            AddressingMode::Special(r) => self.set_special_register(r, result),
            _ => panic!("Invalid addressing mode for DEC 16 writeback"),
        }
    }

    fn add_16_op(&mut self, dest: AddressingMode, src: AddressingMode) {
        let val_dest = match dest {
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::Special(r) => self.get_special_register(r),
            _ => panic!("Invalid addressing mode for ADD 16 dest"),
        };

        let val_src = match src {
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::Special(r) => self.get_special_register(r),
            _ => panic!("Invalid addressing mode for ADD 16 src"),
        };

        let (result, flags) = add_16(val_dest, val_src, self.main_set.F);
        self.main_set.F = flags;

        match dest {
            AddressingMode::RegisterPair(rp) => self.set_register_pair(rp, result),
            AddressingMode::Special(r) => self.set_special_register(r, result),
            _ => panic!("Invalid addressing mode for ADD 16 writeback"),
        }
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
                    AddressingMode::Special(SpecialRegister::IXH)
                }
                AddressingMode::Register(GPR::L) => {
                    test_log!(self, "IXL");
                    AddressingMode::Special(SpecialRegister::IXL)
                }
                AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                    test_log!(self, "(IX + d)");
                    AddressingMode::Indexed(IndexRegister::IX, self.fetch_displacement())
                }
                AddressingMode::RegisterPair(RegisterPair::HL) => {
                    test_log!(self, "IX");
                    AddressingMode::Special(SpecialRegister::IX)
                }
                _ => reg,
            },
            PrefixAddressing::IY => match reg {
                AddressingMode::Register(GPR::H) => {
                    test_log!(self, "IYH");
                    AddressingMode::Special(SpecialRegister::IYH)
                }
                AddressingMode::Register(GPR::L) => {
                    test_log!(self, "IYL");
                    AddressingMode::Special(SpecialRegister::IYL)
                }
                AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                    test_log!(self, "(IY + d)");
                    AddressingMode::Indexed(IndexRegister::IY, self.fetch_displacement())
                }
                AddressingMode::RegisterPair(RegisterPair::HL) => {
                    test_log!(self, "IY");
                    AddressingMode::Special(SpecialRegister::IY)
                }
                _ => reg,
            },
        }
    }

    fn evaluate_condition(&mut self, condition: Condition) -> bool {
        match condition {
            Condition::NZ => !self.main_set.get_flag(Flag::Z),
            Condition::Z => self.main_set.get_flag(Flag::Z),
            Condition::NC => !self.main_set.get_flag(Flag::C),
            Condition::C => self.main_set.get_flag(Flag::C),
            Condition::PO => !self.main_set.get_flag(Flag::PV),
            Condition::PE => self.main_set.get_flag(Flag::PV),
            Condition::P => !self.main_set.get_flag(Flag::S),
            Condition::M => self.main_set.get_flag(Flag::S),
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
                        self.swap_registers(
                            RegisterPair::AF,
                            RegSet::Main,
                            RegisterPair::AF,
                            RegSet::Alt,
                        ); // EX AF, AF'
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
                        self.add_16_op(dest, src);
                    } else {
                        // 16-bit load immediate/add
                        // LD rp[p], nn
                        test_log!(self, "LD rp[p], nn");
                        let nn = self.fetch_word();
                        let src = self.table_rp(p);
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

                            self.main_set.set_flag((f & 0x01) == 1, Flag::C);
                            self.main_set.set_flag(false, Flag::N);
                            self.main_set.set_flag(false, Flag::H);
                        }
                        1 => {
                            // RRCA
                            test_log!(self, "RRCA");
                            let a = self.get_register(GPR::A);
                            let (res, f) = rot::rrc(a);
                            self.set_register(GPR::A, res);

                            self.main_set.set_flag((f & 0x01) == 1, Flag::C);
                            self.main_set.set_flag(false, Flag::N);
                            self.main_set.set_flag(false, Flag::H);
                        }
                        2 => {
                            // RLA
                            test_log!(self, "RLA");
                            let a = self.get_register(GPR::A);
                            let carry = self.main_set.get_flag(Flag::C);
                            let (res, f) = rot::rl(a, carry);
                            self.set_register(GPR::A, res);

                            self.main_set.set_flag((f & 0x01) == 1, Flag::C);
                            self.main_set.set_flag(false, Flag::N);
                            self.main_set.set_flag(false, Flag::H);
                        }
                        3 => {
                            // RRA
                            test_log!(self, "RRA");
                            let a = self.get_register(GPR::A);
                            let carry = self.main_set.get_flag(Flag::C);
                            let (res, f) = rot::rr(a, carry);
                            self.set_register(GPR::A, res);

                            self.main_set.set_flag((f & 0x01) == 1, Flag::C);
                            self.main_set.set_flag(false, Flag::N);
                            self.main_set.set_flag(false, Flag::H);
                        }
                        4 => {
                            // DAA
                            test_log!(self, "DAA");
                            self.daa();
                        }
                        5 => {
                            // TODO CPL
                            test_log!(self, "CPL");

                            let a = self.get_register(GPR::A);
                            let result = a ^ 0xFF;
                            self.set_register(GPR::A, result);
                            self.main_set.set_flag(true, Flag::N);
                            self.main_set.set_flag(true, Flag::H);
                            // undocummented x and y flags
                            // set bit 3 and 5 according to result
                            self.main_set.set_flag((result & 0x08) != 0, Flag::X);
                            self.main_set.set_flag((result & 0x20) != 0, Flag::Y);
                        }
                        6 => {
                            // TODO SCF
                            test_log!(self, "SCF");
                        }
                        7 => {
                            // TODO CCF
                            test_log!(self, "CCF");
                        }
                        _ => unreachable!("Invalid y value"), // should never happen
                    }
                }
                _ => unreachable!("Invalid z value"), // should never happen
            },
            1 => {
                if (z == 6) && (y == 6) {
                    test_log!(self, "HALT"); // TODO: HALT
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
                // TODO: ALU[y] r[z]
                test_log!(self, "ALU[y] r[z]");
                let alu_op = self.table_alu(y);
                let reg = self.table_r(z);
                let src = self.transform_register(reg, addressing);
                let value = match src {
                    AddressingMode::Register(r) => self.get_register(r),
                    AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                        let addr = self.get_register_pair(RegisterPair::HL);
                        self.memory.borrow().read(addr)
                    }
                    _ => unreachable!("Invalid addressing mode for ALU[y] r[z]"), // should never happen (for now)
                };
                self.alu_op(alu_op, value);
            }
            3 => match z {
                0 => test_log!(self, "RET cc[y]"), // TODO: RET cc[y]
                1 => match (q, p) {
                    // POP & various ops
                    (false, _) => test_log!(self, "POP rp2[p]"), // TODO: POP rp2[p]
                    (true, 0) => test_log!(self, "RET"),         // TODO: RET
                    (true, 1) => {
                        test_log!(self, "EXX");
                        // EXX
                        self.swap_registers(
                            RegisterPair::BC,
                            RegSet::Main,
                            RegisterPair::BC,
                            RegSet::Alt,
                        );
                        self.swap_registers(
                            RegisterPair::DE,
                            RegSet::Main,
                            RegisterPair::DE,
                            RegSet::Alt,
                        );
                        self.swap_registers(
                            RegisterPair::HL,
                            RegSet::Main,
                            RegisterPair::HL,
                            RegSet::Alt,
                        );
                    }
                    (true, 2) => test_log!(self, "JP HL"), // TODO: JP HL
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
                2 => test_log!(self, "JP cc[y], nn"), // TODO: JP cc[y], nn
                3 => match y {
                    // Assorted operations
                    0 => test_log!(self, "JP nn"),      // TODO: JP nn
                    1 => test_log!(self, "CB prefix"),  // TODO: CB prefix
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

                        let rp = if let AddressingMode::RegisterPair(rp) = register_pair {
                            self.main_set.get_pair(rp)
                        } else {
                            unreachable!("Invalid addressing mode for EX (SP), HL/IX/IY"); // should never happen
                        };
                        self.memory.borrow_mut().write(self.SP, (rp & 0xFF) as u8);
                        self.memory
                            .borrow_mut()
                            .write(self.SP.wrapping_add(1), (rp >> 8) as u8);
                        if let AddressingMode::RegisterPair(rp) = register_pair {
                            self.set_register_pair(rp, ((temp_h as u16) << 8) | (temp_l as u16));
                        } else {
                            unreachable!("Invalid addressing mode for EX (SP), HL/IX/IY"); // should never happen
                        }
                    }
                    5 => {
                        test_log!(self, "EX DE, HL");
                        self.swap_registers(
                            RegisterPair::DE,
                            RegSet::Main,
                            RegisterPair::HL,
                            RegSet::Main,
                        )
                    } // EX DE, HL (NOTE: REMAINS UNCHANGED)
                    6 => test_log!(self, "DI"),           // TODO: DI
                    7 => test_log!(self, "EI"),           // TODO: EI
                    _ => unreachable!("Invalid y value"), // should never happen
                },
                4 => test_log!(self, "CALL cc[y], nn"), // TODO: CALL cc[y], nn
                5 => match (q, p) {
                    // PUSH & various ops
                    (false, _) => test_log!(self, "PUSH rp2[p]"), // TODO: PUSH rp2[p]
                    (true, 0) => test_log!(self, "CALL nn"),      // TODO: CALL nn
                    (true, 1) => test_log!(self, "DD prefix"),    // TODO: DD prefix
                    (true, 2) => test_log!(self, "ED prefix"),    // TODO: ED prefix
                    (true, 3) => test_log!(self, "FD prefix"),    // TODO: FD prefix
                    _ => unreachable!("Invalid q, p values"),     // should never happen
                },
                6 => {
                    // TODO: ALU[y] n
                    test_log!(self, "ALU[y] n");
                    let alu_op = self.table_alu(y);
                    let n = self.fetch();
                    self.alu_op(alu_op, n);
                }
                7 => test_log!(self, "RST y*8"), // TODO: RST y*8
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
                test_log!(self, &format!("{}", y));
                let reg = self.table_r(z);
                self.bit(y, reg)
            } // NOTE: BIT y, r[z]
            2 => {
                test_log!(self, "RES y, r[z]");
                test_log!(self, &format!("{}", y));
                let reg = self.table_r(z);
                self.res(y, reg)
            } // NOTE: RES y, r[z]
            3 => {
                test_log!(self, "SET y, r[z]");
                test_log!(self, &format!("{}", y));
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
                        test_log!(self, "SBC HL, rp[p]"); // TODO: SBC HL, rp[p]
                    } else {
                        test_log!(self, "ADC HL, rp[p]"); // TODO: ADC HL, rp[p]
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
                        test_log!(self, "NEG"); // TODO: NEG
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
                            AddressingMode::Special(SpecialRegister::I),
                            AddressingMode::Register(GPR::A),
                        )
                    } //  LD I, A
                    1 => {
                        test_log!(self, "LD R, A");
                        self.ld(
                            AddressingMode::Special(SpecialRegister::R),
                            AddressingMode::Register(GPR::A),
                        )
                    } //  LD R, A
                    2 => {
                        test_log!(self, "LD A, I");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::Special(SpecialRegister::I),
                        )
                    } //  LD A, I
                    3 => {
                        test_log!(self, "LD A, R");
                        self.ld(
                            AddressingMode::Register(GPR::A),
                            AddressingMode::Special(SpecialRegister::R),
                        )
                    } //  LD A, R
                    4 => test_log!(self, "RRD"),          // TODO: RRD
                    5 => test_log!(self, "RLD"),          // TODO: RLD
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
        let value = match src {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::Immediate(n) => n,
            AddressingMode::RegisterIndirect(rp) => {
                let address = self.get_register_pair(rp);
                self.memory.borrow().read(address)
            }
            AddressingMode::Absolute(address) => self.memory.borrow().read(address),
            AddressingMode::Indexed(index, displacement) => {
                let base = match index {
                    IndexRegister::IX => self.IX,
                    IndexRegister::IY => self.IY,
                };
                let address = base.wrapping_add_signed(displacement as i16);
                self.memory.borrow().read(address)
            }
            AddressingMode::Special(reg) => self.get_special_register(reg) as u8,
            _ => panic!("Unsupported source addressing mode for LD"),
        };

        match dest {
            AddressingMode::Register(r) => self.main_set.set_register(r, value),
            AddressingMode::RegisterIndirect(rp) => {
                let address = self.get_register_pair(rp);
                self.memory.borrow_mut().write(address, value)
            }
            AddressingMode::Absolute(address) => self.memory.borrow_mut().write(address, value),
            AddressingMode::Indexed(index, displacement) => {
                let base = match index {
                    IndexRegister::IX => self.IX,
                    IndexRegister::IY => self.IY,
                };
                let address = base.wrapping_add_signed(displacement as i16);
                self.memory.borrow_mut().write(address, value)
            }
            AddressingMode::Special(reg) => self.set_special_register(reg, value as u16),
            _ => panic!("Unsupported destination addressing mode for LD"),
        }
    }

    fn ld_16(&mut self, dest: AddressingMode, src: AddressingMode) {
        let value = match src {
            AddressingMode::ImmediateExtended(nn) => nn,
            AddressingMode::Absolute(nn) => self.memory.borrow().read_word(nn),
            AddressingMode::RegisterPair(rp) => self.get_register_pair(rp),
            AddressingMode::Special(SpecialRegister::IX) => self.IX,
            AddressingMode::Special(SpecialRegister::IY) => self.IY,
            _ => panic!("Unsupported source addressing mode for LD 16"),
        };

        match dest {
            AddressingMode::RegisterPair(rp) => self.set_register_pair(rp, value),
            AddressingMode::Absolute(addr) => {
                self.memory.borrow_mut().write_word(addr, value);
            }
            AddressingMode::Special(SpecialRegister::IX) => self.IX = value,
            AddressingMode::Special(SpecialRegister::IY) => self.IY = value,
            AddressingMode::Special(SpecialRegister::SP) => self.SP = value,
            _ => panic!("Unsupported destination addressing mode for LD 16"),
        }
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
        let value = match reg {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(address)
            }
            _ => panic!("Unsupported addressing mode for ROT"),
        };

        let (result, flags) = match operation {
            RotOperation::RLC => rot::rlc(value),
            RotOperation::RRC => rot::rrc(value),
            RotOperation::RL => rot::rl(value, self.main_set.get_flag(Flag::C)),
            RotOperation::RR => rot::rr(value, self.main_set.get_flag(Flag::C)),
            RotOperation::SLA => rot::sla(value),
            RotOperation::SRA => rot::sra(value),
            RotOperation::SLL => rot::sll(value),
            RotOperation::SRL => rot::srl(value),
        };
        self.main_set.set_register(GPR::F, flags);
        match reg {
            AddressingMode::Register(r) => self.main_set.set_register(r, result),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow_mut().write(address, result)
            }
            _ => panic!("Unsupported addressing mode for ROT"),
        }
    }

    fn bit(&mut self, y: u8, reg: AddressingMode) {
        let value = match reg {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(address)
            }
            _ => panic!("Unsupported addressing mode for BIT"),
        };

        let prev_c = self.main_set.get_flag(Flag::C);
        self.set_register(GPR::F, bit(value, y));

        self.main_set.set_flag(prev_c, Flag::C); // C flag is not affected
    }

    fn res(&mut self, y: u8, reg: AddressingMode) {
        let value = match reg {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(address)
            }
            _ => panic!("Unsupported addressing mode for RES"),
        };

        let result = res(value, y);

        match reg {
            AddressingMode::Register(r) => self.main_set.set_register(r, result),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow_mut().write(address, result)
            }
            _ => panic!("Unsupported addressing mode for RES"),
        }
    }

    fn set(&mut self, y: u8, reg: AddressingMode) {
        let value = match reg {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow().read(address)
            }
            _ => panic!("Unsupported addressing mode for SET"),
        };

        let result = set(value, y);

        match reg {
            AddressingMode::Register(r) => self.main_set.set_register(r, result),
            AddressingMode::RegisterIndirect(RegisterPair::HL) => {
                let address = self.get_register_pair(RegisterPair::HL);
                self.memory.borrow_mut().write(address, result)
            }
            _ => panic!("Unsupported addressing mode for SET"),
        }
    }

    fn daa(&mut self) {
        let mut t: u8 = 0;
        let mut a = self.get_register(GPR::A);

        if self.main_set.get_flag(Flag::H) || ((a & 0xf) > 9) {
            t = t.wrapping_add(1);
        }

        if self.main_set.get_flag(Flag::C) || (a > 0x99) {
            t = t.wrapping_add(2);
            self.main_set.set_flag(true, Flag::C);
        }

        if self.main_set.get_flag(Flag::N) && !self.main_set.get_flag(Flag::H) {
            self.main_set.set_flag(false, Flag::H);
        } else {
            if self.main_set.get_flag(Flag::N) && self.main_set.get_flag(Flag::H) {
                self.main_set.set_flag((a & 0x0f) < 6, Flag::H);
            } else {
                self.main_set.set_flag((a & 0x0f) >= 0x0a, Flag::H);
            }
        }

        if t == 1 {
            a = a.wrapping_add(if self.main_set.get_flag(Flag::N) {
                0xFA
            } else {
                0x06
            })
        } else if t == 2 {
            a = a.wrapping_add(if self.main_set.get_flag(Flag::N) {
                0xA0
            } else {
                0x60
            })
        } else if t == 3 {
            a = a.wrapping_add(if self.main_set.get_flag(Flag::N) {
                0x9A
            } else {
                0x66
            })
        }

        self.set_register(GPR::A, a);

        self.main_set.set_flag(a & 0x80 == 0x80, Flag::S);
        self.main_set.set_flag(a == 0, Flag::Z);
        self.main_set.set_flag(a.count_ones() % 2 == 0, Flag::PV);
        self.main_set.set_flag(a & 0x08 == 0x08, Flag::X);
        self.main_set.set_flag(a & 0x20 == 0x20, Flag::Y);
    }
}

impl SyncronousComponent for Z80A {
    fn tick(&mut self) {
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
