use std::{cell::RefCell, ops::Add};
use std::rc::Rc;
use std::usize;

use crate::traits::{MemoryMapper, SyncronousComponent};

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

#[derive(PartialEq)]
enum RegisterPair {
    BC,
    DE,
    HL,
    AF,
    SP,
}

enum RegSet {
    Main,
    Alt,
}

enum IndexRegister {
    IX,
    IY,
}

enum SpecialRegisters {
    PC,
    SP,
    IX,
    IY,
    I,
    R,
    A,
}

enum AddressingMode {
    Immediate(u8),
    ImmediateExtended(u16),
    Absolute(u16),
    ZeroPage,
    Relative(u8),
    Extended,
    Indexed(IndexRegister, u8),
    Register(GPR),
    Special(SpecialRegisters),
    Implied,
    RegisterIndirect(RegisterPair),
    RegisterPair(RegisterPair),
}

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

enum PrefixAddressing {
    HL, // HL, H, L
    IX, // IX, IXH, IXL
    IY, // IY, IYH, IYL
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
        let flag_byte = self.F;
        match flag {
            Flag::C => flag_byte & 0b00000001 != 0,
            Flag::N => flag_byte & 0b00000010 != 0,
            Flag::PV => flag_byte & 0b00000100 != 0,
            Flag::Y => flag_byte & 0b00001000 != 0,
            Flag::H => flag_byte & 0b00010000 != 0,
            Flag::X => flag_byte & 0b00100000 != 0,
            Flag::Z => flag_byte & 0b01000000 != 0,
            Flag::S => flag_byte & 0b10000000 != 0,
        }
    }

    pub fn get_flag_i(&self, usize: usize) -> bool {
        self.F & (1 << usize) != 0
    }
}

fn decode_opcode(opcode: u8) -> (u8, u8, u8, u8, bool)  // x, y, z, p, q
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
        }
    }

    fn fetch(&mut self) -> u8 {
        let data = self.memory.borrow().read(self.PC);
        self.PC += 1;
        data
    }

    fn fetch_word(&mut self) -> u16 {
        ((self.fetch() as u16) << 8) | (self.fetch() as u16)
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

    // TABLES FROM http://www.z80.info/decoding.htm

    fn table_r(p: u8) -> AddressingMode {
        match p {
            0 => AddressingMode::Register(GPR::B),
            1 => AddressingMode::Register(GPR::C),
            2 => AddressingMode::Register(GPR::D),
            3 => AddressingMode::Register(GPR::E),
            4 => AddressingMode::Register(GPR::H),
            5 => AddressingMode::Register(GPR::L),
            6 => AddressingMode::RegisterIndirect(RegisterPair::HL),
            7 => AddressingMode::Register(GPR::A),
            _ => panic!("Invalid p value"), // should never happen
        }
    }

    fn table_rp(p: u8) -> RegisterPair {
        match p {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::SP,
            _ => panic!("Invalid p value"), // should never happen
        }
    }

    fn table_rp2(p: u8) -> RegisterPair {
        match p {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => panic!("Invalid p value"), // should never happen
        }
    }




    fn decode_unprefixed(&mut self, opcode: u8, addressing: PrefixAddressing) -> () {

        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => match z {
                0 => match y {
                    // Relative jumps and assorted ops
                    0 => (), // NOP
                    1 => self.swap_registers(
                        RegisterPair::AF,
                        RegSet::Main,
                        RegisterPair::AF,
                        RegSet::Alt,
                    ),
                    2 => (),                        // TODO: DJNZ d
                    3..=7 => (),                    // TODO: JR cc[y-4], d
                    _ => panic!("Invalid y value"), // should never happen
                },

                1 => {
                    if q {
                        // 16-bit load immediate/add
                        // LD rp[p], nn
                        let nn = self.fetch_word();
                        self.ld_16(AddressingMode::RegisterPair(Self::table_rp(p)), AddressingMode::ImmediateExtended(nn));
                    } else {
                        () // TODO: ADD HL, rp[p]
                    }
                }

                2 => match (q, p) {
                    // Indirect loading
                    (false, 0) => self.ld(AddressingMode::RegisterIndirect(RegisterPair::BC), AddressingMode::Register(GPR::A)),                  // LD (BC), A
                    (false, 1) => self.ld(AddressingMode::RegisterIndirect(RegisterPair::DE), AddressingMode::Register(GPR::A)),                  // LD (DE), A
                    (false, 2) => self.ld(AddressingMode::RegisterIndirect(RegisterPair::HL), AddressingMode::Register(GPR::H)),                  // LD (nn), HL
                    (false, 3) => self.ld(AddressingMode::RegisterIndirect(RegisterPair::SP), AddressingMode::Register(GPR::A)),                  // LD (nn), A
                    (true, 0) => self.ld(AddressingMode::Register(GPR::A), AddressingMode::RegisterIndirect(RegisterPair::BC)),                   // LD A, (BC)
                    (true, 1) => self.ld(AddressingMode::Register(GPR::A), AddressingMode::RegisterIndirect(RegisterPair::DE)),                   // LD A, (DE)
                    (true, 2) => {
                        let addr = self.fetch_word();
                        self.ld_16(AddressingMode::RegisterPair(RegisterPair::HL), AddressingMode::Absolute(addr))
                    },         // LD HL, (nn)
                    (true, 3) => {
                        let addr = self.fetch_word();
                        self.ld(AddressingMode::Register(GPR::A), AddressingMode::Absolute(addr))
                    },                   // LD A, (nn)
                    _ => panic!("Invalid q, p values"), // should never happen
                },

                3 => {
                    if q {
                        // 16-bit INC/DEC
                        () // TODO: INC rp[p]
                    } else {
                        () // TODO: DEC rp[p]
                    }
                }

                4 => (), // TODO: INC r[y]
                5 => (), // TODO: DEC r[y]
                6 => {
                    let n = self.fetch();
                    self.ld(Self::table_r(y), AddressingMode::Immediate(n))
                }, //  LD r[y], n
                7 => {
                    // Assorted operations on accumulator/flags
                    match y {
                        0 => (),                        // TODO: RLCA
                        1 => (),                        // TODO: RRCA
                        2 => (),                        // TODO: RLA
                        3 => (),                        // TODO: RRA
                        4 => (),                        // TODO: DAA
                        5 => (),                        // TODO: CPL
                        6 => (),                        // TODO: SCF
                        7 => (),                        // TODO: CCF
                        _ => panic!("Invalid y value"), // should never happen
                    }
                }
                _ => panic!("Invalid z value"), // should never happen
            },
            1 => match y {
                6 => (), // TODO: HALT
                _ => {
                    if y > 7 {
                        // 8-bit loading
                        panic!("Invalid y value") // should never happen
                    } else {
                        () // TODO: LD r[y], r[z]
                    }
                }
            },

            2 => (), // TODO: ALU[y] r[z]
            3 => match z {
                0 => (), // TODO: RET cc[y]
                1 => match (q, p) {
                    // POP & various ops
                    (false, _) => (), // TODO: POP rp2[p]
                    (true, 0) => (),  // TODO: RET
                    (true, 1) => {
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
                    (true, 2) => (),                    // TODO: JP HL
                    (true, 3) => (),                    // TODO: LD SP, HL
                    _ => panic!("Invalid q, p values"), // should never happen
                },
                2 => (), // TODO: JP cc[y], nn
                3 => match y {
                    // Assorted operations
                    0 => (), // TODO: JP nn
                    1 => (), // TODO: CB prefix
                    2 => (), // TODO: OUT (n), A
                    3 => (), // TODO: IN A, (n)
                    4 => {
                        // EX (SP), HL
                        let w = self.memory.borrow().read_word(self.SP);
                        self.memory
                            .borrow_mut()
                            .write_word(self.SP, self.main_set.get_pair(RegisterPair::HL));
                        self.main_set.set_pair(RegisterPair::HL, w);
                    }
                    5 => self.swap_registers(
                        RegisterPair::DE,
                        RegSet::Main,
                        RegisterPair::HL,
                        RegSet::Main,
                    ), // EX DE, HL
                    6 => (),                        // TODO: DI
                    7 => (),                        // TODO: EI
                    _ => panic!("Invalid y value"), // should never happen
                },
                4 => (), // TODO: CALL cc[y], nn
                5 => match (q, p) {
                    // PUSH & various ops
                    (false, _) => (),                   // TODO: PUSH rp2[p]
                    (true, 0) => (),                    // TODO: CALL nn
                    (true, 1) => (),                    // TODO: DD prefix
                    (true, 2) => (),                    // TODO: ED prefix
                    (true, 3) => (),                    // TODO: FD prefix
                    _ => panic!("Invalid q, p values"), // should never happen
                },
                6 => (),                        // TODO: ALU[y] n
                7 => (),                        // TODO: RST y*8
                _ => panic!("Invalid z value"), // should never happen
            },
            _ => panic!("Invalid x value"), // should never happen
        }
    }

    fn decode_cb(&mut self, opcode: u8) -> () {
        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => (),                        // TODO: rot[y] r[z]
            1 => (),                        // TODO: BIT y, r[z]
            2 => (),                        // TODO: RES y, r[z]
            3 => (),                        // TODO: SET y, r[z]
            _ => panic!("Invalid x value"), // should never happen
        }
    }

    fn decode_ed(&mut self, opcode: u8) -> () {
        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 | 3 => (), // NOTE: NONI
            1 => match z {
                0 => {
                    if y == 6 {
                        () // TODO: IN(C)
                    } else if y < 8 {
                        () // TODO: IN r[y], (C)
                    } else {
                        panic!("Invalid y value") // should never happen
                    }
                }
                1 => {
                    if y == 6 {
                        () // TODO: OUT(C), 0
                    } else if y < 8 {
                        () // TODO: OUT (C), r[y]
                    } else {
                        panic!("Invalid y value") // should never happen
                    }
                }
                2 => {
                    if q {
                        () // TODO: SBC HL, rp[p]
                    } else {
                        () // TODO: ADC HL, rp[p]
                    }
                }
                3 => {
                    if q {
                        () // TODO: LD (nn), rp[p]
                    } else {
                        () // TODO: LD rp[p], (nn)
                    }
                }
                4 => (), // TODO: NEG
                5 => {
                    if y == 1 {
                        () // TODO: RETI
                    } else if y < 8 {
                        () // TODO: RETN
                    } else {
                        panic!("Invalid y value") // should never happen
                    }
                }
                6 => (), // TODO: IM y
                7 => match y {
                    0 => (),                        // TODO: LD I, A
                    1 => (),                        // TODO: LD R, A
                    2 => (),                        // TODO: LD A, I
                    3 => (),                        // TODO: LD A, R
                    4 => (),                        // TODO: RRD
                    5 => (),                        // TODO: RLD
                    6 => (),                        // TODO: NOP
                    7 => (),                        // TODO: NOP
                    _ => panic!("Invalid y value"), // should never happen
                },
                _ => panic!("Invalid z value"), // should never happen
            },
            2 => {
                if y < 4 {
                    () // TODO: NONI
                } else if y < 8 {
                    () // TODO: bli[y,z]
                } else {
                    panic!("Invalid y value") // should never happen
                }
            }
            _ => panic!("Invalid x value"), // should never happen
        }
    }

    fn decode_dd(&mut self, opcode: u8) {
        match opcode {
            0xDD | 0xED | 0xFD => {
                // Current DD is ignored (NONI), fetch next byte and process it
                let next_opcode = self.fetch();
                self.decode(next_opcode)
            }
            0xCB => {
                let displacement = self.fetch();
                let op = self.fetch();
                self.decode_ddcb_fdcb(op, IndexRegister::IX, displacement)
            }
            _ => self.decode_unprefixed(opcode, PrefixAddressing::IX),
        }
    }

    fn decode_fd(&mut self, opcode: u8) {
        match opcode {
            0xDD | 0xED | 0xFD => {
                // Current FD is ignored (NONI), fetch next byte and process it
                let next_opcode = self.fetch();
                self.decode(next_opcode)
            }
            0xCB => {
                let displacement = self.fetch();
                let op = self.fetch();
                self.decode_ddcb_fdcb(op, IndexRegister::IY, displacement)
            }
            _ => self.decode_unprefixed(opcode, PrefixAddressing::IY),
        }
    }

    fn decode_ddcb_fdcb(&mut self, opcode: u8, indexing: IndexRegister, displacement: u8) {
        let (x, y, z, p, q) = decode_opcode(opcode);
        match x {
            0 => {
                if z == 6 {
                    () // TODO: rot[y] (IX+d)
                } else if z < 8 {
                    () // TODO: LD r[z], rot[y] (IX+d)
                } else {
                    panic!("Invalid z value") // should never happen
                }
            }
            1 => (), // TODO: BIT y, (IX+d)
            2 => {
                if z == 6 {
                    () // TODO: RES y, (IX+d)
                } else if z < 8 {
                    () // TODO: LD r[z], RES y, (IX+d)
                } else {
                    panic!("Invalid z value") // should never happen
                }
            }
            3 => {
                if z == 6 {
                    () // TODO: SET y, (IX+d)
                } else if z < 8 {
                    () // TODO: LD r[z], SET y, (IX+d)
                } else {
                    panic!("Invalid z value") // should never happen
                }
            }
            _ => panic!("Invalid x value"), // should never happen
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

    fn ld(&mut self, dest: AddressingMode, src: AddressingMode){
        let value = match src {
            AddressingMode::Register(r) => self.main_set.get_register(r),
            AddressingMode::Immediate(n) => n,
            AddressingMode::RegisterIndirect(rp) => {
                let address = self.main_set.get_pair(rp);
                self.memory.borrow().read(address)
            },
            AddressingMode::Absolute(address) => self.memory.borrow().read(address),
            AddressingMode::Indexed(index, displacement) => {
                let base = match index {
                    IndexRegister::IX => self.IX,
                    IndexRegister::IY => self.IY,
                };
                let address = base.wrapping_add(displacement as u16);
                self.memory.borrow().read(address)
            },
            AddressingMode::Special(reg) => match reg {
                SpecialRegisters::A => self.main_set.A,
                SpecialRegisters::I => self.I,
                SpecialRegisters::R => self.R,
                _ => panic!("Unsupported special register for LD"),
            },
            _ => panic!("Unsupported source addressing mode for LD"),

        };

        match dest {
            AddressingMode::Register(r) => self.main_set.set_register(r, value),
            AddressingMode::RegisterIndirect(rp) => {
                let address = self.main_set.get_pair(rp);
                self.memory.borrow_mut().write(address, value)
            },
            AddressingMode::Absolute(address) => {
                self.memory.borrow_mut().write(address, value)
            },
            AddressingMode::Indexed(index, displacement) => {
                let base = match index {
                    IndexRegister::IX => self.IX,
                    IndexRegister::IY => self.IY,
                };
                let address = base.wrapping_add(displacement as u16);
                self.memory.borrow_mut().write(address, value)
            },
            AddressingMode::Special(reg) => match reg {
                SpecialRegisters::A => self.main_set.A = value,
                SpecialRegisters::I => self.I = value,
                SpecialRegisters::R => self.R = value,
                _ => panic!("Unsupported special register for LD"),
            },
            _ => panic!("Unsupported destination addressing mode for LD"),
        }
    }

    fn ld_16(&mut self, dest: AddressingMode, src: AddressingMode){
        let value = match src {
            AddressingMode::ImmediateExtended(nn) => nn,
            AddressingMode::Absolute(nn) => self.memory.borrow().read_word(nn),
            _ => panic!("Unsupported source addressing mode for LD 16"),
        };
        match dest {
            AddressingMode::RegisterPair(rp) => if rp == RegisterPair::SP {
                self.SP = value;
            } else {
                self.main_set.set_pair(rp, value);
            },
            _ => panic!("Unsupported destination addressing mode for LD 16"),
        }
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