#[cfg(test)]
mod tests {
    use crate::assembler::assemble;
    use std::collections::HashMap;

    #[test]
    fn test_ds_basic() {
        let code = "
            ORG 0x100
            DS 5
            DEFS 2, 0xAA
        ";
        let (bytes, _, _, _) = assemble(code).unwrap();

        let total_len = 0x100 + 5 + 2;
        assert_eq!(bytes.len(), total_len);
        assert_eq!(&bytes[0x100..0x100 + 5], &[0, 0, 0, 0, 0]);
        assert_eq!(&bytes[0x100 + 5..], &[0xAA, 0xAA]);
    }

    #[test]
    fn test_block_instructions() {
        // These should pass parsing and emit correct opcodes
        let code = "
            LDI
            LDIR
            LDD
            LDDR
            CPI
            CPIR
            CPD
            CPDR
            INI
            INIR
            IND
            INDR
            OUTI
            OTIR
            OUTD
            OTDR
        ";
        let (bytes, _, _, _) = assemble(code).unwrap();

        let expected = vec![
            0xED, 0xA0, // LDI
            0xED, 0xB0, // LDIR
            0xED, 0xA8, // LDD
            0xED, 0xB8, // LDDR
            0xED, 0xA1, // CPI
            0xED, 0xB1, // CPIR
            0xED, 0xA9, // CPD
            0xED, 0xB9, // CPDR
            0xED, 0xA2, // INI
            0xED, 0xB2, // INIR
            0xED, 0xAA, // IND
            0xED, 0xBA, // INDR
            0xED, 0xA3, // OUTI
            0xED, 0xB3, // OTIR
            0xED, 0xAB, // OUTD
            0xED, 0xBB, // OTDR
        ];

        assert_eq!(bytes, expected);
    }
}
