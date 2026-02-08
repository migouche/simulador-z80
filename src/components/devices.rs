use crate::traits::IODevice;
use crate::ui_traits::DeviceWithUi;
use eframe::egui;

pub struct SevenSegmentDisplay {
    port: u16,
    value: u8,
    is_open: bool,
}

impl SevenSegmentDisplay {
    pub fn new(port: u16) -> Self {
        SevenSegmentDisplay {
            port,
            value: 0,
            is_open: true,
        }
    }
    pub fn get_value(&self) -> u8 {
        self.value
    }
    pub fn get_port(&self) -> u16 {
        self.port
    }
}

impl IODevice for SevenSegmentDisplay {
    fn read_in(&mut self, _port: u16) -> Option<u8> {
        None
    }

    fn write_out(&mut self, port: u16, data: u8) -> bool {
        // Ignore upper 8 bits of port addressing for simple I/O decoding
        if (port & 0xFF) == (self.port & 0xFF) {
            self.value = data;
            true
        } else {
            false
        }
    }
}

impl DeviceWithUi for SevenSegmentDisplay {
    fn get_name(&self) -> String {
        format!("Seven Segment Display (Port 0x{:02X})", self.port)
    }

    fn get_window_open_state(&self) -> bool {
        self.is_open
    }

    fn set_window_open_state(&mut self, open: bool) {
        self.is_open = open;
    }

    fn draw(&mut self, ctx: &egui::Context) {
        let mut open = self.is_open;
        egui::Window::new(self.get_name())
            .open(&mut open)
            .show(ctx, |ui| {
                ui.group(|ui| {
                    ui.label(format!("Port: 0x{:02X}", self.port));
                    ui.separator();

                    // Simple text representation of 7-segment
                    let val = self.value;

                    // Draw a big number
                    ui.vertical_centered(|ui| {
                        let text = egui::RichText::new(format!("{:02X}", val))
                            .size(40.0)
                            .family(egui::FontFamily::Monospace)
                            .strong();
                        ui.label(text);

                        // Binary representation
                        ui.label(format!("Binary: {:08b}", val));
                    });
                });
            });
        self.is_open = open;
    }
}

pub struct Keypad {
    port: u16,
    current_key: u8,
    interrupt_pending: bool,
    is_open: bool,
}

impl Keypad {
    pub fn new(port: u16) -> Self {
        Keypad {
            port,
            current_key: 0,
            interrupt_pending: false,
            is_open: true,
        }
    }

    pub fn press_key(&mut self, key: u8) {
        self.current_key = key;
        self.interrupt_pending = true;
    }

    pub fn get_last_key(&self) -> u8 {
        self.current_key
    }

    pub fn get_port(&self) -> u16 {
        self.port
    }
}

impl IODevice for Keypad {
    fn read_in(&mut self, port: u16) -> Option<u8> {
        // Ignore upper 8 bits of port addressing for simple I/O decoding
        if (port & 0xFF) == (self.port & 0xFF) {
            self.interrupt_pending = false;
            Some(self.current_key)
        } else {
            None
        }
    }

    fn write_out(&mut self, _port: u16, _data: u8) -> bool {
        false
    }

    fn poll_interrupt(&self) -> bool {
        self.interrupt_pending
    }

    fn ack_interrupt(&mut self) -> u8 {
        // Simple default vector 0xFF, effectively RST 38h in Mode 0, or vector FF in Mode 2
        self.interrupt_pending = false;
        0xFF
    }
}

impl DeviceWithUi for Keypad {
    fn get_name(&self) -> String {
        format!("Keypad (Port 0x{:02X})", self.port)
    }

    fn get_window_open_state(&self) -> bool {
        self.is_open
    }

    fn set_window_open_state(&mut self, open: bool) {
        self.is_open = open;
    }

    fn draw(&mut self, ctx: &egui::Context) {
        let mut open = self.is_open;
        egui::Window::new(self.get_name())
            .open(&mut open)
            .show(ctx, |ui| {
                ui.group(|ui| {
                    ui.label(format!("Port: 0x{:02X}", self.port));
                    ui.separator();

                    ui.label(format!("Last Key: {:02X}", self.current_key));

                    egui::Grid::new("keypad_grid")
                        .spacing(egui::vec2(5.0, 5.0))
                        .show(ui, |ui| {
                            let keys = [
                                [(0x1, "1"), (0x2, "2"), (0x3, "3"), (0xA, "A")],
                                [(0x4, "4"), (0x5, "5"), (0x6, "6"), (0xB, "B")],
                                [(0x7, "7"), (0x8, "8"), (0x9, "9"), (0xC, "C")],
                                [(0xE, "*"), (0x0, "0"), (0xF, "#"), (0xD, "D")],
                            ];

                            for row in keys {
                                for (code, label) in row {
                                    if ui
                                        .add(
                                            egui::Button::new(label)
                                                .min_size(egui::vec2(30.0, 30.0)),
                                        )
                                        .clicked()
                                    {
                                        self.press_key(code);
                                    }
                                }
                                ui.end_row();
                            }
                        });
                });
            });
        self.is_open = open;
    }
}
pub struct GenericInterruptDevice {
    port: u16,
    vector: u8,
    interrupt_pending: bool,
    is_open: bool,
    // UI temporary state
    vector_input: String,
}

impl GenericInterruptDevice {
    pub fn new(port: u16) -> Self {
        Self {
            port,
            vector: 0xFF, // Default RST 38h
            interrupt_pending: false,
            is_open: true,
            vector_input: "FF".to_string(),
        }
    }

    pub fn trigger(&mut self) {
        self.interrupt_pending = true;
    }
}

impl IODevice for GenericInterruptDevice {
    fn read_in(&mut self, _port: u16) -> Option<u8> {
        None
    }

    fn write_out(&mut self, _port: u16, _data: u8) -> bool {
        false
    }

    fn poll_interrupt(&self) -> bool {
        self.interrupt_pending
    }

    fn ack_interrupt(&mut self) -> u8 {
        self.interrupt_pending = false;
        self.vector
    }
}

impl DeviceWithUi for GenericInterruptDevice {
    fn get_name(&self) -> String {
        format!("Int Controller (Port 0x{:02X})", self.port)
    }

    fn get_window_open_state(&self) -> bool {
        self.is_open
    }

    fn set_window_open_state(&mut self, open: bool) {
        self.is_open = open;
    }

    fn draw(&mut self, ctx: &egui::Context) {
        let mut open = self.is_open;
        egui::Window::new(self.get_name())
            .open(&mut open)
            .show(ctx, |ui| {
                ui.group(|ui| {
                    ui.label("Control interrupt generation for Mode 0 or 2.");
                    ui.separator();

                    ui.horizontal(|ui| {
                        ui.label("Vector/Opcode (Hex):");
                        if ui.text_edit_singleline(&mut self.vector_input).changed() {
                            if let Ok(val) = u8::from_str_radix(&self.vector_input, 16) {
                                self.vector = val;
                            }
                        }
                    });

                    ui.label(format!("Current Vector: 0x{:02X}", self.vector));

                    if ui.button("TRIGGER INTERRUPT").clicked() {
                        self.trigger();
                    }

                    if self.interrupt_pending {
                        ui.colored_label(egui::Color32::RED, "Interrupt Pending...");
                    } else {
                        ui.label("Idle");
                    }
                });
            });
        self.is_open = open;
    }
}
