use crate::assembler::assemble;
use eframe::egui;
use std::cell::RefCell;
use std::rc::Rc;

use crate::components::memories::mem_64k::Mem64k;
use crate::cpu::{Flag, Z80A, GPR};
use crate::traits::{MemoryMapper, SyncronousComponent};

pub fn run() -> eframe::Result<()> {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "Z80 Simulator",
        options,
        Box::new(|_cc| Ok(Box::new(Z80App::default()))),
    )
}

struct Z80App {
    cpu: Z80A,
    memory: Rc<RefCell<dyn MemoryMapper>>,
    code: String,
    last_error: Option<String>,
    running: bool,
}

impl Default for Z80App {
    fn default() -> Self {
        let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
        let cpu = Z80A::new(memory.clone());
        
        Self {
            cpu,
            memory,
            code: "LD A, 0x42\nLD (0x2000), A\nHALT".to_string(),
            last_error: None,
            running: false,
        }
    }
}

impl eframe::App for Z80App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Top Panel: Control Toolbar
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui.button("⟳ Load & Reset").clicked() {
                    // Reset
                    let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
                    self.memory = memory.clone();
                    self.cpu = Z80A::new(self.memory.clone());
                    
                    // Assemble and  Load code
                    let (bytes, error) = match assemble(&self.code) {
                        Ok(b) => (b, None),
                        Err(e) => (Vec::new(), Some(e)),
                    };

                    if let Some(err) = error {
                        self.last_error = Some(err);
                    } else {
                        self.last_error = None;
                        let mut mem = self.memory.borrow_mut();
                        for (i, b) in bytes.iter().enumerate() {
                            mem.write(i as u16, *b);
                        }
                    }
                }

                ui.separator();

                if ui.button("⏭ Step").clicked() {
                    self.cpu.tick();
                }
                
                let run_label = if self.running { "⏸ Stop" } else { "▶ Run" };
                if ui.button(run_label).clicked() {
                    self.running = !self.running;
                }

                ui.separator();

                if let Some(err) = &self.last_error {
                    ui.colored_label(egui::Color32::RED, format!("⚠ {}", err));
                } else {
                    ui.label(egui::RichText::new("Ready").color(egui::Color32::GREEN));
                }
            });
        });

        // Right Panel: Registers and Flags
        egui::SidePanel::right("right_panel")
            .resizable(true)
            .default_width(280.0)
            .show(ctx, |ui| {
                ui.heading("Registers");
                ui.separator();
                
                let reg_a = self.cpu.get_register(GPR::A);
                let reg_f = self.cpu.get_register(GPR::F);
                let reg_b = self.cpu.get_register(GPR::B);
                let reg_c = self.cpu.get_register(GPR::C);
                let reg_d = self.cpu.get_register(GPR::D);
                let reg_e = self.cpu.get_register(GPR::E);
                let reg_h = self.cpu.get_register(GPR::H);
                let reg_l = self.cpu.get_register(GPR::L);
                
                let pc = self.cpu.get_pc();
                let sp = self.cpu.get_sp();
                let ix = self.cpu.get_ix();
                let iy = self.cpu.get_iy();

                egui::Grid::new("regs_grid")
                    .striped(true)
                    .spacing([20.0, 8.0])
                    .show(ui, |ui| {
                        let mono = |text: String| egui::RichText::new(text).monospace();

                        ui.label("PC"); ui.label(mono(format!("0x{:04X}", pc))); ui.end_row();
                        ui.label("SP"); ui.label(mono(format!("0x{:04X}", sp))); ui.end_row();
                        ui.label("IX"); ui.label(mono(format!("0x{:04X}", ix))); ui.end_row();
                        ui.label("IY"); ui.label(mono(format!("0x{:04X}", iy))); ui.end_row();
                        
                        ui.separator(); ui.separator(); ui.end_row();

                        ui.label("A"); ui.label(mono(format!("0x{:02X}", reg_a))); ui.end_row();
                        ui.label("F"); ui.label(mono(format!("0x{:02X}", reg_f))); ui.end_row();
                        ui.label("B"); ui.label(mono(format!("0x{:02X}", reg_b))); ui.end_row();
                        ui.label("C"); ui.label(mono(format!("0x{:02X}", reg_c))); ui.end_row();
                        ui.label("D"); ui.label(mono(format!("0x{:02X}", reg_d))); ui.end_row();
                        ui.label("E"); ui.label(mono(format!("0x{:02X}", reg_e))); ui.end_row();
                        ui.label("H"); ui.label(mono(format!("0x{:02X}", reg_h))); ui.end_row();
                        ui.label("L"); ui.label(mono(format!("0x{:02X}", reg_l))); ui.end_row();
                    });

                ui.add_space(20.0);
                ui.heading("Flags");
                ui.separator();
                
                ui.horizontal_wrapped(|ui| {
                    ui.spacing_mut().item_spacing.x = 10.0;
                    for (flag, label) in [
                        (Flag::S, "S"), (Flag::Z, "Z"), (Flag::Y, "Y"), (Flag::H, "H"),
                        (Flag::X, "X"), (Flag::PV, "PV"), (Flag::N, "N"), (Flag::C, "C")
                    ] {
                        let on = self.cpu.get_flag(flag);
                        let color = if on { egui::Color32::from_rgb(0, 255, 0) } else { egui::Color32::from_rgb(60, 60, 60) };
                        let text_color = if on { egui::Color32::BLACK } else { egui::Color32::WHITE };
                        
                        // Drawn as a small badge
                        let text = egui::RichText::new(label).strong().color(text_color);
                        egui::Frame::new()
                            .fill(color)
                            .corner_radius(4.0)
                            .inner_margin(4.0)
                            .show(ui, |ui| {
                                ui.label(text);
                            });
                    }
                });
            });

        // Central Panel: Code Editor
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Assembly Source (Hex)");
            
            egui::ScrollArea::vertical().show(ui, |ui| {
                ui.add(
                    egui::TextEdit::multiline(&mut self.code)
                        .font(egui::TextStyle::Monospace)
                        .code_editor()
                        .desired_width(f32::INFINITY)
                        .desired_rows(25)
                        .lock_focus(true)
                );
            });
        });

        if self.running {
            self.cpu.tick();
            ctx.request_repaint();
        }
    }
}
