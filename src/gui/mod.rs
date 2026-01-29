use crate::assembler::{Symbol, SymbolType, assemble};
use eframe::egui;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::components::memories::mem_64k::Mem64k;
use crate::cpu::{Flag, GPR, Z80A};
use crate::traits::{MemoryMapper, SyncronousComponent};

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct Z80App {
    #[serde(skip)]
    cpu: Z80A,
    #[serde(skip)]
    memory: Rc<RefCell<dyn MemoryMapper>>,

    code: String,
    
    #[serde(skip)]
    last_error: Option<String>,
    #[serde(skip)]
    symbol_table: HashMap<String, Symbol>,
    
    // File management
    current_file_path: Option<PathBuf>,
    recent_files: Vec<PathBuf>,
}
pub fn run() -> eframe::Result<()> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_app_id("z80-simulator")
            .with_inner_size([1280.0, 720.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Z80 Simulator",
        options,
        Box::new(|cc| {
            let mut app = if let Some(storage) = cc.storage {
                eframe::get_value(storage, "z80_workspace").unwrap_or_default()
            } else {
                Z80App::default()
            };
            
            // Re-initialize non-serialized fields
            app.load_and_reset();
            Ok(Box::new(app))
        }),
    )
}

impl Z80App {
    fn load_and_reset(&mut self) {
        // Reset
        let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
        self.memory = memory.clone();
        self.cpu = Z80A::new(self.memory.clone());
        self.cpu.set_halted(true);

        // Assemble and  Load code
        let (bytes, symbols, error) = match assemble(&self.code) {
            Ok((b, s)) => (b, s, None),
            Err(e) => (Vec::new(), HashMap::new(), Some(e)),
        };

        self.symbol_table = symbols;

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

    fn save_to_storage(&self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if let Some(storage) = storage {
            eframe::set_value(storage, "z80_workspace", self);
            storage.flush();
        }
    }

    fn open_file_dialog(&mut self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if let Some(path) = rfd::FileDialog::new()
            .add_filter("Assembly", &["asm", "z80"])
            .add_filter("All files", &["*"])
            .pick_file()
        {
            self.open_file(path, storage);
        }
    }

    fn open_file(&mut self, path: PathBuf, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        match std::fs::read_to_string(&path) {
            Ok(content) => {
                self.code = content;
                self.current_file_path = Some(path.clone());
                self.add_recent_file(path);
                self.load_and_reset();
                self.save_to_storage(storage);
            }
            Err(err) => {
                self.last_error = Some(format!("Failed to open file: {}", err));
            }
        }
    }

    fn save_file(&mut self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if let Some(path) = &self.current_file_path {
            if let Err(err) = std::fs::write(path, &self.code) {
                self.last_error = Some(format!("Failed to save file: {}", err));
            }
        } else {
            self.save_file_as(storage);
            return;
        }
        if let Some(path) = &self.current_file_path {
            self.add_recent_file(path.clone());
        }
        self.save_to_storage(storage);
    }

    fn save_file_as(&mut self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if let Some(path) = rfd::FileDialog::new()
            .add_filter("Assembly", &["asm", "z80"])
            .save_file()
        {
            if let Err(err) = std::fs::write(&path, &self.code) {
                self.last_error = Some(format!("Failed to save file: {}", err));
            } else {
                self.current_file_path = Some(path.clone());
                self.add_recent_file(path);
                self.save_to_storage(storage);
            }
        }
    }

    fn add_recent_file(&mut self, path: PathBuf) {
        // Remove if exists to move it to the top
        println!("Adding recent file: {:?}", path);
        self.recent_files.retain(|p| p != &path);
        self.recent_files.insert(0, path);
        
        // Keep only last 10
        if self.recent_files.len() > 10 {
            self.recent_files.truncate(10);
        }
    }
}

impl Default for Z80App {
    fn default() -> Self {
        let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
        let cpu = Z80A::new(memory.clone());

        Self {
            cpu,
            memory,
            code: r"        ORG 0000h
        JP START

; ----------------------------
; Data
; ----------------------------

; Write your variables here

; ----------------------------
; Code
; ----------------------------
START:
; Write your code here:
"
            .to_string(),
            last_error: None,
            symbol_table: HashMap::new(),
            current_file_path: None,
            recent_files: Vec::new(),
        }
    }
}

impl eframe::App for Z80App {
    /// Called by the frame work to save state before shutdown.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, "z80_workspace", self);
    }

    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        enum Action {
            OpenFileDialog,
            OpenFile(PathBuf),
            SaveFile,
            SaveFileAs,
        }
        let mut action = None;

        // Top Panel: Menu Bar and Control Toolbar
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // Menu Bar
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open...").clicked() {
                        action = Some(Action::OpenFileDialog);
                        ui.close();
                    }
                    
                    ui.menu_button("Open Recent", |ui| {
                        if self.recent_files.is_empty() {
                            ui.label("No recent files");
                        } else {
                             let mut to_open = None;
                            for path in &self.recent_files {
                                if ui.button(path.display().to_string()).clicked() {
                                    to_open = Some(path.clone());
                                }
                            }
                            if let Some(path) = to_open {
                                action = Some(Action::OpenFile(path));
                                ui.close();
                            }
                        }
                    });

                    ui.separator();

                    if ui.button("Save").clicked() {
                        action = Some(Action::SaveFile);
                        ui.close();
                    }
                    if ui.button("Save As...").clicked() {
                        action = Some(Action::SaveFileAs);
                        ui.close();
                    }
                    
                    ui.separator();
                     if ui.button("Quit").clicked() {
                         ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                     }
                });
            });
            ui.separator();
            
            // Toolbar
            ui.horizontal(|ui| {
                if ui.button("⟳ Load & Reset").clicked() {
                    self.load_and_reset();
                }

                ui.separator();

                if ui
                    .add_enabled(self.last_error.is_none(), egui::Button::new("⏭ Step"))
                    .clicked()
                {
                    if self.cpu.is_halted() {
                        self.cpu.set_halted(false);
                    }
                    self.cpu.tick();
                    self.cpu.set_halted(true);
                }

                let run_label = if !self.cpu.is_halted() {
                    "⏸ Stop"
                } else {
                    "▶ Run"
                };
                if ui
                    .add_enabled(self.last_error.is_none(), egui::Button::new(run_label))
                    .clicked()
                {
                    self.cpu.set_halted(!self.cpu.is_halted());
                }

                ui.separator();

                if let Some(err) = &self.last_error {
                    ui.colored_label(egui::Color32::RED, format!("⚠ {}", err));
                } else if self.cpu.is_halted() {
                    ui.colored_label(egui::Color32::RED, "HALTED");
                } else {
                    ui.label(egui::RichText::new("Ready").color(egui::Color32::GREEN));
                }
            });
        });

        if let Some(action) = action {
            match action {
                Action::OpenFileDialog => self.open_file_dialog(frame.storage_mut()),
                Action::OpenFile(path) => self.open_file(path, frame.storage_mut()),
                Action::SaveFile => self.save_file(frame.storage_mut()),
                Action::SaveFileAs => self.save_file_as(frame.storage_mut()),
            }
        }

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

                        ui.label("PC");
                        ui.label(mono(format!("0x{:04X}", pc)));
                        ui.end_row();
                        ui.label("SP");
                        ui.label(mono(format!("0x{:04X}", sp)));
                        ui.end_row();
                        ui.label("IX");
                        ui.label(mono(format!("0x{:04X}", ix)));
                        ui.end_row();
                        ui.label("IY");
                        ui.label(mono(format!("0x{:04X}", iy)));
                        ui.end_row();

                        ui.separator();
                        ui.separator();
                        ui.end_row();

                        ui.label("A");
                        ui.label(mono(format!("0x{:02X}", reg_a)));
                        ui.end_row();
                        ui.label("F");
                        ui.label(mono(format!("0x{:02X}", reg_f)));
                        ui.end_row();
                        ui.label("B");
                        ui.label(mono(format!("0x{:02X}", reg_b)));
                        ui.end_row();
                        ui.label("C");
                        ui.label(mono(format!("0x{:02X}", reg_c)));
                        ui.end_row();
                        ui.label("D");
                        ui.label(mono(format!("0x{:02X}", reg_d)));
                        ui.end_row();
                        ui.label("E");
                        ui.label(mono(format!("0x{:02X}", reg_e)));
                        ui.end_row();
                        ui.label("H");
                        ui.label(mono(format!("0x{:02X}", reg_h)));
                        ui.end_row();
                        ui.label("L");
                        ui.label(mono(format!("0x{:02X}", reg_l)));
                        ui.end_row();
                    });

                ui.add_space(20.0);
                ui.heading("Flags");
                ui.separator();

                ui.horizontal_wrapped(|ui| {
                    ui.spacing_mut().item_spacing.x = 10.0;
                    for (flag, label) in [
                        (Flag::S, "S"),
                        (Flag::Z, "Z"),
                        (Flag::Y, "Y"),
                        (Flag::H, "H"),
                        (Flag::X, "X"),
                        (Flag::PV, "PV"),
                        (Flag::N, "N"),
                        (Flag::C, "C"),
                    ] {
                        let on = self.cpu.get_flag(flag);
                        let color = if on {
                            egui::Color32::from_rgb(0, 255, 0)
                        } else {
                            egui::Color32::from_rgb(60, 60, 60)
                        };
                        let text_color = if on {
                            egui::Color32::BLACK
                        } else {
                            egui::Color32::WHITE
                        };

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

        let mut sorted_symbols: Vec<_> = self.symbol_table.iter().collect();
        sorted_symbols.sort_by_key(|item| item.1.address);

        egui::SidePanel::right("vars_panel")
            .resizable(true)
            .default_width(280.0)
            .show(ctx, |ui| {
                ui.heading("Variables");
                ui.separator();

                egui::ScrollArea::vertical()
                    .id_salt("vars_scroll")
                    .max_height(200.0)
                    .show(ui, |ui| {
                        egui::Grid::new("vars_grid")
                            .striped(true)
                            .spacing([20.0, 4.0])
                            .show(ui, |ui| {
                                ui.label(egui::RichText::new("Name").strong());
                                ui.label(egui::RichText::new("Addr").strong());
                                ui.label(egui::RichText::new("Value").strong());
                                ui.end_row();

                                for (name, symbol) in &sorted_symbols {
                                    if symbol.kind == SymbolType::Label {
                                        continue;
                                    }

                                    let addr = symbol.address;

                                    ui.label(*name);
                                    ui.monospace(format!("0x{:04X}", addr));

                                    match symbol.kind {
                                        SymbolType::Byte => {
                                            let val = self.memory.borrow().read(addr);
                                            ui.monospace(format!("0x{:02X}", val));
                                        }
                                        SymbolType::Word => {
                                            let low = self.memory.borrow().read(addr);
                                            let high =
                                                self.memory.borrow().read(addr.wrapping_add(1));
                                            let val = (low as u16) | ((high as u16) << 8);
                                            ui.monospace(format!("0x{:04X}", val));
                                        }
                                        _ => {}
                                    }
                                    ui.end_row();
                                }
                            });
                    });

                ui.add_space(20.0);
                ui.heading("Labels");
                ui.separator();

                egui::ScrollArea::vertical()
                    .id_salt("labels_scroll")
                    .show(ui, |ui| {
                        egui::Grid::new("labels_grid")
                            .striped(true)
                            .spacing([20.0, 4.0])
                            .show(ui, |ui| {
                                ui.label(egui::RichText::new("Name").strong());
                                ui.label(egui::RichText::new("Addr").strong());
                                ui.end_row();

                                for (name, symbol) in &sorted_symbols {
                                    if symbol.kind != SymbolType::Label {
                                        continue;
                                    }
                                    let addr = symbol.address;

                                    ui.label(*name);
                                    ui.monospace(format!("0x{:04X}", addr));
                                    ui.end_row();
                                }
                            });
                    });
            });

        // Central Panel: Code Editor
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Assembly Source");

            egui::ScrollArea::vertical().show(ui, |ui| {
                ui.horizontal_top(|ui| {
                    let num_lines = if self.code.is_empty() {
                        1
                    } else {
                        self.code.lines().count() + if self.code.ends_with('\n') { 1 } else { 0 }
                    };

                    let line_numbers = (1..=num_lines)
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join("\n");

                    ui.add(egui::Label::new(
                        egui::RichText::new(line_numbers)
                            .text_style(egui::TextStyle::Monospace)
                            .color(egui::Color32::GRAY),
                    ));

                    ui.add(
                        egui::TextEdit::multiline(&mut self.code)
                            .font(egui::TextStyle::Monospace)
                            .code_editor()
                            .desired_width(f32::INFINITY)
                            .desired_rows(25)
                            .lock_focus(true),
                    );
                });
            });
        });

        if !self.cpu.is_halted() {
            while !self.cpu.is_halted() {
                self.cpu.tick();
            }
            ctx.request_repaint();
        }
    }
}
