use crate::assembler::{Symbol, SymbolType, assemble};
use eframe::egui;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::components::memories::mem_64k::Mem64k;
use crate::cpu::{Flag, GPR, Z80A};
use crate::traits::{MemoryMapper, SyncronousComponent};

#[derive(Clone)]
enum HeaderAction {
    NewFile,
    OpenFileDialog,
    OpenFile(PathBuf),
    SaveFile,
    SaveFileAs,
    CloseTab(usize),
    Quit,
}

#[derive(Clone, Copy, PartialEq)]
enum ModalType {
    CloseTab(usize),
    Quit,
}

#[derive(serde::Deserialize, serde::Serialize, Clone)]
pub struct EditorTab {
    pub path: Option<PathBuf>,
    pub code: String,
    pub is_dirty: bool,
}

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct Z80App {
    #[serde(skip)]
    cpu: Z80A,
    #[serde(skip)]
    memory: Rc<RefCell<dyn MemoryMapper>>,

    tabs: Vec<EditorTab>,
    active_tab: usize,

    #[serde(skip)]
    last_error: Option<String>,
    #[serde(skip)]
    symbol_table: HashMap<String, Symbol>,
    #[serde(skip)]
    pending_modal: Option<ModalType>,

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
                match eframe::get_value::<Z80App>(storage, "z80_workspace") {
                    Some(mut app) => {
                        // Ensure at least one tab exists if something went wrong
                        if app.tabs.is_empty() {
                            app.tabs.push(EditorTab {
                                path: None,
                                code: Z80App::default_code(),
                                is_dirty: false,
                            });
                        }
                        if app.active_tab >= app.tabs.len() {
                            app.active_tab = 0;
                        }
                        app
                    }
                    None => Z80App::default(),
                }
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
    fn default_code() -> String {
        r"        ORG 0000h
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
        .to_string()
    }

    fn keybinds() -> Vec<(egui::Modifiers, egui::Key, HeaderAction)> {
        vec![
            (
                egui::Modifiers::COMMAND,
                egui::Key::S,
                HeaderAction::SaveFile,
            ),
            (
                egui::Modifiers::COMMAND | egui::Modifiers::SHIFT,
                egui::Key::S,
                HeaderAction::SaveFileAs,
            ),
            (
                egui::Modifiers::COMMAND,
                egui::Key::O,
                HeaderAction::OpenFileDialog,
            ),
        ]
    }

    fn check_shortcuts(&self, ctx: &egui::Context) -> Option<HeaderAction> {
        let mut action = None;
        ctx.input_mut(|i| {
            for (modifiers, key, act) in Self::keybinds() {
                if i.consume_key(modifiers, key) {
                    action = Some(act);
                    break;
                }
            }
        });
        action
    }

    fn load_and_reset(&mut self) {
        // Reset
        let memory: Rc<RefCell<dyn MemoryMapper>> = Rc::new(RefCell::new(Mem64k::new()));
        self.memory = memory.clone();
        self.cpu = Z80A::new(self.memory.clone());
        self.cpu.set_halted(true);

        if self.tabs.is_empty() {
            return;
        }

        let code = &self.tabs[self.active_tab].code;
        // Assemble and  Load code
        let (bytes, symbols, error) = match assemble(code) {
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

    fn new_file(&mut self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        self.tabs.push(EditorTab {
            path: None,
            code: Self::default_code(),
            is_dirty: false,
        });
        self.active_tab = self.tabs.len() - 1;
        self.save_to_storage(storage);
    }

    fn close_tab(&mut self, index: usize, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if index < self.tabs.len() {
            self.tabs.remove(index);
            if self.tabs.is_empty() {
                self.new_file(storage); // Ensure at least one tab
            } else {
                if self.active_tab >= self.tabs.len() {
                    self.active_tab = self.tabs.len() - 1;
                }
                self.save_to_storage(storage);
            }
        }
    }

    fn open_file(&mut self, path: PathBuf, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        // Check if already open
        if let Some(idx) = self
            .tabs
            .iter()
            .position(|t| t.path.as_ref() == Some(&path))
        {
            self.active_tab = idx;
            // Reload from disk? Maybe user wants to revert. For now just switch.
            self.save_to_storage(storage);
            return;
        }

        match std::fs::read_to_string(&path) {
            Ok(content) => {
                // If current tab is empty (default code or empty) and untitled, replace it
                let current_tab = &self.tabs[self.active_tab];
                let default_code = Self::default_code();
                let current_is_disposable = current_tab.path.is_none()
                    && (current_tab.code.trim().is_empty() || current_tab.code == default_code)
                    && !current_tab.is_dirty;

                if current_is_disposable {
                    self.tabs[self.active_tab] = EditorTab {
                        path: Some(path.clone()),
                        code: content,
                        is_dirty: false,
                    };
                } else {
                    self.tabs.push(EditorTab {
                        path: Some(path.clone()),
                        code: content,
                        is_dirty: false,
                    });
                    self.active_tab = self.tabs.len() - 1;
                }

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
        let tab = &mut self.tabs[self.active_tab];
        if let Some(path) = &tab.path {
            let path_clone = path.clone(); // Clone to avoid borrow check issues if we needed it later, but here write takes reference
            if let Err(err) = std::fs::write(&path_clone, &tab.code) {
                self.last_error = Some(format!("Failed to save file: {}", err));
            } else {
                tab.is_dirty = false;
            }
        } else {
            self.save_file_as(storage);
            return;
        }

        if let Some(path) = &self.tabs[self.active_tab].path {
            self.add_recent_file(path.clone());
        }
        self.save_to_storage(storage);
    }

    fn save_file_as(&mut self, storage: Option<&mut (dyn eframe::Storage + 'static)>) {
        if let Some(path) = rfd::FileDialog::new()
            .add_filter("Assembly", &["asm", "z80"])
            .save_file()
        {
            let tab = &mut self.tabs[self.active_tab];
            if let Err(err) = std::fs::write(&path, &tab.code) {
                self.last_error = Some(format!("Failed to save file: {}", err));
            } else {
                tab.path = Some(path.clone());
                tab.is_dirty = false;
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
            tabs: vec![EditorTab {
                path: None,
                code: Self::default_code(),
                is_dirty: false,
            }],
            active_tab: 0,
            last_error: None,
            symbol_table: HashMap::new(),
            pending_modal: None,
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
        // Handle window close request
        if ctx.input(|i| i.viewport().close_requested()) {
            let any_dirty = self.tabs.iter().any(|t| t.is_dirty);
            if any_dirty {
                ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
                self.pending_modal = Some(ModalType::Quit);
            }
        }

        let mut action = self.check_shortcuts(ctx);

        // Top Panel: Menu Bar and Control Toolbar
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // Menu Bar
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        action = Some(HeaderAction::NewFile);
                        ui.close();
                    }
                    if ui
                        .add(egui::Button::new("Open...").shortcut_text("Ctrl+O"))
                        .clicked()
                    {
                        action = Some(HeaderAction::OpenFileDialog);
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
                                action = Some(HeaderAction::OpenFile(path));
                                ui.close();
                            }
                        }
                    });

                    ui.separator();

                    if ui
                        .add(egui::Button::new("Save").shortcut_text("Ctrl+S"))
                        .clicked()
                    {
                        action = Some(HeaderAction::SaveFile);
                        ui.close();
                    }
                    if ui
                        .add(egui::Button::new("Save As...").shortcut_text("Ctrl+Shift+S"))
                        .clicked()
                    {
                        action = Some(HeaderAction::SaveFileAs);
                        ui.close();
                    }

                    ui.separator();
                    if ui.button("Quit").clicked() {
                        action = Some(HeaderAction::Quit);
                        ui.close();
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

        // Tab Bar Logic (pre-processing to find if we need to close tabs)
        // We'll define the closure later inside central panel?
        // No, let's process actions first. but Tab Bar is part of Central Panel UI.

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
            // Tab Bar
            ui.horizontal(|ui| {
                egui::ScrollArea::horizontal()
                    .id_salt("tabs_scroll")
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            let mut to_activate = None;
                            let mut to_close = None;

                            for (i, tab) in self.tabs.iter().enumerate() {
                                let name = tab
                                    .path
                                    .as_ref()
                                    .and_then(|p| p.file_name())
                                    .and_then(|n| n.to_str())
                                    .unwrap_or("Untitled");

                                let is_active = i == self.active_tab;

                                // Visuals
                                let bg_color = if is_active {
                                    ui.visuals().selection.bg_fill
                                } else {
                                    ui.visuals().faint_bg_color
                                };
                                let fg_color = if is_active {
                                    ui.visuals().selection.stroke.color
                                } else {
                                    ui.visuals().text_color()
                                };

                                let resp = egui::Frame::new()
                                    .fill(bg_color)
                                    .inner_margin(egui::Margin::symmetric(8, 4))
                                    .corner_radius(egui::CornerRadius {
                                        nw: 5,
                                        ne: 5,
                                        ..Default::default()
                                    })
                                    .stroke(egui::Stroke::new(
                                        1.0,
                                        ui.visuals().widgets.noninteractive.bg_stroke.color,
                                    ))
                                    .show(ui, |ui| {
                                        ui.horizontal(|ui| {
                                            if ui
                                                .add(
                                                    egui::Label::new(
                                                        egui::RichText::new(name).color(fg_color),
                                                    )
                                                    .sense(egui::Sense::click()),
                                                )
                                                .clicked()
                                            {
                                                to_activate = Some(i);
                                            }

                                            if tab.is_dirty {
                                                ui.add(
                                                    egui::Label::new(
                                                        egui::RichText::new("●").size(10.0),
                                                    )
                                                    .sense(egui::Sense::hover()),
                                                );
                                            }

                                            // Close button (x)
                                            if ui
                                                .add(
                                                    egui::Button::new(
                                                        egui::RichText::new("×").size(14.0),
                                                    )
                                                    .frame(false),
                                                )
                                                .clicked()
                                            {
                                                to_close = Some(i);
                                            }
                                        });
                                    });

                                if resp.response.clicked() {
                                    to_activate = Some(i);
                                }

                                ui.add_space(2.0);
                            }

                            if let Some(i) = to_activate {
                                self.active_tab = i;
                            }
                            if let Some(i) = to_close {
                                action = Some(HeaderAction::CloseTab(i));
                            }
                        });
                    });
            });
            ui.separator();

            ui.heading("Assembly Source");

            if self.active_tab < self.tabs.len() {
                let current_tab = &mut self.tabs[self.active_tab];

                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.horizontal_top(|ui| {
                        let num_lines = if current_tab.code.is_empty() {
                            1
                        } else {
                            current_tab.code.lines().count()
                                + if current_tab.code.ends_with('\n') {
                                    1
                                } else {
                                    0
                                }
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

                        if ui
                            .add(
                                egui::TextEdit::multiline(&mut current_tab.code)
                                    .font(egui::TextStyle::Monospace)
                                    .code_editor()
                                    .desired_width(f32::INFINITY)
                                    .desired_rows(25)
                                    .lock_focus(true),
                            )
                            .changed()
                        {
                            current_tab.is_dirty = true;
                        }
                    });
                });
            } else {
                ui.label("No files open");
            }
        });

        if let Some(action) = action {
            match action {
                HeaderAction::NewFile => self.new_file(frame.storage_mut()),
                HeaderAction::OpenFileDialog => self.open_file_dialog(frame.storage_mut()),
                HeaderAction::OpenFile(path) => self.open_file(path, frame.storage_mut()),
                HeaderAction::SaveFile => self.save_file(frame.storage_mut()),
                HeaderAction::SaveFileAs => self.save_file_as(frame.storage_mut()),
                HeaderAction::CloseTab(idx) => {
                    if self.tabs[idx].is_dirty {
                        self.pending_modal = Some(ModalType::CloseTab(idx));
                    } else {
                        self.close_tab(idx, frame.storage_mut());
                    }
                }
                HeaderAction::Quit => {
                    // This triggers on_close_event
                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                }
            }
        }

        // Handle Modal Dialogs
        if let Some(modal_type) = self.pending_modal {
            let mut open = true;
            let mut should_close_modal = false;

            // We use a centered window to simulate a modal
            egui::Window::new("Unsaved Changes")
                .collapsible(false)
                .resizable(false)
                .anchor(egui::Align2::CENTER_CENTER, [0.0, 0.0])
                .open(&mut open)
                .show(ctx, |ui| {
                    match modal_type {
                        ModalType::CloseTab(idx) => {
                            let name = self
                                .tabs
                                .get(idx)
                                .and_then(|t| t.path.as_ref())
                                .and_then(|p| p.file_name())
                                .and_then(|n| n.to_str())
                                .unwrap_or("Untitled");

                            ui.label(format!(
                                "Do you want to save the changes you made to {}?",
                                name
                            ));
                            ui.label("Your changes will be lost if you don't save them.");

                            ui.horizontal(|ui| {
                                if ui.button("Save").clicked() {
                                    // Hack: temporarily activate tab to save it, then restore?
                                    // Ideally save_file should take an index.
                                    // For now, let's just force sync active tab or refactor save_file.
                                    // Since save_file uses active_tab, let's swap active tab momentarily?
                                    let prev_active = self.active_tab;
                                    self.active_tab = idx;
                                    self.save_file(frame.storage_mut());
                                    self.active_tab = prev_active; // Restore (although closing tab will change it anyway)

                                    // Check if save succeeded (is_dirty false)
                                    if !self.tabs[idx].is_dirty {
                                        self.close_tab(idx, frame.storage_mut());
                                        should_close_modal = true;
                                    }
                                }
                                if ui.button("Don't Save").clicked() {
                                    if idx < self.tabs.len() {
                                        self.tabs[idx].is_dirty = false; // Forced clear
                                        self.close_tab(idx, frame.storage_mut());
                                    }
                                    should_close_modal = true;
                                }
                                if ui.button("Cancel").clicked() {
                                    should_close_modal = true;
                                }
                            });
                        }
                        ModalType::Quit => {
                            ui.label("You have unsaved changes in your workspace.");
                            ui.label("Do you want to save all changes before quitting?");

                            ui.horizontal(|ui| {
                                if ui.button("Save All").clicked() {
                                    // Iterate and save all dirty tabs
                                    let prev_active = self.active_tab;
                                    for i in 0..self.tabs.len() {
                                        if self.tabs[i].is_dirty {
                                            self.active_tab = i;
                                            self.save_file(frame.storage_mut());
                                        }
                                    }
                                    self.active_tab = prev_active;

                                    // If all clear, close
                                    if !self.tabs.iter().any(|t| t.is_dirty) {
                                        // Force close by clearing modal first to prevent loop
                                        should_close_modal = true;
                                        self.pending_modal = None; // clear immediately
                                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                                    }
                                }
                                if ui.button("Quit Without Saving").clicked() {
                                    // Clear all dirty flags to bypass on_close_event check
                                    for tab in &mut self.tabs {
                                        tab.is_dirty = false;
                                    }
                                    should_close_modal = true;
                                    self.pending_modal = None;
                                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                                }
                                if ui.button("Cancel").clicked() {
                                    should_close_modal = true;
                                }
                            });
                        }
                    }
                });

            if !open || should_close_modal {
                self.pending_modal = None;
            }
        }

        if !self.cpu.is_halted() {
            while !self.cpu.is_halted() {
                self.cpu.tick();
            }
            ctx.request_repaint();
        }
    }
}
