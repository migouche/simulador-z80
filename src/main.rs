mod assembler;
mod components;
mod cpu;
mod gui;
mod traits;
mod ui_traits;

#[cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() -> eframe::Result<()> {
    gui::run()
}
