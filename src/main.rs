mod components;
mod cpu;
mod traits;
mod gui;
mod assembler;


#[cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() -> eframe::Result<()> {
    gui::run()
}
