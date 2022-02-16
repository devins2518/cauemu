use std::{error::Error, time::Duration};

mod cpu;
mod gba;
mod mem;
mod render;

pub const HEIGHT: u32 = 160;
pub const WIDTH: u32 = 240;

fn main() -> Result<(), Box<dyn Error>> {
    let sdl = sdl2::init()?;
    let mut gameboy = gba::GameboyAdvance::new(&sdl)?;

    let mut event_pump = sdl.event_pump()?;
    'main: loop {
        gameboy.tick();
        for event in event_pump.poll_iter() {
            if let None = gameboy.handle_event(event) {
                break 'main;
            }
        }
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
    Ok(())
}
