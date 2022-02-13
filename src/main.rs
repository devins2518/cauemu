use sdl2::{event::Event, pixels::Color};
use std::{error::Error, time::Duration};

mod cpu;

const HEIGHT: u32 = 160;
const WIDTH: u32 = 240;

fn main() -> Result<(), Box<dyn Error>> {
    let mut cpu = cpu::Arm7TDMI::new();
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let window = video_subsystem
        .window("cauemu", WIDTH, HEIGHT)
        .position_centered()
        .build()?;

    let mut canvas = window.into_canvas().build()?;
    let texture_creator = canvas.texture_creator();

    let mut event_pump = sdl_context.event_pump()?;
    let mut i = 0;
    'running: loop {
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                _ => {}
            }
        }
        // The rest of the game loop goes here...

        canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    loop {
        cpu.clock()
    }
}
