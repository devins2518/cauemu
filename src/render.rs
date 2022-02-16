use crate::{cpu::Arm7TDMI, HEIGHT, WIDTH};
use sdl2::{render::WindowCanvas, VideoSubsystem};
use std::error::Error;

pub struct Renderer {
    pub window: WindowCanvas,
    pub instr_window: WindowCanvas,
}

impl Renderer {
    pub fn new(v_subsystem: &VideoSubsystem) -> Result<Renderer, Box<dyn Error>> {
        let window = v_subsystem
            .window("cauemu", WIDTH, HEIGHT)
            .position_centered()
            .build()?
            .into_canvas()
            .build()?;

        let instr_window = v_subsystem
            .window("Instructions", WIDTH * 2, HEIGHT * 4)
            .position(0, 0)
            .build()?
            .into_canvas()
            .build()?;

        Ok(Self {
            window,
            instr_window,
        })
    }
}
