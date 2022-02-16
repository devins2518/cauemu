use crate::{cpu::Arm7TDMI, render::Renderer};
use sdl2::{event::Event, Sdl};
use std::error::Error;

pub struct GameboyAdvance {
    cpu: Arm7TDMI,
    renderer: Renderer,
}

impl GameboyAdvance {
    pub fn new(sdl: &Sdl) -> Result<Self, Box<dyn Error>> {
        let cpu = Arm7TDMI::new();
        let renderer = Renderer::new(&sdl.video()?)?;

        Ok(Self { cpu, renderer })
    }

    pub fn tick(&mut self) {
        self.cpu.render();
    }

    pub fn handle_event(&mut self, event: Event) -> Option<()> {
        match event {
            Event::Quit { .. } => None,
            _ => Some(()),
        }
    }
}
