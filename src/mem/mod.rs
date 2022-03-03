pub trait Memory {
    fn read(&self, addr: u32) -> u8;
    fn write(&mut self, addr:u32, n:u8) {}
}
