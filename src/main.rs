mod cpu;

fn main() {
    let mut cpu = cpu::Arm7TDMI::new();
    cpu.init();

    loop {
        cpu.clock()
    }
}
