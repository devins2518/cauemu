mod cpu;

fn main() {
    let mut cpu = cpu::Arm7TDMI::new();

    loop {
        cpu.clock()
    }
}
