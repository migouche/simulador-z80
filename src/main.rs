
mod alu;

use alu::types::Byte;

fn main() {
    println!("Hello, world!");
    
    let mut byte = Byte::new(42);
    println!("Byte value: {}", byte.get());
    
    byte.set(100);
    println!("New byte value: {}", byte.get());
}
