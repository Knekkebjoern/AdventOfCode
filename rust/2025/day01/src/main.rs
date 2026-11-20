use std::fs;

fn main() {
    let data = fs::read_to_string("data/input.txt");
    println!("{}", data);
}
