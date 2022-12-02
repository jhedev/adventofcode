use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let reader = read_lines("input.txt");
    let lines = reader.collect()::<Result<_,_>>().unwrap();
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
