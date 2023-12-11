use std::env::args;
use std::fs::File;
use std::io::{BufReader, BufRead, ErrorKind};
use std::process::ExitCode;


fn error(msg: String) -> ExitCode {
    eprintln!("Error: {}", msg);
    ExitCode::FAILURE
}


fn main() -> ExitCode {
    let args: Vec<String> = args().collect();
    if args.len() <= 1 {
        return error(String::from("Please specify an input file."));
    }
    
    let input_filepath = &args[1];
    let input_file = match File::open(input_filepath) {
        Ok(file) => file,
        Err(err) => return match err.kind() {
            ErrorKind::NotFound => error(format!("File not found: \"{}\"",
                                                 input_filepath)),
            _ => error(err.to_string()), 
        }
    };

    let lines = BufReader::new(input_file).lines();
    for line in lines {
        let line = match line {
            Ok(l) => l,
            Err(err) => return error(err.to_string()),
        };
        // do stuff here
        println!("{}", line);
    }

    ExitCode::SUCCESS
}
