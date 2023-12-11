use std::env::args;
use std::fs::File;
use std::io::{BufReader, BufRead, ErrorKind};


// values represent some number of cubes of a specified color
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
enum Color { Red(usize), Green(usize), Blue(usize) }

type Round = Vec<Color>;

#[derive(Debug)]
struct Game { number: usize, rounds: Vec<Round> }

type CubeSet = (Color, Color, Color);


fn main() {
    let args: Vec<String> = args().collect();
    if args.len() <= 1 {
        panic!("Please specify an input file.");
    }
    
    let input_filepath: &String = &args[1];
    let input_file: File = match File::open(input_filepath) {
        Ok(file) => file,
        Err(err) => panic!("{}", match err.kind() {
            ErrorKind::NotFound => format!("File not found: \"{}\"",
                                           input_filepath),
            _ => err.to_string()
        })
    };

    let lines = BufReader::new(input_file).lines();
    let mut games = Vec::<Game>::new();
    for line in lines {
        let line: String = match line {
            Ok(l) => l,
            Err(err) => panic!("{}", err.to_string()),
        };
        let mut line_words = line.split_whitespace();
        let mut game = Game { number: 0, rounds: Vec::<Round>::new() };
        loop {
            match line_words.next() {
                // current word is the first word, "Game".
                // the next word will be the game #, followed by a colon.
                Some("Game") => {
                    // instantiate empty array for the first round
                    game.rounds.push(Round::new());
                    game.number = match line_words.next() {
                        Some(s) => s[0..s.len()-1].parse::<usize>().unwrap(),
                        None => break  // EOL
                    };
                },
                // otherwise, we assume current word is a number of cubes and
                // that the next word will tell us which color.
                Some(s) => match s.parse::<usize>() {
                    Ok(n) => match line_words.next() {
                        Some(s) => {
                            let cube: Color = match &s[0..3] {
                                "red" => Color::Red(n),
                                "gre" => Color::Green(n),
                                "blu" => Color::Blue(n),
                                _ => panic!("Unknown color: {}", s)
                            };

                            let current_round = game.rounds.len() - 1;
                            game.rounds[current_round].push(cube);

                            // if after the cube color, there's a semicolon,
                            // we "start a new round"
                            if s.chars().nth(s.len() - 1).unwrap() == ';' {
                                game.rounds.push(Round::new());
                            }
                        },
                        None => break  // EOL
                    },
                    Err(e) => panic!("{}", e.to_string())
                },
                None => break  // EOL
            }
        };
        games.push(game);
    }

    fn proper_count(cube: &Color) -> bool {
        match cube {
            Color::Red(n) if *n > 12 => false,
            Color::Green(n) if *n > 13 => false,
            Color::Blue(n) if *n > 14 => false,
            _ => true
        }
    }
    println!(
        "Sum of possible games: {}",
        games.iter().fold(
            0,
            |sum, game| {
                sum + (if game.rounds.iter().flatten().all(proper_count)
                       {game.number} else {0})
            }
        )
    );

    // Part 2
    let mut min_cubes_per_round = Vec::<CubeSet>::new();
    for game in games {
        let mut min_cubes = (Color::Red(0), Color::Green(0), Color::Blue(0));
        for cubes in game.rounds.iter().flatten() {
            match cubes {
                Color::Red(_) if *cubes > min_cubes.0 => min_cubes.0 = *cubes,
                Color::Green(_) if *cubes > min_cubes.1 => min_cubes.1 = *cubes,
                Color::Blue(_) if *cubes > min_cubes.2 => min_cubes.2 = *cubes,
                _ => (),
            };
        }
        min_cubes_per_round.push(min_cubes)
    }
    fn power(set: CubeSet) -> usize {
        let mut power = 1;
        if let Color::Red(n) = set.0 { power *= n; }
        if let Color::Green(n) = set.1 { power *= n; }
        if let Color::Blue(n) = set.2 { power *= n; }
        power
    }
    println!(
        "Sum of the power of the set of the minimum number of cubes/game: {}",
        min_cubes_per_round.iter().fold(
            0,
            |sum, set| { sum + power(*set) }
        )
    );
}
