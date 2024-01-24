type card =
    | Joker
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type score =
    | High_card
    | One_pair
    | Two_pair
    | Three_of_a_kind
    | Full_house
    | Four_of_a_kind
    | Five_of_a_kind

type game = {
    hand: card list;
    bet: int;
    score: score;
}

exception ParseError of string

let show_card (c: card) : string =
    match c with
    | Joker -> "J"
    | Two   -> "2"
    | Three -> "3"
    | Four  -> "4"
    | Five  -> "5"
    | Six   -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine  -> "9"
    | Ten   -> "T"
    | Jack  -> "J"
    | Queen -> "Q"
    | King  -> "K"
    | Ace   -> "A"

let show_score (s: score) : string =
    match s with
    | High_card -> "High card"
    | One_pair -> "One pair"
    | Two_pair -> "Two pair"
    | Three_of_a_kind -> "Three of a kind"
    | Full_house -> "Full house"
    | Four_of_a_kind -> "Four of a kind"
    | Five_of_a_kind -> "Five of a kind"

let show_hand (hand: card list) : string =
    let accumulate str c =  str ^ (show_card c) in
    List.fold_left accumulate "" hand

let show_game (g: game) : string =
    let hand = show_hand g.hand in
    let bet = string_of_int g.bet in
    let score = show_score g.score in
    "Hand: " ^ hand ^ " | Bet: " ^ bet ^ " | Score: " ^ score

let rec print_games (games: game list) =
    match games with
    | [] -> ()
    | g :: games' ->
        print_endline (show_game g);
        flush stdout;
        print_games games'

let rec ordered_insert (item: 'a) (lst: 'a list) : 'a list =
    match lst with
    | x :: lst' when item >= x -> x :: (ordered_insert item lst')
    | x :: lst' -> item :: x :: lst'
    | [] -> item :: []

let sort_hand (hand: card list) : card list = 
    let rec sort (sorted: card list) (h: card list) : card list =
        match h with
        | c :: h' -> sort (ordered_insert c sorted) h'
        | [] -> sorted
    in
    sort [] hand

let hand_score (hand: card list) : score =
    let sorted = sort_hand hand in
    let rec next_grouping (acc: card list) (hand: card list) =
        match hand with
        | c :: hand' ->
            begin
                match acc with
                    | [] -> next_grouping (c :: []) hand'
                    | fst :: _ when fst == c -> next_grouping (c :: acc) hand'
                    | _ -> (acc, hand)
            end
        | [] -> (acc, hand)
    in
    let rec score (h: card list) =
        let (grouping, remainder) = next_grouping [] h in
        let num_jokers =
            match List.hd grouping with
            | Joker -> List.length grouping
            | _ -> 0
        in
        if num_jokers > 0 then
            match num_jokers with
            | 4 | 5 -> Five_of_a_kind
            | 3 ->
                begin
                    match score remainder with
                    | One_pair -> Five_of_a_kind
                    | _ -> Four_of_a_kind
                end
            | 2 ->
                begin
                    match score remainder with
                    | Three_of_a_kind -> Five_of_a_kind
                    | One_pair -> Four_of_a_kind
                    | _ -> Three_of_a_kind
                end
            | 1 ->
                begin
                    match score remainder with
                    | Four_of_a_kind -> Five_of_a_kind
                    | Three_of_a_kind -> Four_of_a_kind
                    | Two_pair -> Full_house
                    | One_pair -> Three_of_a_kind
                    | _ -> One_pair
                end
            | _ -> score remainder
        else
            match List.length grouping with
            | 5 -> Five_of_a_kind
            | 4 -> Four_of_a_kind
            | 3 when List.length remainder > 0 ->
                begin
                    match score remainder with
                    | One_pair -> Full_house
                    | _ -> Three_of_a_kind
                end
            | 3 -> Three_of_a_kind
            | 2 when List.length remainder > 0 ->
                begin
                    match score remainder with
                    | Three_of_a_kind -> Full_house
                    | One_pair -> Two_pair
                    | _ -> One_pair
                end
            | 2 -> One_pair
            | 1 when List.length remainder > 0 -> score remainder
            | 1 -> High_card
            | _ -> raise (ParseError "error parsing score")
    in
    score sorted

let compare_games (a: game) (b: game) : int =
    let rec cmp_at i =
        if i == 5 then 0
        else
            match (List.nth a.hand i, List.nth b.hand i) with
            | a, b when a == b -> cmp_at (i + 1)
            | a, b when a > b -> 1
            | a, b when a < b -> -1
            | _, _ -> 0
    in
    if a.score == b.score then
        cmp_at 0
    else
        if a.score > b.score then 1 else -1

let parse_card (ch: char) : card =
    match ch with
    | 'J' -> Joker
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
 (* | 'J' -> Jack *)
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _   -> raise (ParseError ("Unknown card: " ^ String.make 1 ch))

let rec parse_cards_ (hand: char list) (cards: card list) : card list =
    let card = match hand with
    | [] -> None
    | ch :: _ -> Some (parse_card ch) in
    match card with
    | Some c -> 
        begin
            match hand with
            | _ :: hand' -> parse_cards_ hand' (c :: cards)
            | [] -> c :: []
        end
    | None -> cards

let parse_cards (hand: char list) (cards: card list) : card list =
    let backwards_hand = parse_cards_ hand cards in
    List.rev backwards_hand

let parse (line_: string) : game =
    let line = line_ |> String.to_seq |> List.of_seq in
    let hand_ = List.filteri (fun i _ -> i < 5) line in
    let hand = parse_cards hand_ [] in
    let bet_ = (List.filteri (fun i _ -> i >= 6) line) in
    let bet = bet_ |> List.to_seq |> String.of_seq in
    { hand = hand;
      bet = int_of_string bet; 
      score = hand_score hand; }

let () =
    let quit_error msg =
        Printf.eprintf "Error: %s\n" msg;
        exit 1
    in

    let args = Sys.argv in
    if Array.length args < 2 then quit_error "Please provide an input file.";

    let input_filepath = args.(1) in
    let input_file = open_in input_filepath in
    try
        let rec parse_lines (games: game list) : game list =
            try
                let line = input_line input_file in
                parse_lines ((parse line) :: games)
            with
                End_of_file -> games
        in
        let games = List.rev (parse_lines []) in
        let sorted = List.sort compare_games games in
        let rec accumulate (i: int) (winnings: int) (games: game list) : int =
            match games with
            | [] -> winnings
            | g :: games' -> accumulate (i + 1) (i * g.bet + winnings) games'
        in
        (* Alter comments in `parse_line` to activate/deactivate joker rule *)
        Printf.printf "Total winnings: %d\n" (accumulate 1 0 sorted);
        close_in input_file
    with e ->
        close_in_noerr input_file;
        raise e
