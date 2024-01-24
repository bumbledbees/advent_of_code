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
        let rec process_lines (accumulator: string list) : string list =
            try 
                let line = input_line input_file in

                (* do stuff here *)

                process_lines (line :: accumulator)
            with
                End_of_file -> accumulator
        in
        let lines = process_lines [] in

        (* ...and here *)

        close_in input_file
    with e ->
        close_in_noerr input_file;
        raise e
