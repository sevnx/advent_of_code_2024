let file = "inputs/day1.txt"

let split_once_whitespace str = 
    let re = Str.regexp "[ \t\r\n]" in Str.bounded_split re str 2

let get_two_numbers str_list = 
    match str_list with
    |first::second::_ -> int_of_string (String.trim first), int_of_string (String.trim second)
    | _ -> raise (Failure "Error")

let list channel =
    let rec aux channel left right = 
        match input_line channel with
        | line -> let (first, last) = 
            get_two_numbers (split_once_whitespace line) in 
            aux channel (first :: left) (last :: right)
        | exception End_of_file -> List.rev left, List.rev right
    in
    aux channel [] []

let diff_abs a b = 
    abs (a - b)

let () = 
    let ic = open_in file in
    let (first, last) = list ic in
    let (sorted_first, sorted_last) = List.sort compare first, List.sort compare last in
    let rec sum left right acc =
        match left, right with
            | [], [] -> acc
            | left_first :: left_rest, right_first :: right_rest -> sum left_rest right_rest (acc + (diff_abs left_first right_first))
            | [], _::_ -> raise (Failure "Uneven")
            | _::_, [] -> raise (Failure "Uneven")
    in
    let length_sum = sum sorted_first sorted_last 0 in
    Printf.printf "%d\n" length_sum

