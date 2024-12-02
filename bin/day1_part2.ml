let file = "inputs/day1.txt"

let split_once_whitespace str =
  let re = Str.regexp "[ \t\r\n]" in
  Str.bounded_split re str 2

let get_two_numbers str_list =
  match str_list with
  | first :: second :: _ ->
      (int_of_string (String.trim first), int_of_string (String.trim second))
  | _ -> raise (Failure "Error")

module IntMap = Map.Make (Int)

let update_or_insert map key =
  match map |> IntMap.mem key with
  | true -> map |> IntMap.update key (Option.map (fun old -> old + 1))
  | false -> map |> IntMap.add key 1

let list channel =
  let rec aux channel left right =
    match input_line channel with
    | line ->
        let first, last = get_two_numbers (split_once_whitespace line) in
        aux channel (update_or_insert left first) (update_or_insert right last)
    | exception End_of_file -> (left, right)
  in
  aux channel IntMap.empty IntMap.empty

let () =
  let ic = open_in file in
  let first, last = list ic in
  let sum =
    IntMap.fold
      (fun key value acc ->
        acc
        + (key * value * Option.value ~default:0 (last |> IntMap.find_opt key)))
      first 0
  in
  Printf.printf "%d\n" sum
