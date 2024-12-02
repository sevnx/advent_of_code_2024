let file = "inputs/day2.txt"

type order = Ascending | Descending

let get_ordering first second =
  match first with
  | Some first -> if first > second then Some Descending else Some Ascending
  | None -> None

let diff_abs a b = abs (a - b)

let is_diff_incorrect a b =
  let diff = diff_abs a b in
  if diff > 3 || diff == 0 then true else false

let is_line_safe line =
  let numbers = List.map int_of_string (String.split_on_char ' ' line) in
  let rec loop last_number rest ascending_type has_one_failure =
    match rest with
    | [] -> true
    | first :: rest -> (
        let ordering = get_ordering last_number first in
        let ascending_type =
          match ascending_type with
          | Some Ascending -> Some Ascending
          | Some Descending -> Some Descending
          | None -> if last_number = None then None else ordering
        in
        match ascending_type with
        | Some value ->
            if Option.get ordering <> value then
              if has_one_failure then false
              else loop (Some first) rest ascending_type true
            else if is_diff_incorrect (Option.get last_number) first then
              if has_one_failure then false
              else loop (Some first) rest ascending_type true
            else loop (Some first) rest ascending_type has_one_failure
        | None -> loop (Some first) rest ascending_type has_one_failure)
  in
  loop None numbers None false

let () =
  let ic = open_in file in

  let rec aux channel acc =
    match input_line channel with
    | line -> aux channel (acc + if is_line_safe line then 1 else 0)
    | exception End_of_file -> acc
  in
  let sum = aux ic 0 in
  Printf.printf "%d\n" sum
