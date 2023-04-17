let file = "input.txt"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let lines = read_lines file

(* Start of Day 02 - Part 1 *)

type move = Rock | Paper | Scissors
type result = Win | Lose | Draw

let result_to_points result =
  match result with Win -> 6 | Lose -> 0 | Draw -> 3

let move_to_points move =
  match move with Rock -> 1 | Paper -> 2 | Scissors -> 3

let convert_to_move s =
  match s with
  | 'X' | 'A' -> Rock
  | 'Y' | 'B' -> Paper
  | 'Z' | 'C' -> Scissors
  | _ -> failwith "Invalid move"

let convert_to_result s =
  match s with
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "Invalid result"

let move_to_play move expected_result =
  match (move, expected_result) with
  | Rock, Win -> Paper
  | Rock, Lose -> Scissors
  | Rock, Draw -> Rock
  | Paper, Win -> Scissors
  | Paper, Lose -> Rock
  | Paper, Draw -> Paper
  | Scissors, Win -> Rock
  | Scissors, Lose -> Paper
  | Scissors, Draw -> Scissors

let get_result_part_1 line =
  let move1 = convert_to_move line.[0] in
  let move2 = convert_to_move line.[2] in
  let move2_points = move_to_points move2 in
  match (move1, move2) with
  | Rock, Paper -> move2_points + result_to_points Win
  | Rock, Scissors -> move2_points + result_to_points Lose
  | Paper, Rock -> move2_points + result_to_points Lose
  | Paper, Scissors -> move2_points + result_to_points Win
  | Scissors, Rock -> move2_points + result_to_points Win
  | Scissors, Paper -> move2_points + result_to_points Lose
  | _ -> move2_points + result_to_points Draw

let get_result_part_2 line =
  let move1 = convert_to_move line.[0] in
  let result = convert_to_result line.[2] in
  let move_to_play = move_to_play move1 result in
  move_to_points move_to_play + result_to_points result

let rec get_results lines =
  match lines with
  | [] -> []
  | h :: t -> (
      match h with "" -> [] | _ -> get_result_part_1 h :: get_results t)

(* print get_results *)
let results = get_results lines
(* print int list results *)

let rec sum_results results =
  match results with [] -> 0 | h :: t -> h + sum_results t

let () = print_endline (string_of_int (sum_results results))

(* Start of Day 02 - Part 2 *)

let rec get_results lines =
  match lines with
  | [] -> []
  | h :: t -> (
      match h with "" -> [] | _ -> get_result_part_2 h :: get_results t)

(* print get_results *)
let results = get_results lines
(* print int list results *)

let rec sum_results results =
  match results with [] -> 0 | h :: t -> h + sum_results t

let () = print_endline (string_of_int (sum_results results))
