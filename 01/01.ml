let file = "input.txt"

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let rec group input result =
  match input with
  | [] -> result (* base case*)
  | "" :: rest -> group rest (0 :: result)
  | cals :: rest ->
      group rest
        (match result with
        | [] -> [ int_of_string cals ]
        | hd :: tl -> (hd + int_of_string cals) :: tl)

let rec max_of_list input current =
  match input with
  | [] -> current (* Base Case *)
  | first :: rest -> max_of_list rest (max first current)

let rec max3 input (m1, m2, m3) =
  match input with
  | [] -> (m1, m2, m3)
  | first :: rest ->
      max3 rest
        (match (m1, m2, m3) with
        | m1, m2, _ when first > m1 -> (first, m1, m2)
        | m1, m2, _ when first > m2 -> (m1, first, m2)
        | m1, m2, m3 when first > m3 -> (m1, m2, first)
        | _ -> (m1, m2, m3))
;;

(* Part 1 *)
print_int (max_of_list (group (read_lines file) []) 0)

(* Part 2 *)
let m1, m2, m3 = max3 (group (read_lines file) []) (0, 0, 0);;

print_newline ();;
print_string (string_of_int (m1 + m2 + m3))
