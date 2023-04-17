let file = "inputs/03/input.txt"

let read_lines file =
  In_channel.open_bin file |> In_channel.input_all |> String.split_on_char '\n'

let () = read_lines file |> List.iter print_endline
