open Core

let init_fish fn =
  let fishes_array = Array.create ~len:9 0 in
    In_channel.read_lines fn
    |> fun lines -> List.nth_exn lines 0
    |> String.split ~on:','
    |> List.map ~f:int_of_string
    |> List.iter ~f:(fun x -> Array.set fishes_array x (fishes_array.(x) + 1))
    ;
  fishes_array

let nextday fishes =
  let n_0 = fishes.(0) in
    (* printf "%d\n" n_0; *)
    Array.iteri fishes ~f:(fun i _ ->
      match i with
      | 6 -> Array.set fishes 6 (fishes.(7) + n_0)
      | 8 -> Array.set fishes 8 n_0
      | _ -> Array.set fishes i (fishes.(i+1))
    )

let sum fishes = Array.sum (module Int) fishes ~f:Fun.id

let rec after_days fishes days =
  let () = printf "day %d: %d\n" days (sum fishes) in
    match days with
    | 0 -> fishes
    | x when x > 0 -> (nextday fishes; after_days  fishes (x - 1))
    | _ -> raise (Failure "error")

let fishes_array = init_fish "./day06_input.txt"
let count_fishes = after_days fishes_array 80 |> sum
let () = printf "%d\n" count_fishes
let fishes_array_2 = init_fish "./day06_input.txt"
let count_fishes_2 = after_days fishes_array_2 256 |> sum
let () = printf "%d\n" count_fishes_2