open Core

let reports =
  let lines = In_channel.read_lines "./day03_input.txt" in
  List.map lines ~f:(fun row ->
      String.to_list row |> List.map ~f:int_of_char
      |> List.map ~f:(fun x -> x - 48))

let compare_data data =
  let length = List.length data / 2 in
  List.reduce_exn data ~f:(fun r1 r2 ->
      List.map2_exn r1 r2 ~f:(fun a b -> a + b))
  |> List.map ~f:(fun s -> compare s length)

let sum_reports = compare_data reports
let gamma = List.map sum_reports ~f:(fun s -> if s >= 0 then "1" else "0")
let epsilon = List.map sum_reports ~f:(fun s -> if s <= 0 then "1" else "0")
let binary_to_decimal b = b |> String.concat |> fun s -> int_of_string ("0b" ^ s)

(* let () = printf "%s, %s\n" (binary_to_decimal gamma) (binary_to_decimal epsilon) *)
let () = printf "%d, %d\n" (binary_to_decimal gamma) (binary_to_decimal epsilon)
let () = printf "%d\n" (binary_to_decimal gamma * binary_to_decimal epsilon)
let most_common x = if x >= 0 then 1 else 0
let least_common x = if x < 0 then 1 else 0

let rec find data i c =
  if List.length data = 1 then List.nth_exn data 0
  else
    let common = c (List.nth_exn (compare_data data) i) in
    List.filter data ~f:(fun row -> List.nth_exn row i = common) |> fun d ->
    (find [@tailcall]) d (i + 1) c

let gamma =
  find reports 0 most_common
  |> List.map ~f:(fun s -> if s = 1 then "1" else "0")

let epsilon =
  find reports 0 least_common
  |> List.map ~f:(fun s -> if s = 1 then "1" else "0")

(* let () = printf "%s, %s\n" (binary_to_decimal gamma) (binary_to_decimal epsilon) *)
let () = printf "%d, %d\n" (binary_to_decimal gamma) (binary_to_decimal epsilon)
let () = printf "%d\n" (binary_to_decimal gamma * binary_to_decimal epsilon)
