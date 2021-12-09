open Core

let depths =
  let lines = In_channel.read_lines "./day01_input.txt" in
  List.map ~f:int_of_string lines

let rec zip3_exn lists =
  match lists with
  | [], [], [] -> []
  | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
      (hd1, hd2, hd3) :: zip3_exn (tl1, tl2, tl3)
  | _ -> raise (Failure "List length mismatch")

let depths3 =
  let l = List.length depths in
  let depth_window =
    zip3_exn
      ( List.sub ~pos:0 ~len:(l - 2) depths,
        List.sub ~pos:1 ~len:(l - 2) depths,
        List.sub ~pos:2 ~len:(l - 2) depths )
  in
  List.map ~f:(fun (a, b, c) -> a + b + c) depth_window

let count_inceased data =
  let _, sum =
    List.fold data ~init:(0, 0) ~f:(fun (o, sum) n ->
        (n, sum + if n > o then 1 else 0))
  in
  sum - 1

let () = printf "%d\n" @@ count_inceased depths
let () = printf "%d\n" @@ count_inceased depths3
