open Core

let crabs =
  In_channel.read_lines "./day07_input.txt"
  |> fun lines -> List.nth_exn lines 0
  |> String.split ~on:','
  |> List.map ~f:int_of_string


let cost crabs pos =
  List.map crabs ~f:(fun x -> Int.abs (x - pos))
  |> List.sum (module Int) ~f:Fun.id

let cost_2 crabs pos =
  List.map crabs ~f:(fun x ->
    let step = Int.abs (x - pos) in
      step * (step + 1) / 2
    )
  |> List.sum (module Int) ~f:Fun.id

let middle b e = (b + e) / 2

let rec min_cost cost crabs b e =
  let m = middle b e in
    let (b_cost, m_cost, e_cost) = (cost crabs b, cost crabs m, cost crabs e) in
      (* printf "%d, %d, %d\n" b m e;
      printf "%d, %d, %d\n" b_cost m_cost e_cost; *)
      if (b = m) || (m = e) then Int.min b_cost e_cost else
        if b_cost = e_cost then m_cost else
          if b_cost < e_cost then min_cost cost crabs b m else min_cost cost crabs m e

let max = List.reduce_exn crabs ~f:Int.max
let c = min_cost cost crabs 0 max
let () = printf "%d\n" c
let c2 = min_cost cost_2 crabs 0 max
let () = printf "%d\n" c2