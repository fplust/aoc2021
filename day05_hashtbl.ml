(* open Stdlib *)
open Core

module Point = struct
  module T = struct
    type t = {x: int; y: int} [@@deriving compare, sexp, hash]
  end
  include T
  include Comparable.Make(T)
end

let parse_point p =
  let tp = String.split ~on:',' p
  |> List.map ~f:int_of_string in
    ({x = List.nth_exn tp 0; y = List.nth_exn tp 1} : Point.t)

let lines =
  In_channel.read_lines "./day05_input.txt"
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(fun x ->
      (parse_point (List.nth_exn x 0), parse_point (List.nth_exn x 2))
    )


let h_and_v_lines = List.filter lines ~f:(fun (p1, p2) ->
  (p1.x = p2.x) || (p1.y = p2.y))

let range_n n1 n2 = if n1 < n2 then
  List.range n1 (n2+1) else List.range ~stride:(-1) n1 (n2-1)

let p_of_line ((p1, p2):(Point.t * Point.t)) =
  let xn, yn = (p1.x - p2.x, p1.y - p2.y) in
  match xn, yn with
  | (0, _) -> range_n p1.y p2.y 
    |> List.map ~f:(fun y -> ({x=p1.x; y=y}: Point.t))
  | (_, 0) -> range_n p1.x p2.x
    |> List.map ~f:(fun x -> ({x=x; y=p1.y}: Point.t))
  | _ when (Int.abs xn) = (Int.abs yn) -> List.zip_exn (range_n p1.x p2.x) (range_n p1.y p2.y)
    |> List.map ~f:(fun (x, y) -> ({x=x; y=y}: Point.t))
  | _ -> []

let line_to_map lines m = List.iter lines ~f:(fun line ->
  p_of_line line
  |> List.iter ~f:(fun p ->
    match Hashtbl.find m p with
    | Some(n) -> Hashtbl.set m ~key:p ~data:(n+1)
    | None -> Hashtbl.set m ~key:p ~data:1
  ))

let print_map m = Hashtbl.iter_keys m ~f:(fun (p: Point.t) ->
  let v = Hashtbl.find_exn m p in
  if v > 1 then printf "%d, %d: %d\n" p.x p.y v else ()
  )

let count_map m = Hashtbl.filter m ~f:(fun v -> v > 1) |> Hashtbl.keys |> List.length

(* part 1 *)
let p_map = Hashtbl.create (module Point)
let () = line_to_map h_and_v_lines p_map
let gt1_count = count_map p_map

let () = printf "%d\n" gt1_count

(* part 2 *)
let p_map_2 = Hashtbl.create (module Point)
let () = line_to_map lines p_map_2
let gt1_count_2 = count_map p_map_2

let () = printf "%d\n" gt1_count_2