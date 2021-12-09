open Stdlib
open Core

let call, boards =
  let lines = In_channel.read_lines "./day04_input.txt" in
  match lines with
  | first :: after ->
      ( first |> String.split ~on:',' |> List.map ~f:int_of_string,
        after |> List.chunks_of ~length:6 )
  | [] -> raise (Failure "input empty")

type value = { v : int; mutable marked : bool }

let parse_board lines =
  match lines with
  | [] -> raise (Failure "empty board")
  | _ :: b_lines ->
      List.map
        ~f:(fun line ->
          line |> String.to_list |> List.chunks_of ~length:3
          |> List.map ~f:(fun cs ->
                 String.of_char_list cs |> String.strip |> fun v ->
                 { v = int_of_string v; marked = false }))
        b_lines

let board_list = List.map ~f:parse_board boards
let () = printf "%d, %d\n" (List.length call) (List.length board_list)
(* let () = List.iter ~f:(fun l ->
     printf "%d\n" (List.length l)
   ) (List.nth_exn board_list 0) *)

let mark board called =
  Stdlib.List.flatten board
  |> List.iter ~f:(fun x -> if x.v = called then x.marked <- true else ())

let diagonal board =
  (* 对角线 *)
  let length = List.length board in
  [
    List.map (List.range ~stride:1 0 length) ~f:(fun i ->
        List.nth_exn (List.nth_exn board i) i);
    List.map (List.range ~stride:1 0 length) ~f:(fun i ->
        List.nth_exn (List.nth_exn board i) (length - 1 - i));
  ]

let check_win board =
  let checks =
    List.append
      (List.append board
         ((* 纵向 *)
          List.fold ~init:[] ~f:(fun a b -> b :: a) board))
      (diagonal board)
  in
  List.map checks ~f:(List.for_all ~f:(fun x -> x.marked))
  |> List.exists ~f:(fun x -> x)

let rec iter_until ~f ~p list =
  match list with
  | x :: xs -> (
      f x;
      let r = p x in
      match r with Some v -> Some (x, v) | None -> iter_until ~f ~p xs)
  | [] -> None

let result =
  iter_until
    ~f:(fun x -> List.iter ~f:(fun b -> mark b x) board_list)
    ~p:(fun _ ->
      let result = List.map board_list ~f:check_win in
      match List.findi result ~f:(fun _ x -> x) with
      | Some (i, _) -> Some i
      | None -> None)
    call

let last_call, index = Option.value ~default:(-1, -1) result
let win_board = List.nth_exn board_list index

let sum_of_unmark board =
  Stdlib.List.flatten board
  |> List.filter ~f:(fun x -> not x.marked)
  |> List.map ~f:(fun x -> x.v)
  |> List.reduce_exn ~f:(fun sum x -> sum + x)

let () = printf "%d, %d\n" last_call index
let () = printf "%d\n" (last_call * sum_of_unmark win_board)
let last_index = ref 0

let result_last =
  iter_until
    ~f:(fun x -> List.iter ~f:(fun b -> mark b x) board_list)
    ~p:(fun _ ->
      let result = List.map board_list ~f:check_win in
      match List.findi result ~f:(fun _ x -> not x) with
      | Some (i, _) ->
          last_index := i;
          None
      | None -> Some (-1))
    call

let last_win_board = List.nth_exn board_list !last_index
let last_call, index = Option.value ~default:(-1, -1) result_last
let () = printf "%d, %d\n" last_call !last_index
let () = printf "%d\n" (last_call * sum_of_unmark last_win_board)
