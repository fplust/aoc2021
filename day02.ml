open Core

let commands =
  let lines = In_channel.read_lines "./day02_input.txt" in
  List.map lines ~f:(String.split_on_chars ~on:[ ' ' ])
  |> List.map ~f:(fun command ->
         (List.nth_exn command 0, int_of_string @@ List.nth_exn command 1))

type position = { mutable x : int; mutable z : int; mutable aim : int }

let final_pos =
  let pos : position = { x = 0; z = 0; aim = 0 } in
  List.iter
    ~f:(fun (direction, distance) ->
      match direction with
      | "forward" -> pos.x <- pos.x + distance
      | "down" -> pos.z <- pos.z + distance
      | "up" -> pos.z <- pos.z - distance
      | _ -> raise (Failure "unsupported command"))
    commands;
  pos

let () = printf "%d, %d\n" final_pos.x final_pos.z
let () = printf "%d\n" (final_pos.x * final_pos.z)

let final_pos2 =
  let pos : position = { x = 0; z = 0; aim = 0 } in
  List.iter
    ~f:(fun (direction, distance) ->
      match direction with
      | "forward" ->
          pos.x <- pos.x + distance;
          pos.z <- pos.z + (pos.aim * distance)
      | "down" -> pos.aim <- pos.aim + distance
      | "up" -> pos.aim <- pos.aim - distance
      | _ -> raise (Failure "unsupported command"))
    commands;
  pos

let () = printf "%d, %d, %d\n" final_pos2.x final_pos2.z final_pos2.aim
let () = printf "%d\n" (final_pos2.x * final_pos2.z)