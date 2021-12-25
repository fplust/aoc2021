open Core

let entrys =
  In_channel.read_lines "./day08_input.txt"
  |> List.map ~f:(fun l ->
         String.split ~on:'|' l
         |> List.map ~f:(fun l -> String.split ~on:' ' (String.strip l))
         |> fun ls -> (List.nth_exn ls 0, List.nth_exn ls 1))

let count_1 =
  List.map entrys ~f:(fun (_, digits) ->
      List.map digits ~f:(fun digit ->
          (* printf "%s\n" digit; *)
          match String.length digit with 2 | 3 | 4 | 7 -> 1 | _ -> 0)
      |> List.sum (module Int) ~f:(fun x -> x))
  |> List.sum (module Int) ~f:(fun x -> x)

let () = printf "%d\n" count_1

let diff a b =
  let al, bl = (String.to_list a, String.to_list b) in
  List.filter ~f:(fun x -> not (List.mem bl x ~equal:Char.equal)) al |> fun l ->
  List.nth_exn l 0

let same a b =
  let al, bl = (String.to_list a, String.to_list b) in
  List.filter ~f:(fun x -> List.mem bl x ~equal:Char.equal) al |> fun l ->
  List.nth_exn l 0

let get_left_up m = diff m.(4) m.(3)

let get_midle m =
  let al, bl = (String.to_list m.(3), String.to_list m.(4)) in
  List.filter
    ~f:(fun x ->
      List.mem bl x ~equal:Char.equal && not (String.contains m.(1) x))
    al
  |> fun l -> List.nth_exn l 0

let get_ru_rd m = (same m.(2) m.(1), same m.(5) m.(1))

let patterns entry =
  let m = Array.create ~len:10 "" in
  List.filter entry ~f:(fun x ->
      match String.length x with
      | 2 ->
          Array.set m 1 x;
          false
      | 3 ->
          Array.set m 7 x;
          false
      | 4 ->
          Array.set m 4 x;
          false
      | 7 ->
          Array.set m 8 x;
          false
      | _ -> true)
  |> List.filter ~f:(fun x ->
         match String.length x with
         | 5 ->
             if
               String.to_list m.(1)
               |> List.map ~f:(String.contains x)
               |> List.reduce_exn ~f:(fun a b -> a && b)
             then (
               Array.set m 3 x;
               false)
             else true
         | _ -> true)
  |> List.filter ~f:(fun x ->
         match String.length x with
         | 5 ->
             let left_up = get_left_up m in
             (* printf "%s, %c\n" x left_up; *)
             if String.contains x left_up then Array.set m 5 x
             else Array.set m 2 x;
             false
         | 6 ->
             let middle = get_midle m in
             if not (String.contains x middle) then (
               Array.set m 0 x;
               false)
             else true
         | _ -> true)
  |> List.iter ~f:(fun x ->
         let ru, _rd = get_ru_rd m in
         (* printf "%s, %c, %c\n" x ru _rd; *)
         if String.contains x ru then Array.set m 9 x else Array.set m 6 x);
  Array.to_list m

let sort s =
  String.to_list s |> List.sort ~compare:Char.compare |> String.of_char_list

let digits_list =
  List.map entrys ~f:(fun (signals, digits) ->
      (* printf "\n"; *)
      let map =
        patterns signals
        |> List.mapi ~f:(fun i s -> (* printf "%s, %d\n" s i; *)
                                    (sort s, i))
        |> Hashtbl.of_alist_exn (module String)
      in
      List.map digits ~f:(fun d -> Hashtbl.find_exn map (sort d))
      |> List.map ~f:string_of_int |> String.concat |> int_of_string)

let () = printf "%d\n" (List.sum (module Int) digits_list ~f:(fun x -> x))
