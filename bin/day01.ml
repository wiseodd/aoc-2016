open Base
open Stdio

type rot_dir = L | R
type dir = N | E | S | W
type loc = { x : int; y : int }
type agent = loc * dir

let l1_dist (a : loc) (b : loc) : int = Int.abs (a.x - b.x) + Int.abs (a.y - b.y)

let rotate (orig_dir : dir) (rot : rot_dir) : dir =
  match (orig_dir, rot) with
  | N, L -> W
  | N, R -> E
  | E, L -> N
  | E, R -> S
  | S, L -> E
  | S, R -> W
  | W, L -> S
  | W, R -> N

let move (curr_loc : loc) (curr_dir : dir) (n : int) : loc =
  match curr_dir with
  | N -> { x = curr_loc.x; y = curr_loc.y - n }
  | E -> { x = curr_loc.x + n; y = curr_loc.y }
  | S -> { x = curr_loc.x; y = curr_loc.y + n }
  | W -> { x = curr_loc.x - n; y = curr_loc.y }

let rec simulate (actions : (rot_dir * int) list) (a : agent) : agent =
  match actions with
  | [] -> a
  | action :: rest ->
      let new_dir = rotate (snd a) (fst action) in
      let new_loc = move (fst a) new_dir (snd action) in
      simulate rest (new_loc, new_dir)

let () =
  let actions : (rot_dir * int) list =
    Stdlib.In_channel.with_open_text "data/day01_input.txt"
      In_channel.input_lines
    |> List.hd |> Option.value_exn |> String.split ~on:','
    |> List.map ~f:(fun s ->
           match s |> String.strip |> String.to_list with
           | d :: digits ->
               let d = match d with 'L' -> L | 'R' -> R | _ -> failwith "" in
               (d, digits |> String.of_char_list |> Int.of_string)
           | _ -> failwith "")
  in
  let s = ({ x = 0; y = 0 }, N) in
  let e = simulate actions s in
  print_endline ("Part 1: " ^ Int.to_string (l1_dist (fst s) (fst e)))
