open Tsdl;;
open Array;;

let rect1 = Sdl.Rect.create 0 0 2 1;;
let rect2 = Sdl.Rect.create 2 0 2 1;;
let tab = [|1;2;3;4|];;
let b = Sdl.has_intersection rect1 rect2 in if b then Printf.printf "True" else Printf.printf "False";;

let test l =
  let rec aux l acc =
    match l with
    |[] -> acc
    |h::t ->
       match h with
       |1 -> let acc = h::acc in aux t acc
       |2 -> let acc = 0::acc in aux t acc
       |__ -> aux t acc
  in aux l []
;;

let l = test [1;2;3;2;1;2;3];;

let rec print_list l =
  match l with
  |[] -> ()
  |h::t -> Printf.printf "%d; " h;
    print_list t
;;

type truc = { kind : string; pv : int};;


let t = {kind = "oui"; pv = 10};;
let t2 = {kind = "non"; pv = 10};;
let a = length tab in
print_int a
