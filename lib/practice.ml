let hello () = print_endline "Hello, World!"

let add x y = x + y

(*  Construct a function generate which, given
integers n, returns a list consisting of the
first n positive integers *)

(* build list incrementally from 1 to n consing each element
    to the front (current :: acc) then reverse the accumulator at the end
*)
let generate_inc (n : int) : int list =
    let rec aux current acc = 
        (* base case *)
        if current > n then
            List.rev acc
        (* rec case *)  
        else
            aux (current + 1) (current :: acc)
    in
    aux 1 []

(* build list by counting down from n to 1 and consing each element
    to the front, so the final list is already in the correct order
*)
let generate (n : int) : int list = 
    let rec aux current acc = 
        (* base case *)
        if current = 0 then 
            acc
        (* rec case *)
        else 
            aux (current - 1) (current :: acc)
    in
    aux n []