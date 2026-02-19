let hello () = print_endline "Hello, World!"

let add x y = x + y

(*  
    Construct a function generate which, given
    integers n, returns a list consisting of the
    first n positive integers 
*)

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

(*
    Implement the function double where double l is
    the same as the list l but with every element
    doubled   
*)
let rec double (l: int list) : int list = 
    match l with
    | [] -> []
    | x :: xs -> (2 * x) :: double(xs)


(*
    Implement the function

    remove_all_negatives : int list -> int list

    where remove_all_negative l is the same as the
    list l but with all negative numbers removed
*)
let rec remove_all_negatives (l : int list) : int list =
    match l with 
    | [] -> []
    | x :: xs -> 
        if x < 0 then
            (* dont add *)
            remove_all_negatives(xs)
        else
            (* add *)
            x :: remove_all_negatives(xs)

(*
    Implement the function

    delete_every_other : int list -> int list

    such that delete_every_other l is the first,
    third, fifth,..., and so on elements of l
*)
let delete_every_other (l : int list) : int list = 
    let rec aux (delete : bool) (current : int list) =
        match current with
        | [] -> []
        | x :: xs ->
            if delete = true then
                aux (false) (xs)
            else
                x :: aux (true) (xs)
    in
    aux (false) (l)


(*
    Implement the function

    reverse : 'a list -> 'a list

    such that reverse l is the same as l but in
    reverse order

    let rec aux (current: 'a list) =
        match current with
        | [] -> []
        | x :: xs ->
            x :: aux (xs)
    in
    aux (l)
*)
let reverse (l : 'a list) : 'a list = 
    List.rev l

