open Syntax
module Labels = Map.Make(String)

exception Error
exception Error_f

let rec f e map = 
    match e with
    | Label(s,i) ->
        let newmap = Labels.add s i map in 
        newmap
    | Instlis(head, tail) ->
        let new_map = f head map in 
        f tail new_map
    | _ ->
        map