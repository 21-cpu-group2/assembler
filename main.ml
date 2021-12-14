open Parser
open Lexer
open Emit
open Mkmap

let print_correspond label pc =
    print_string (label ^ " " ^ pc ^ "\n")

let print_label_pc = false

let lexbuf outchan l =
    let map_init = Labels.empty in
    let tree = (Parser.exp Lexer.token l) in
    let map = Mkmap.f tree map_init in 
    let pc_init = ((int_of_string(Labels.find "min_caml_start" map))) in
    if print_label_pc then Labels.iter print_correspond map
    else (
        print_int pc_init;
        print_string "\n";
        Emit.f tree map;
        print_string "11111111111111111111111111111111"
    )

(* let string s = lexbuf stdout (Lexing.from_string s) *)

let file f =
    let inchan = open_in (f ^ ".asm") in
    let outchan = open_out (f ^ ".bin") in
    try
        lexbuf outchan (Lexing.from_channel inchan);
        close_in inchan;
        close_out outchan;
    with e -> (close_in inchan; close_out outchan; raise e)

let () = 
    let file_name = Sys.argv.(1) in
    file file_name