open Parser
open Lexer
open Emit
open Mkmap

let lexbuf outchan l =
    let map_init = Labels.empty in
    let tree = (Parser.exp Lexer.token l) in
    let map = Mkmap.f tree map_init in
    Emit.f tree map

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