open Parser
open Lexer
open Emit

let lexbuf outchan l =
    Emit.f(Parser.exp Lexer.token l)

let string s = lexbuf stdout (Lexing.from_string s)

let file f =
    let inchan = open_in (f ^ ".asm") in
    let outchan = open_out (f ^ ".bin") in
    try
        lexbuf outchan (Lexing.from_channel inchan);
        close_in inchan;
        close_out outchan;
    with e -> (close_in inchan; close_out outchan; raise e)

let () = 
    file "test";
    print_string "11111111111111111111111111111111\n" (* nop *)