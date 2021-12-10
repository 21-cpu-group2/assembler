{
    open Parser
}

let space = [' ' '\t' '\r' ',']
let digit = ['0'-'9']
let label_head = ['a'-'e' 'g'-'w' 'y'-'z' 'A'-'Z' '.']
let label_alp = ['a'-'z' 'A'-'Z' '.']

rule token = parse
| '\n'
    { NL }
| space+
    { token lexbuf } (* 空白を読み飛ばす *)
| "//"
    { token lexbuf }
| "jal"
    { Lexing.new_line lexbuf; JAL }
| "jalr"
    { Lexing.new_line lexbuf; JALR }
| "xor"
    { Lexing.new_line lexbuf; XOR }
| "addi"
    { Lexing.new_line lexbuf; ADDI }
| "add"
    { Lexing.new_line lexbuf; ADD }
| "sub"
    { Lexing.new_line lexbuf; SUB }
| "beq"
    { Lexing.new_line lexbuf; BEQ }
| "bne"
    { Lexing.new_line lexbuf; BNE }
| "blt"
    { Lexing.new_line lexbuf; BLT }
| "bge" 
    { Lexing.new_line lexbuf; BGE }
| "sll"
    { Lexing.new_line lexbuf; SLL }
| "slli"
    { Lexing.new_line lexbuf; SLLI }
| "srli"
    { Lexing.new_line lexbuf; SRLI }
| "lw"
    { Lexing.new_line lexbuf; LW }
| "sw"
    { Lexing.new_line lexbuf; SW }
| "fadd"
    { Lexing.new_line lexbuf; FADD }
| "fsub"
    { Lexing.new_line lexbuf; FSUB }
| "fmul"
    { Lexing.new_line lexbuf; FMUL }
| "fdiv"
    { Lexing.new_line lexbuf; FDIV }
| "min_caml_sqrt"
    { Lexing.new_line lexbuf; SQRT }
| "min_caml_fhalf"
    { Lexing.new_line lexbuf; FHALF }
| "min_caml_fabs"
    { Lexing.new_line lexbuf; FABS }
| "min_caml_fneg"
    { Lexing.new_line lexbuf; FNEG }
| "min_caml_fless"
    { Lexing.new_line lexbuf; FLESS }
| "min_caml_fiszero"
    { Lexing.new_line lexbuf; FISZERO }
| "min_caml_fispos"
    { Lexing.new_line lexbuf; FISPOS }
| "min_caml_fisneg"
    { Lexing.new_line lexbuf; FISNEG }
| "min_caml_floor"
    { Lexing.new_line lexbuf; FLOOR }
| "min_caml_ftoi"
    { Lexing.new_line lexbuf; FTOI }
| "min_caml_itof"
    { Lexing.new_line lexbuf; ITOF }
| "nop"
    { NOP }
| 'x' | 'f'
    { REG }
| '+' digit+ | '-' digit+ | digit+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ':'
    { COLON }
| label_head (label_alp|digit)* as l
    { LABEL (l) }
| '#' (label_alp|digit|space)* as l
    { print_string l; COMMENT_OUT }
| eof
    { EOF }