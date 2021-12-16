{
    open Parser
}

let space = [' ' '\t' '\r' ',']
let digit = ['0'-'9']
let label_head = ['a'-'e' 'g'-'w' 'y'-'z' 'A'-'Z' '.']
let alphabet = ['a'-'z' 'A'-'Z' '.' '_']

rule token = parse
| ('\n')+
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
| "li" (*immの値に応じてluiを用いるかが変わる。すなわちLexing.new_lineを1やるか2回やるかわからない *)
    { Lexing.new_line lexbuf; 
      Lexing.new_line lexbuf; 
      LI } 
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
| "sqrt"
    { Lexing.new_line lexbuf; SQRT }
| "fhalf"
    { Lexing.new_line lexbuf; FHALF }
| "fabs"
    { Lexing.new_line lexbuf; FABS }
| "fneg"
    { Lexing.new_line lexbuf; FNEG }
| "fless"
    { Lexing.new_line lexbuf; FLESS }
| "fiszero"
    { Lexing.new_line lexbuf; FISZERO }
| "fispos"
    { Lexing.new_line lexbuf; FISPOS }
| "fisneg"
    { Lexing.new_line lexbuf; FISNEG }
| "floor"
    { Lexing.new_line lexbuf; FLOOR }
| "ftoi"
    { Lexing.new_line lexbuf; FTOI }
| "itof"
    { Lexing.new_line lexbuf; ITOF }
| "nop"
    { Lexing.new_line lexbuf; NOP }
| "%" | "%a" 
    { REG }
| "%f"
    { FREG }
| "%zero"
    { REG_ZERO }
| "%fzero"
    { FREG_ZERO }
| "ra"
    { INT(-5) }
| "sp"
    { INT(-4) }
| "min_caml_hp"
    { INT(-3) }
| "in"
    { INT(-2) }
| "out"
    { INT(-1) }
| '+' digit+ | '-' digit+ | digit+ as l
    { INT (int_of_string (l)) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ':'
    { COLON }
| "l." (alphabet|digit)* as l
    { LABEL_FLOAT_TABLE (l) }
| (alphabet|digit)+ as l
    { LABEL (l) }
| '#' ([^'\n'])* 
    { COMMENT_OUT }
| eof
    { EOF }