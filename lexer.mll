{
    open Parser
}

let space = [' ' '\t' '\r' '\n' ',']
let digit = ['0'-'9']
let lower = ['a'-'z']

rule token = parse
| space+
    { token lexbuf } (* 空白を読み飛ばす *)
| "//"
    { token lexbuf }
| "jal"
    { JAL }
| "jalr"
    { JALR }
| "xor"
    { XOR }
| "addi"
    { ADDI }
| "add"
    { ADD }
| "sub"
    { SUB }
| "beq"
    { BEQ }
| "bne"
    { BNE }
| "blt"
    { BLT }
| "bge" 
    { BGE }
| "sll"
    { SLL }
| "slli"
    { SLLI }
| "srli"
    { SRLI }
| "lw"
    { LW }
| "sw"
    { SW }
| "fadd"
    { FADD }
| "fsub"
    { FSUB }
| "fmul"
    { FMUL }
| "fdiv"
    { FDIV }
| "min_caml_sqrt"
    { SQRT }
| "min_caml_fhalf"
    { FHALF }
| "min_caml_fabs"
    { FABS }
| "min_caml_fneg"
    { FNEG }
| "min_caml_fless"
    { FLESS }
| "min_caml_fiszero"
    { FISZERO }
| "min_caml_fispos"
    { FISPOS }
| "min_caml_fisneg"
    { FISNEG }
| "min_caml_floor"
    { FLOOR }
| "min_caml_ftoi"
    { FTOI }
| "min_caml_itof"
    { ITOF }
| "nop"
    { NOP }
| 'r' | 'f'
    { REG }
| '+' digit+ | '-' digit+ | digit+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }