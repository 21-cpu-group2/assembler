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
| "fsqrt"
    { FSQRT }
| "sin"
    { SIN }
| "cos"
    { COS }
| "atan"
    { ATAN }
| "fhalf"
    { FHALF }
| "fabs"
    { FABS }
| "fneg"
    { FNEG }
| "fless"
    { FLESS }
| "fiszero"
    { FISZERO }
| "fispos"
    { FISPOS }
| "fisneg"
    { FISNEG }
| "floor"
    { FLOOR }
| "ftoi"
    { FTOI }
| "itof"
    { ITOF }
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