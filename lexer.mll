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
| 'r' 
    { REG }
| 'f'
    { FREG }
| '+' digit+ | '-' digit+ | digit+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }