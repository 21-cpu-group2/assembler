%{  
    open Syntax
%}

%token <int> INT
%token <string> LABEL
%token LABEL
%token NL
%token REG
%token JAL JALR XOR ADD SUB ADDI BEQ BNE BLT BGE SLL SLLI SRLI LW SW
%token FADD FSUB FMUL FDIV SQRT
%token FHALF FABS FNEG
%token FLESS FISZERO FISNEG FISPOS
%token FLOOR FTOI ITOF
%token LPAREN RPAREN
%token COLON
%token NOP
%token EOF
%token COMMENT_OUT

%type <Syntax.t> exp
%start exp
%%

integer:
| INT                          { Int($1) }

reg:
| REG integer                  { Reg($2) }                         

oprand:
| INT                          { Int($1) }
| LABEL                        { Label($1, (string_of_int(Parsing.symbol_start_pos ()).pos_lnum)) }
| reg                          { $1 }
| integer LPAREN reg RPAREN    { Base_rel($1, $3) }

inst:
| JAL oprand oprand            { Jal($2, $3) }
| JALR oprand oprand oprand    { Jalr($2, $3, $4) }
| XOR oprand oprand oprand     { Xor($2, $3, $4) }
| ADD oprand oprand oprand     { Add($2, $3, $4) }
| SUB oprand oprand oprand     { Sub($2, $3, $4) }
| ADDI oprand oprand oprand    { Addi($2, $3, $4) }
| BEQ oprand oprand oprand     { Beq($2, $3, $4) }
| BNE oprand oprand oprand     { Bne($2, $3, $4) }
| BLT oprand oprand oprand     { Blt($2, $3, $4) }
| BGE oprand oprand oprand     { Bge($2, $3, $4) }
| SLL oprand oprand oprand     { Sll($2, $3, $4) }
| SLLI oprand oprand oprand    { Sll($2, $3, $4) }
| SRLI oprand oprand oprand    { Sll($2, $3, $4) }
| LW oprand oprand oprand      { Lw($2, $3, $4) }
| SW oprand oprand oprand      { Sw($2, $3, $4) }
| FADD oprand oprand oprand    { Fadd($2, $3, $4) }
| FSUB oprand oprand oprand    { Fsub($2, $3, $4) }
| FMUL oprand oprand oprand    { Fmul($2, $3, $4) }
| FDIV oprand oprand oprand    { Fdiv($2, $3, $4) }
| SQRT oprand oprand           { Sqrt($2, $3) }
| FHALF oprand oprand          { Fhalf($2, $3) }
| FABS oprand oprand           { Fabs($2, $3) }
| FNEG oprand oprand           { Fneg($2, $3) }
| FLESS oprand oprand oprand   { Fless($2, $3, $4) }
| FISZERO oprand oprand        { Fiszero($2, $3) }
| FISPOS oprand oprand         { Fispos($2, $3) }
| FISNEG oprand oprand         { Fisneg($2, $3) }
| FLOOR oprand oprand          { Floor($2, $3) }
| FTOI oprand oprand           { Ftoi($2, $3) }
| ITOF oprand oprand           { Itof($2, $3) }
| NOP                          { Nop }
| LABEL COLON                  { Label($1, (string_of_int(Parsing.symbol_start_pos ()).pos_lnum)) }


exp:
| inst                         { $1 }
| inst NL exp                  { Instlis($1, $3) }
| inst COMMENT_OUT NL exp      { Instlis($1, $4) }
| COMMENT_OUT NL exp           { $3 }