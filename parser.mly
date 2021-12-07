%{  
    open Syntax
%}

%token <int> INT
%token REG
%token JAL XOR ADD SUB ADDI BEQ BNE LW SW
%token FADD FSUB FMUL FDIV FSQRT
%token SIN COS ATAN
%token FHALF FABS FNEG
%token FLESS FISZERO FISNEG FISPOS
%token FLOOR FTOI ITOF
%token LPAREN RPAREN
%token EOF

%type <Syntax.t> exp
%start exp
%%

integer:
| INT                          { Int($1) }

reg:
| REG integer                  { Reg($2) }                         

oprand:
| INT                          { Int($1) }
| reg                          { $1 }
| integer LPAREN reg RPAREN    { Base_rel($1, $3) }

inst:
| JAL oprand oprand            { Jal($2, $3) }
| XOR oprand oprand oprand     { Xor($2, $3, $4) }
| ADD oprand oprand oprand     { Add($2, $3, $4) }
| SUB oprand oprand oprand     { Sub($2, $3, $4) }
| ADDI oprand oprand oprand    { Addi($2, $3, $4) }
| BEQ oprand oprand oprand     { Beq($2, $3, $4) }
| BNE oprand oprand oprand     { Bne($2, $3, $4) }
| LW oprand oprand             { Lw($2, $3) }
| SW oprand oprand             { Sw($2, $3) }
| FADD oprand oprand oprand    { Fadd($2, $3, $4) }
| FSUB oprand oprand oprand    { Fsub($2, $3, $4) }
| FMUL oprand oprand oprand    { Fmul($2, $3, $4) }
| FDIV oprand oprand oprand    { Fdiv($2, $3, $4) }
| FSQRT oprand oprand          { Fsqrt($2, $3) }
| SIN oprand oprand            { Sin($2, $3) }
| COS oprand oprand            { Cos($2, $3) }
| ATAN oprand oprand           { Atan($2, $3) }
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

exp:
| inst                         { $1 }
| inst exp                     { Instlis($1, $2) }