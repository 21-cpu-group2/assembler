%{  
    open Syntax
%}

%token <int> INT
%token REG
%token FREG
%token JAL XOR ADD SUB ADDI BEQ BNE LW SW
%token LPAREN RPAREN
%token EOF

%type <Syntax.t> exp
%start exp
%%

integer:
| INT                          { Int($1) }

oprand:
| INT                          { Int($1) }
| REG integer                  { Reg($2) }
| FREG integer                 { Freg($2) }
| integer LPAREN REG integer RPAREN    { Base_rel($1, $4) }

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

exp:
| inst                         { $1 }
| inst exp                     { Instlis($1, $2) }