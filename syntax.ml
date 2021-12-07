type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Int of int
  | Reg of t
  | Base_rel of t * t
  | Jal of t * t
  | Xor of t * t * t
  | Add of t * t * t
  | Sub of t * t * t
  | Addi of t * t * t
  | Beq of t * t * t
  | Bne of t * t * t
  | Lw of t * t 
  | Sw of t * t 
  | Fadd of t * t * t
  | Fsub of t * t * t
  | Fmul of t * t * t
  | Fdiv of t * t * t
  | Fsqrt of t * t
  | Sin of t * t
  | Cos of t * t
  | Atan of t * t
  | Fhalf of t * t
  | Fabs of t * t
  | Fneg of t * t
  | Fless of t * t * t
  | Fiszero of t * t
  | Fispos of t * t
  | Fisneg of t * t
  | Floor of t * t
  | Ftoi of t * t
  | Itof of t * t
  | Instlis of t * t