type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Int of int
  | Reg of t
  | Freg of t
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
  | Instlis of t * t