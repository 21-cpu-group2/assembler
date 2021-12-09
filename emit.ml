open Syntax

exception Error

let print_reg n =
    match n with
    | Int(n) -> 
        (if ( n land (1 lsl 4) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 3) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 2) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 1) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 0) = 0 ) then print_int 0 else print_int 1)
    | _ -> raise Error 

let print_12 n =
    match n with
    | Int(n) ->  
        (if ( n land (1 lsl 11) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 10) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 9) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 8) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 7) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 6) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 5) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 4) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 3) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 2) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 1) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 0) = 0 ) then print_int 0 else print_int 1)
    | _ -> raise Error 

let print_offset_m offset =
    match offset with
    | Int(n) -> 
        (if ( n land (1 lsl 11) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 10) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 9) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 8) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 7) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 6) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 5) = 0 ) then print_int 0 else print_int 1)
    | _ -> raise Error
    
let print_offset_l offset = 
    match offset with
    | Int(n) -> 
        (if ( n land (1 lsl 4) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 3) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 2) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 1) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 0) = 0 ) then print_int 0 else print_int 1)
    | _ -> raise Error 

let print_offset offset =
    match offset with
    | Int(n) -> 
        (if ( n land (1 lsl 19) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 18) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 17) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 16) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 15) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 14) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 13) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 12) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 11) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 10) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 9) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 8) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 7) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 6) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 5) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 4) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 3) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 2) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 1) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 0) = 0 ) then print_int 0 else print_int 1)
    | _ -> raise Error 

let rec f e = 
    match e with
    | Reg(i) -> print_reg i
    | Jal(rd, offset) ->
        print_offset offset;
        f rd;
        print_string "1101111\n"
    | Jalr(rd, rs1, offset) ->
        print_12 offset;
        f rs1;
        print_string "000";
        f rd;
        print_string "1100111\n"
    | Xor(rd, rs1, rs2) -> 
        print_string "0000000";
        f rs2;
        f rs1;
        print_string "100";
        f rd;
        print_string "0110011\n"
    | Addi(rd, rs1, imm) ->
        print_12 imm;
        f rs1;
        print_string "000";
        f rd;
        print_string "0010011\n"
    | Add(rd, rs1, rs2) -> 
        print_string "0000000";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "0110011\n"
    | Sub(rd, rs1, rs2) -> 
        print_string "0100000";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "0110011\n"
    | Sll(rd, rs1, rs2) ->
        print_string "0000000";
        f rs2;
        f rs1;
        print_string "001";
        f rd;
        print_string "0010011\n"
    | Slli(rd, rs1, shamt) ->
        print_string "0000000";
        print_offset_l shamt; (* 5bit *)
        f rs1;
        print_string "001";
        f rd;
        print_string "0110011\n"
    | Srli(rd, rs1, shamt) ->
        print_string "0000000";
        print_offset_l shamt; (* 5bit *)
        f rs1;
        print_string "101";
        f rd;
        print_string "0010011\n"
    | Beq(rs1, rs2, offset) ->
        print_offset_m offset;
        f rs2;
        f rs1;
        print_string "000";
        print_offset_l offset;
        print_string "1100011\n"
    | Bne(rs1, rs2, offset) ->
        print_offset_m offset;
        f rs2;
        f rs1;
        print_string "100"; (* in risc-v "001" *)
        print_offset_l offset;
        print_string "1100011\n"
    | Blt(rs1, rs2, offset) ->
        print_offset_m offset;
        f rs2;
        f rs1;
        print_string "001";
        print_offset_l offset;
        print_string "1100011\n"
    | Bge(rs1, rs2, offset) ->
        print_offset_m offset;
        f rs2;
        f rs1;
        print_string "101";
        print_offset_l offset;
        print_string "1100011\n"
    | Lw(rd, rs1, offset) ->
        print_12 offset;
        f rs1;
        print_string "010";
        f rd;
        print_string "0000011\n"
    | Sw(rs2, rs1, offset) ->
        print_offset_m offset;
        f rs2;
        f rs1;
        print_string "010";
        print_offset_l offset;
        print_string "0100011\n"
    (* Float instructions *)
    | Fadd(rd, rs1, rs2) -> 
        print_string "0000000";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Fsub(rd, rs1, rs2) -> 
        print_string "0000100";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Fmul(rd, rs1, rs2) -> 
        print_string "0001000";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Fdiv(rd, rs1, rs2) -> 
        print_string "0001100";
        f rs2;
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Sqrt(rd, rs1) ->
        print_string "0101100";
        print_string "00000";
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Fhalf(rd, rs1) ->
        print_string "0001000";
        print_string "00000";
        f rs1;
        print_string "001";
        f rd;
        print_string "1010011\n"
    | Fabs(rd, rs1) ->
        print_string "0010000";
        print_string "00000";
        f rs1;
        print_string "010";
        f rd;
        print_string "1010011\n";
    | Fneg(rd, rs1) ->
        print_string "0010000";
        print_string "00000";
        f rs1;
        print_string "001";
        f rd;
        print_string "1010011\n"
    | Fless(rd, rs1, rs2) ->
        print_string "1010000";
        f rs2;
        f rs1;
        print_string "001";
        f rd;
        print_string "1010011\n"
    | Fiszero(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        f rs1;
        print_string "010";
        f rd;
        print_string "1010011\n"
    | Fispos(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        f rs1;
        print_string "011";
        f rd;
        print_string "1010011\n"
    | Fisneg(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        f rs1;
        print_string "101";
        f rd;
        print_string "1010011\n"
    | Floor(rd, rs1) ->
        print_string "1100000";
        print_string "00000";
        f rs1;
        print_string "001";
        f rd;
        print_string "1010011\n"
    | Ftoi(rd, rs1) ->
        print_string "1100000";
        print_string "00000";
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Itof(rd, rs1) ->
        print_string "1101000";
        print_string "00000";
        f rs1;
        print_string "000";
        f rd;
        print_string "1010011\n"
    | Instlis(head, tail) ->
        f head;
        f tail
    | Nop ->
        print_string "11111111111111111111111111111111\n"
    | _ -> raise Error