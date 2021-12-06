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
    | Lw(rd, offrs1) ->
        (match offrs1 with
        | Base_rel(offset, rs1) ->
            print_12 offset;
            f rs1;
            print_string "010";
            f rd;
            print_string "0000011\n"
        | _ -> raise Error)
    | Sw(rs2, offrs1) ->
        (match offrs1 with
        | Base_rel(offset, rs1) ->
            print_offset_m offset;
            f rs2;
            f rs1;
            print_string "010";
            print_offset_l offset;
            print_string "0100011\n"
        | _ -> raise Error)
    | Instlis(head, tail) ->
        f head;
        f tail
    | _ -> raise Error