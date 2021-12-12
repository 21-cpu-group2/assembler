open Syntax
open Mkmap

exception Error
exception Error_f

let print_reg n =
    match n with
    | Reg(Int(n)) | Freg(Int(n)) -> 
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

let print_for_lui offset =
    match offset with
    | Int(n) -> 
        (if ( n land (1 lsl 31) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 30) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 29) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 28) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 27) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 26) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 25) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 24) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 23) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 22) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 21) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 20) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 19) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 18) = 0 ) then print_int 0 else print_int 1);
        (if ( n land (1 lsl 17) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 16) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 15) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 14) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 13) = 0 ) then print_int 0 else print_int 1); 
        (if ( n land (1 lsl 12) = 0 ) then print_int 0 else print_int 1)
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

let rec f e map = 
    match e with
    | Jal(rd, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_offset rel_address;
        | Int(i) -> 
            print_offset offset;
        | _ ->
            raise Error);
        print_reg rd;
        print_string "1101111\n";
        map
    | Jalr(rd, rs1, offset) ->
        print_12 offset;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1100111\n";
        map
    | Xor(rd, rs1, rs2) -> 
        print_string "0000000";
        print_reg rs2;
        print_reg rs1;
        print_string "100";
        print_reg rd;
        print_string "0110011\n";
        map
    | Addi(rd, rs1, imm) ->
        (match imm with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_12 rel_address
        | Int(i) -> 
            print_12 imm
        | _ ->
            raise Error);
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "0010011\n";
        map;
    | Add(rd, rs1, rs2) -> 
        print_string "0000000";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "0110011\n";
        map
    | Sub(rd, rs1, rs2) -> 
        print_string "0100000";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "0110011\n";
        map
    | Sll(rd, rs1, rs2) ->
        print_string "0000000";
        print_reg rs2;
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "0010011\n";
        map
    | Slli(rd, rs1, shamt) ->
        print_string "0000000";
        print_offset_l shamt; (* 5bit *)
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "0110011\n";
        map
    | Srli(rd, rs1, shamt) ->
        print_string "0000000";
        print_offset_l shamt; (* 5bit *)
        print_reg rs1;
        print_string "101";
        print_reg rd;
        print_string "0010011\n";
        map
    | Beq(rs1, rs2, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_offset_m rel_address;
            print_reg rs2;
            print_reg rs1;
            print_string "000";
            print_offset_l rel_address;
            print_string "1100011\n";
        | Int(i) -> 
            print_offset_m offset;
            print_reg rs2;
            print_reg rs1;
            print_string "000";
            print_offset_l offset;
            print_string "1100011\n";
        | _ ->
            raise Error);
        map
    | Bne(rs1, rs2, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_offset_m rel_address;
            print_reg rs2;
            print_reg rs1;
            print_string "100";  (* in risc-v "001" *)
            print_offset_l rel_address;
            print_string "1100011\n";
        | Int(i) -> 
            print_offset_m offset;
            print_reg rs2;
            print_reg rs1;
            print_string "100";
            print_offset_l offset;
            print_string "1100011\n";
        | _ ->
            raise Error);
        map
    | Blt(rs1, rs2, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_offset_m rel_address;
            print_reg rs2;
            print_reg rs1;
            print_string "001";  (* in risc-v "100" *)
            print_offset_l rel_address;
            print_string "1100011\n";
        | Int(i) -> 
            print_offset_m offset;
            print_reg rs2;
            print_reg rs1;
            print_string "001";
            print_offset_l offset;
            print_string "1100011\n";
        | _ ->
            raise Error);
        map
    | Bge(rs1, rs2, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Labels.find s map in
            let pc = int_of_string i in
            let rel_address = Int((int_of_string (label_address)) - pc + 1) in
            print_offset_m rel_address;
            print_reg rs2;
            print_reg rs1;
            print_string "101";
            print_offset_l rel_address;
            print_string "1100011\n";
        | Int(i) -> 
            print_offset_m offset;
            print_reg rs2;
            print_reg rs1;
            print_string "101";
            print_offset_l offset;
            print_string "1100011\n";
        | _ ->
            raise Error);
        map
    | Li(rd, offset) ->
        (match offset with 
        | Label(s, i) ->
            let label_address = Int((int_of_string(Labels.find s map)))in
            print_for_lui label_address;
            print_reg rd;
            print_string "0110111\n";
            print_12 label_address;
        | Int(i) -> 
            print_for_lui offset;
            print_reg rd;
            print_string "0110111\n";
            print_12 offset;
        | _ ->
            raise Error);
        print_string "00000";
        print_string "000";
        print_reg rd;
        print_string "0010011\n";
        map
    | Lw(rd, rs1, offset) ->
        print_12 offset;
        print_reg rs1;
        print_string "010";
        print_reg rd;
        print_string "0000011\n";
        map
    | Sw(rs2, rs1, offset) ->
        print_offset_m offset;
        print_reg rs2;
        print_reg rs1;
        print_string "010";
        print_offset_l offset;
        print_string "0100011\n";
        map
    (* Float instructions *)
    | Fadd(rd, rs1, rs2) -> 
        print_string "0000000";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fsub(rd, rs1, rs2) -> 
        print_string "0000100";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fmul(rd, rs1, rs2) -> 
        print_string "0001000";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fdiv(rd, rs1, rs2) -> 
        print_string "0001100";
        print_reg rs2;
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Sqrt(rd, rs1) ->
        print_string "0101100";
        print_string "00000";
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fhalf(rd, rs1) ->
        print_string "0001000";
        print_string "00000";
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fabs(rd, rs1) ->
        print_string "0010000";
        print_string "00000";
        print_reg rs1;
        print_string "010";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fneg(rd, rs1) ->
        print_string "0010000";
        print_string "00000";
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fless(rd, rs1, rs2) ->
        print_string "1010000";
        print_reg rs2;
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fiszero(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        print_reg rs1;
        print_string "010";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fispos(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        print_reg rs1;
        print_string "011";
        print_reg rd;
        print_string "1010011\n";
        map
    | Fisneg(rd, rs1) ->
        print_string "1010000";
        print_string "00000";
        print_reg rs1;
        print_string "101";
        print_reg rd;
        print_string "1010011\n";
        map
    | Floor(rd, rs1) ->
        print_string "1100000";
        print_string "00000";
        print_reg rs1;
        print_string "001";
        print_reg rd;
        print_string "1010011\n";
        map
    | Ftoi(rd, rs1) ->
        print_string "1100000";
        print_string "00000";
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Itof(rd, rs1) ->
        print_string "1101000";
        print_string "00000";
        print_reg rs1;
        print_string "000";
        print_reg rd;
        print_string "1010011\n";
        map
    | Instlis(head, tail) ->
        let new_map = f head map in 
        f tail new_map
    | Nop ->
        print_string "11111111111111111111111111111111\n";
        map
    | Label(s,i) ->
        map
    | _ -> raise Error_f