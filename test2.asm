fadd f0, f1, f2
fadd f0, f1, f12
addi x0, x1, 2
l.1234 :
fadd f0, f1, f2
addi x1, x2, l.1234
l.3456 :
fadd f0, f1, f2
fadd f0, f1, f2
fadd f0, f1, f2
jal x5, l.3456
fsub f11, f12, f13
fsub f11, f12, f13
beq x3, x5, l.3456
beq x3, x5, 3456
bne x3, x5, l.3456
bne x3, x5, 3456
blt x3, x5, l.3456
blt x3, x5, 3456
bge x3, x5, l.3456
bge x3, x5, 3456
fsub f11, f12, f13
nop