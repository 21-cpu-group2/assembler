lw %f6 %a16 0  # A13の値をセット
fmul %f7 %f0 %f0 # A^2
fmul %f8 %f0 %f7 # A^3
fmul %f9 %f7 %f8 # A^5
fmul %f10 %f7 %f9 # A^7
fmul %f11 %f7 %f10 # A^9
fmul %f12 %f7 %f11 # A^11
fmul %f13 %f7 %f12 # A^13
fmul %f1 %f1 %f8 # A3 * A^3
fmul %f2 %f2 %f9 # A5 * A^5
fmul %f3 %f3 %f10 # A7 * A^7