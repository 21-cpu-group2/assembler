li %sp 44000
li %in 100000
li %out 200000
li %min_caml_hp 300000
l.6:	# 2.000000
	1073741824
l.4:	# 1.000000
	1065353216
min_caml_start:
	li %a0 l.4 #1
	slli %a0 %a0 2 #1
	lw %f0 %a0 0 #1
	li %a0 l.6 #2
	slli %a0 %a0 2 #2
	lw %f2 %a0 0 #2
	fadd %f0 %f0 %f2 #3
	fadd %f0 %a0 %fzero #3