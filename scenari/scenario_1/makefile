all: test

test:
	mkdir obj -p
	gprbuild scenario_1.gpr
	arm-eabi-objdump obj/scenario_1 -D > obj/scenario_1.s

clean:
	gprclean scenario_1.gpr