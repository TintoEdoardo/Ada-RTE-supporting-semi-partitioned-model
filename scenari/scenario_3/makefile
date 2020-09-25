all: test

test:
	mkdir obj -p
	gprbuild scenario_3.gpr
	arm-eabi-objdump obj/scenario_3 -D > obj/scenario_3.s

clean:
	gprclean scenario_3.gpr