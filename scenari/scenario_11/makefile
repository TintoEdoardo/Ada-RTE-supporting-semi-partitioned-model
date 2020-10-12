all: test

test:
	mkdir obj -p
	gprbuild scenario_11.gpr
	arm-eabi-objdump obj/scenario_11 -D > obj/scenario_11.s

clean:
	gprclean scenario_11.gpr