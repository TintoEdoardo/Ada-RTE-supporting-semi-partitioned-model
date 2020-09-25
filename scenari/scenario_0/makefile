all: test

test:
	mkdir obj -p
	gprbuild scenario_0.gpr
	arm-eabi-objdump obj/scenario_0 -D > obj/scenario_0.s

clean:
	gprclean scenario_0.gpr