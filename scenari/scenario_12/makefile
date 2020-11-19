all: test

test:
	mkdir obj -p
	gprbuild scenario_12.gpr
	arm-eabi-objdump obj/scenario_12 -D > obj/scenario_12.s

clean:
	gprclean scenario_12.gpr