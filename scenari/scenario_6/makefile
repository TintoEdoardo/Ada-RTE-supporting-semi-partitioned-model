all: test

test:
	mkdir obj -p
	gprbuild scenario_6.gpr
	arm-eabi-objdump obj/scenario_6 -D > obj/scenario_6.s

clean:
	gprclean scenario_6.gpr