all: test

test:
	mkdir obj -p
	gprbuild scenario_4.gpr
	arm-eabi-objdump obj/scenario_4 -D > obj/scenario_4.s

clean:
	gprclean scenario_4.gpr