all: test

test:
	mkdir obj -p
	gprbuild scenario_2.gpr
	arm-eabi-objdump obj/scenario_2 -D > obj/scenario_2.s

clean:
	gprclean scenario_2.gpr