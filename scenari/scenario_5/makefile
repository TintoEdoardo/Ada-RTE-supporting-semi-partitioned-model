all: test

test:
	mkdir obj -p
	gprbuild scenario_5.gpr
	arm-eabi-objdump obj/scenario_5 -D > obj/scenario_5.s

clean:
	gprclean scenario_5.gpr