all: test

test:
	mkdir obj -p
	gprbuild scenario_7.gpr
	arm-eabi-objdump obj/scenario_7 -D > obj/scenario_7.s

clean:
	gprclean scenario_7.gpr