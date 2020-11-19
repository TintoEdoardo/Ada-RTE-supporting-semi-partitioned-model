all: test

test:
	mkdir obj -p
	gprbuild scenario_10.gpr
	arm-eabi-objdump obj/scenario_10 -D > obj/scenario_10.s

clean:
	gprclean scenario_10.gpr