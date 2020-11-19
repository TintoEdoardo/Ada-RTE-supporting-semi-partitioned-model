all: test

test:
	mkdir obj -p
	gprbuild scenario_9.gpr
	arm-eabi-objdump obj/scenario_9 -D > obj/scenario_9.s

clean:
	gprclean scenario_9.gpr