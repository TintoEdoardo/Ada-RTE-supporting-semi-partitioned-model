
mkdir $PWD/cortex-ar/zynq7000/ravenscar-full/adalib/ $PWD/cortex-ar/zynq7000/ravenscar-full/obj/ $PWD/cortex-ar/zynq7000/ravenscar-full/user_srcs/ -p &&
gprbuild -P ravenscar_full_zynq7000.gpr -j0 &&
mkdir runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000 -p &&
gprinstall -P ravenscar_full_zynq7000.gpr -p -f -XPREFIX=$PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000 --install-name=ravenscar_full_zynq7000 &&
[ -d "$PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/adalib/ravenscar_full_zynq7000" ] && mv $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/adalib/ravenscar_full_zynq7000/* $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/adalib/;
[ -d "$PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnarl/ravenscar_full_zynq7000" ] && mv $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnarl/ravenscar_full_zynq7000/* $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnarl/;
[ -d "$PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnat/ravenscar_full_zynq7000" ] && mv $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnat/ravenscar_full_zynq7000/* $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnat/;

rm -rf $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/adalib/ravenscar_full_zynq7000 $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnarl/ravenscar_full_zynq7000 $PWD/runtime/arm-eabi/lib/gnat/ravenscar_full_zynq7000/gnat/ravenscar_full_zynq7000