

nec2dxs: nec2dxs.f90
	clear
	gfortran -std=gnu -O -fno-automatic nec2dxs.f90 -o nec2dxs 2>&1 | tee compile.log

old: nec2dxs.f
	clear
	gfortran -std=gnu -O -fno-automatic nec2dxs.f -o nec2dxs 2>&1 | tee compile.log

clean:
	@rm -f *~ *.out *.mod *.log nec2dxs

test: nec2dxs
	echo "G5RV.nec\nG5RV.out\n" | ./nec2dxs

discard:
	git checkout -- .

nocomment:
	git commit -m "more general cleanup"

