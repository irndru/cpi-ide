cpiwb : reset
		ghc -O2 --make -W -fno-warn-unused-binds -fno-warn-unused-matches cpiwb
		ghc -O2 --make -W -fno-warn-unused-binds -fno-warn-unused-matches cpi-ide

clean : 
		rm -rf cpiwb *.o *.hi CPi/*.o CPi/*.hi
		rm -rf cpi-ide *.o *.hi

reset :
		rm -rf cpiwb
		rm -rf cpi-ide
