Test: Test.hs Debug.o
	ghc --make -O2 Test.hs

Debug.o: Debug.hs
	ghc -O2 Debug.hs

clean:
	-rm Debug.hi
	-rm Debug.o
	-rm Test.hi
	-rm Test.o
	-rm Test
