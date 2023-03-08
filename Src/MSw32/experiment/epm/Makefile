all: dynepm.exe epm.dll
%.dll : %.o
	gcc -shared -o $@ $< -lpsapi

dynepm.exe: dynepm.o 
	gcc -o $@ $<

clean: 
	- rm *.exe *.dll *.o
