TARGET=microcc
TESTSEMANT = ./testsemant
TESTCODEGEN = ./testcodegen
PARSEROUTPUT = parserout

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	rm -f test.o
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.bitwriter,llvm.scalar_opts $@
	mv $@ $*

clean:
	ocamlbuild -clean ;\
	rm -f $(PARSEROUTPUT);\
	rm -f test.o;\
	rm -f test.bc;\
	rm -f a.bc;

testsemant: clean $TARGET
		rm -f $(PARSEROUTPUT); \
		for file in $(TESTSEMANT)/* ; do \
			echo $${file} >> $(PARSEROUTPUT); \
			./$(TARGET) -s $${file} >> $(PARSEROUTPUT);\
			done
rtsupport: 
		clang -emit-llvm -S src/rt-support.c -o rt-support.ll; \
		llvm-as rt-support.ll -o rt-support.bc

testcodegen: clean rtsupport $TARGET
			for file in $(TESTCODEGEN)/*.mc ; do \
			echo $${file};\
			./$(TARGET) $${file};\
			llvm-link a.bc rt-support.bc -o test.bc;\
			llc -filetype=obj test.bc;\
			clang test.o;\
			./a.out >> text1.txt;\
			diff text1.txt $${file%.*}.out;\
			rm -f text1.txt;\
			done

compile: rtsupport $TARGET
		rm -f a.out;\
		./$(TARGET) $(file);\
		 llvm-link a.bc rt-support.bc -o test.bc;\
		 rm -f a.bc;\
		 llc -filetype=obj test.bc;\
		 rm -f test.bc;\
		 clang test.o;\
		 rm -f test.o;\


.PHONY: clean default testsemant testcodegen compile compilerun