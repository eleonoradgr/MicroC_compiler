TARGET=microcc
TEST = ./test
TESTCODEGEN = ./testcodegen
PARSEROUTPUT = parserout

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.bitwriter,llvm.scalar_opts $@
	mv $@ $*

clean:
	ocamlbuild -clean ;\
	rm -f $(PARSEROUTPUT);\
	rm -f test.o

test: $TARGET $(TEST)
		rm -f $(PARSEROUTPUT); \
		for file in $(TEST)/* ; do \
			echo $${file} >> $(PARSEROUTPUT); \
			./$(TARGET) -s $${file} >> $(PARSEROUTPUT);\
			done
testcodegen: $TARGET
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

.PHONY: clean default test testcodegen