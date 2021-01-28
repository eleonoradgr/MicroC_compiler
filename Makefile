TARGET=microcc
TEST = ./test
PARSEROUTPUT = parserout

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.bitwriter,llvm.scalar_opts $@
	mv $@ $*

clean:
	ocamlbuild -clean ;\
	rm -f $(PARSEROUTPUT);

test: $TARGET $(TEST)
		rm -f $(PARSEROUTPUT); \
		for file in $(TEST)/* ; do \
			echo $${file} >> $(PARSEROUTPUT); \
			./$(TARGET) -s $${file} >> $(PARSEROUTPUT);\
			done

.PHONY: clean default test 