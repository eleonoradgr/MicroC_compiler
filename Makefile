TARGET=microcc
TEST = ./test
PARSEROUTPUT = parserout

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

clean:
	ocamlbuild -clean ;\
	rm -f $(PARSEROUTPUT);

test: $TARGET $(TEST)
		rm -f $(PARSEROUTPUT); \
		for file in $(TEST)/* ; do \
			echo $${file} >> $(PARSEROUTPUT); \
			./$(TARGET) $${file} >> $(PARSEROUTPUT);\
			done

.PHONY: clean default test 