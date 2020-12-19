TARGET=microcc
PARSERTEST = ./testparser
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

parsertest: $TARGET $(PARSERTEST)
		rm -f $(PARSEROUTPUT); \
		for file in $(PARSERTEST)/* ; do \
			echo $${file} >> $(PARSEROUTPUT); \
			./$(TARGET) $${file} >> $(PARSEROUTPUT);\
			done

.PHONY: clean default parsertest 