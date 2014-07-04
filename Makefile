PACKAGE     := batteries
DIRECTORIES := Data,Test
OCAMLBUILD  := ocamlbuild -classic-display -j 2 -package $(PACKAGE) -Is $(DIRECTORIES)

MAIN        := Test/test
EXEC        := test
COMPILE     := native
TYPECHECK   := inferred.mli

main:
	$(OCAMLBUILD) $(MAIN).$(COMPILE)
	ln -sf $(MAIN).$(COMPILE) $(EXEC)

dev:
	watch -d -n 1 "$(OCAMLBUILD) $(MAIN).$(TYPECHECK) && cat _build/$(MAIN).$(TYPECHECK)"

test:
	./test.sh

clean:
	rm -f *~ $(EXEC) $(MAIN).$(COMPILE)
	$(OCAMLBUILD) -clean
