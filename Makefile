PACKAGES    := batteries
DIRECTORIES := Data,Test
OCAMLBUILD  := ocamlbuild -use-ocamlfind -classic-display -j 2 -pkgs $(PACKAGES) -Is $(DIRECTORIES)

MAIN        := Test/test
EXEC        := test
COMPILE     := native
TYPECHECK   := inferred.mli

main:
	$(OCAMLBUILD) $(MAIN).$(COMPILE)
	ln -sf $(MAIN).$(COMPILE) $(EXEC)

dev:
	watch -d -n 1 "make type && cat _build/$(MAIN).$(TYPECHECK)"

type:
	$(OCAMLBUILD) $(MAIN).$(TYPECHECK)

test:
	./test.sh

clean:
	rm -f *~ $(EXEC) $(MAIN).$(COMPILE)
	$(OCAMLBUILD) -clean
