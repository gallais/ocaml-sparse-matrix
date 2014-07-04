#!/bin/bash
echo -e '\e[1mTesting: \e[32mSuccess\e[39m directory\e[0m'
for i in `find Test/Success/ | grep 'ml$' | sed s/\.ml//`
do
EXEC=$(basename $i)
ocamlbuild -classic-display -j 2 -package batteries -Is Data,Test $i.native > /dev/null \
  && ./$EXEC.native && rm -f $EXEC.native && echo -e "$EXEC: \e[1m\e[32mok!\e[0m"

done
