#!/bin/bash
echo -e '\e[1mTesting: \e[32mSuccess\e[39m directory\e[0m'
for i in `find Test/Success/ | grep 'ml$' | sed s/\.ml//`
do
EXEC=$(basename $i)

if ocamlbuild -classic-display -j 2 -package batteries -Is Data,Test $i.native > /dev/null \
  && ./$EXEC.native >2 /dev/null
then echo -e "$EXEC: \e[1m\e[32mok!\e[0m"
else echo -e "$EXEC: \e[1m\e[31mfail!\e[0m"
fi
rm -f $EXEC.native

done
