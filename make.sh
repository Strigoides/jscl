#!/bin/sh

if [ $1 = clean ]; then
	rm -f *.jso src/*.jso tests/*.jso
else
	sbcl --load 'jscl.lisp' --eval '(jscl:bootstrap)' --eval '(quit)'
fi
