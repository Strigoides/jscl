#!/bin/sh

if [ $# -gt 1 ]; then
	echo "Too many arguments '$@'" >&2
	exit 1
elif [ $# -eq 1 ]; then
	case "$1" in
		clean)
			rm -f *.jso src/*.jso tests/*.jso
			exit
			;;
		verbose)
			vars='(setf jscl::*verbosity* t)'
			;;
		*)
			echo "Illegal option '$1'" >&2
			exit 1
	esac
fi

sbcl --load 'jscl.lisp' --eval "(progn $vars (jscl:bootstrap) (quit))"
