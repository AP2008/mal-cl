main: 
	sbcl --non-interactive --eval "(progn (load \"step9_try.asd\") (asdf:operate :build-op \"step9_try\"))"
