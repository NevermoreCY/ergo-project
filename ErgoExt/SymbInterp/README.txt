This directory contains code for an experimental interpreter that works on the symbolic form of ERGO programs (as opposed to the macro-expanded form). The main function (ergo-trans-match a prog) returns the (first) remaining program after action a is executed by program prog proviede it can execute a, and otherwise #f.

This is work in progress and not all ERGO constructs are yet handled.  Also, Scheme constructs in ERGO programs (except in test conditions) are not yet handled.

 - ergo-trans-match.scm      the interpreter
 - ergo-trans-match-tester   a tester for the interpreter
 
