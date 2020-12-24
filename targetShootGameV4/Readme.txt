To run the code, first adjest the input file name in the line " (let ((iport (open-input-file "targetShootObs1.txt")))" in targetShootGameV4.ergo.scm.
The file name should match the txt file one want to test.
The txt file contains all the exogenous actions and one can also modify the file to test differernt exmaples.

After setting the input file name, simply run "racket -l ergoExt -f targetShootGameV4.ergo.scm -m" in terminal to run the example.
