This is the readme file for the extension of SW. The original readme documentation contains more detail about the model
are packed in the original documentation folder.

Extended new files are simple-main-2.scm , sw-bridge-2 and reactive1.scm.


To run this simple example, one need to run the following lines in terminal 

0. first move to the SW folder

1. create the server for graphic interface

racket -fm sw-server.scm

2. Use another terminal call the reactive agent 

racket -l ergoExt -f reactive1.scm -m

3. Use another terminal call the simple agent 

racket -l ergoExt -f simple-main-2.scm -m

Or use the following line to call the store squirrel 

racket -l ergoExt -f systematic-main.scm -m

The reactive agent can now take reaction once it recognize the target is "store squirrel" which is initialized by "racket -l ergoExt -f systematic-main.scm -m".
It will take action to steal the acorn and win.
