This is the readme file for the extension of SW. The original readme documentation contains more detail about the model
are packed in the original documentation folder.

New files added to the SW are : 

controlled_squirrel.scm : Our controlled agent, has plan recognition and degree of belief features.
controlled-bridege.scm : The bridge used between the controlled agent and the SW server
squirrel1-main.scm: The main code for the first observed agent
squirrel2-main.scm: The main code for the second observed agent
squirrel2-bat.scm: The basic action theory code for the second observed agent
squirrel2-procs.scm: Other help function for the second observed agent
squirrel3-main.scm: The main code for the third observed agent
squirrel3-bat.scm: The basic action theory code for the third observed agent
squirrel3-procs.scm: Other help function for the third observed agent
target-bridge.scm The bridge used between the observed agent and SW server


To Run this example, one need three terminal windows. 
Please follow the instruction below:

0. first move to the SW folder

1. create the server for graphic interface in the first window using the following line:

racket -fm sw-server.scm

2. Use second terminal call the controlled  agent 

racket -l ergoExt -f controlled_squirrel -m

3. Use third terminal call the observed agent, note there are 3 choice for the observed agent 

racket -l ergoExt -f squirrel1-main.scm -m
or
racket -l ergoExt -f squirrel2-main.scm -m
or
racket -l ergoExt -f squirrel3-main.scm -m


The controlled agent will print out the degree of believes of some fluents and it will take 3 different reaction 
against three observed agents.

