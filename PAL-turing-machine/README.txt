**How to run**
Assuming you have Haskell installed along with ghci or Stack.
First load the environment by writing the following prompt in the terminal:
	> stack ghci turingMachine.hs
(NB: Remove 'stack' if you don't use Stack)

To run the script, simply type
    > main

Follow the instructions printed in the terminal.

**Description**
A simulation of a deterministic Turing Machine that decides if the input string
is a palindrome. The input string must be of the alphabet {a,b}^*. The TM
returns 'a' if true, 'b' otherwise. 
