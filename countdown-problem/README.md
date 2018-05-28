**How to run**
Assuming you have Haskell installed along with ghc or Stack.
Write the following prompt in the terminal:
	> Stack ghci countdown.hs
(NB: Remove 'Stack' if you don't use Stack)

To specify an integer array and integer target value and find solutions, type:
	> solutions' [x,y,z,...] k
where x,y,z,...,k are integers

There is also defined a main method that will run the script for the following input:
[1,3,7,10,25,50] 765.
Simply type
	> main
to run it.

**Problem description**
In the countdown problem a function (arithmetic expression) must be constructed
that calculates a given result from a given set of N numbers. All
numbers must be positive integers (including intermediate results), and each of
the source numbers can be used at most once when constructing the expression.

For example, given the integer array: [1, 3, 7, 10, 25, 50] and arithmetic
operators: [+, −, ∗, /], construct an expression whose value is: 765.
One possible solution is: ( 25 − 10 ) ∗ ( 50 + 1 ) = 765.

Given this, implement Haskell functions that solve the countdown problem
as efficiently as possible. That is, the solution should reject expressions that
fail to evaluate and not evaluate redundant expressions (exploiting algebraic
properties to reduce the number of generated expressions). Assume only the
following operators are used in constructing expressions: [+, −, ∗, /] from a list
of N positive integers and one positive integer value that must be calculated.
