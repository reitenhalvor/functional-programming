-- THE COUNTDOWN PROBLEM --
{-

Given a list of integers and a target value, combine these into an expression
using the arithmetic operators addition, subtraction, multiplication and division
s.t. the expression evaluates to the target value.
Ex.: [1,2,3,4,5] can be combined to (1+2)*(3+5)/4 to get 6

-}

-- declaring data types for the arithmetic operations
data Operation = Add | Sub | Mul | Div
  deriving (Eq)

operations :: [Operation]
operations = [Add, Sub, Mul, Div]

-- defining the string representation of 'Operations'
instance Show Operation where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

-- defining the application of the operations through applyOp
-- applyOp Add 1 2 = (+) 1 2 = 3
applyOp :: Operation -> Int -> Int -> Int
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = div

-- validOp defines what is "legal" input
validOp :: Operation -> Int -> Int -> Bool
validOp Add _ _ = True
validOp Sub x y = x > y
validOp Mul _ _ = True
validOp Div x y = x `mod` y == 0

-- declaring data types for expressions.
-- either of integer value or an application of an operation with two expressions
-- as input.
data Expr = Val Int | Apply Operation Expr Expr

-- defining the string representation of 'Expr'
instance Show Expr where
  show (Val num) = show num
  show (Apply operation a b)
        | operation `elem` [Add, Sub] = "(" ++ show a ++ show operation ++ show b ++ ")"
        | otherwise = show a ++ show operation ++ show b

-- getExprValues will return a list all numbers used in an expression Expr
getExprValues :: Expr -> [Int]
getExprValues (Val num) = [num]
getExprValues (Apply _ a b) = getExprValues a ++ getExprValues b

-- evaluateExpr will evaluate an expression and return a list with the result
-- if the list is empty, failure to evaluate the expr, but a singleton list
-- denotes that the expression was evaluated successfully.
evaluateExpr :: Expr -> [Int]
evaluateExpr (Val num) = [num | num > 0]
evaluateExpr (Apply op a b) =
      [applyOp op x y | x <- evaluateExpr a, y <- evaluateExpr b, validOp op x y]


--- ======================= ---
--- COMBINATORIAL FUNCTIONS ---
--- ======================= ---

-- subseqs returns a list containing all subsequences of the input list
-- subseqs [1,2] = [[1],[2],[1,2]]
subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) =
    (subseqs xs) ++ map (x:) (subseqs xs)

-- poss_insertions returns all possible ways of inserting a new element into a list
poss_insertions :: a -> [a] -> [[a]]
poss_insertions n [] = [[n]]
poss_insertions n (x:xs) =
    (n:x:xs) : (map (x:) (poss_insertions n xs))

-- poss_permutations returns all possible permutations of a list
poss_permutations :: [a] -> [[a]]
poss_permutations [] = [[]]
poss_permutations (x:xs) =
    concat (map (poss_insertions x) (poss_permutations xs))

-- poss_subseqs combines subseqs and poss_permutations and returns all possible subseqences
poss_subseqs :: [a] -> [[a]]
poss_subseqs xs = [y | x <- subseqs xs, y <- poss_permutations x]


-- listContainsList returns True iff all elements in one list exists in the other
listContainsList :: Eq a => [a] -> [a] -> Bool
listContainsList [] _ = True
listContainsList (x:xs) ys
        | x `elem` ys = listContainsList xs ys
        | otherwise = False


-- poss_splits returns all possile splits of a list
-- poss_splits [1,2,3] = [([1],[2,3]),([1,2],[3])]
poss_splits :: [a] -> [([a],[a])]
poss_splits [] = []
poss_splits [_] = []
poss_splits (x:xs) =
    ([x],xs) : [(x:l1,l2) | (l1,l2) <- poss_splits xs]


--- ================== ---
--- FINDING A SOLUTION ---
--- ================== ---
-- Will now find a solution by brute force, not worrying about efficiency.

-- isSolution returns True if a given expression can solve the problem
-- expression is a solution for a given list og numbers and a target value
isSolution :: Expr -> [Int] -> Int -> Bool
isSolution expr input_list target = listContainsList (getExprValues expr) (input_list)
                                    && evaluateExpr expr == [target]

-- getAllExpressions returns all possible expressions, e.g. all possible ways
-- of combining elements in a list.
-- empty list = no possible expressions
-- single number n = a single expression comprimising n
-- multiple numbers [n] = first produce all splits with 'split', then recursively
  -- calculate all possible expressions for each of the lists, until we finally
  -- combine each pair of expressions.
getAllExpressions :: [Int] -> [Expr]
getAllExpressions [] = []
getAllExpressions [n] = [Val n]
getAllExpressions xs =
    [exprs | (l1,l2) <- poss_splits xs, e1 <- getAllExpressions l1,
                                        e2 <- getAllExpressions l2,
                                        exprs <- combineExprs e1 e2]

-- combineExprs will combine two expressions to one in a list
combineExprs :: Expr -> Expr -> [Expr]
combineExprs e1 e2 = [Apply op e1 e2 | op <- operations]


-- finds all possible solutions to the problem
solutions :: [Int] -> Int -> [Expr]
solutions xs target =
    [expr | xs' <- poss_subseqs xs, expr <- getAllExpressions xs', isSolution expr xs target]



--- ======================== ---
--- MAKING IT MORE EFFECTIVE ---
--- ======================== ---

-- Can exploit algebraic properties to make it more effective.
-- x * y = y * x; x / 1 = x; x * 1 = x; x + y = y + x;
-- Therefore, making a new validate function that considers this

valid_eff :: Operation -> Int -> Int -> Bool
valid_eff Add x y = x <= y
valid_eff Sub x y = x > y
valid_eff Mul x y = x /= 1 && y /= 1 && x <= y
valid_eff Div x y = y /= 1 && x `mod` y == 0


-- Can also make it more effective by combining expressions with their
-- resulting numerical value from evaluateExpression.
-- Using this, expressions that fail to evaluate are rejected at an early stage,
-- s.t. the program don't spend time playing with it.

-- making a new type that stores both the expression and the resulting value
type ExprVal = (Expr, Int)

-- getExprVals work a lot like getAllExpressions, just that we're now calculating
-- objects of type ExprVal.
getExprVals :: [Int] -> [ExprVal]
getExprVals [] = []
getExprVals [n] = [(Val n, n) | n > 0]
getExprVals ns =
    [res | (l1,l2) <- poss_splits ns,
                    l <- getExprVals l1,
                    r <- getExprVals l2,
                    res <- combineExprs' l r]

-- We also need to redefine combineExprs to combineExprs'
-- The same, but now checking for validity and working with objects of type ExprVal
combineExprs' :: ExprVal -> ExprVal -> [ExprVal]
combineExprs' (e1,x1) (e2,x2) =
    [(Apply op e1 e2, applyOp op x1 x2) | op <- operations, valid_eff op x1 x2]


solutions' :: [Int] -> Int -> [Expr]
solutions' xs target =
    [expr | xs' <- poss_subseqs xs, (expr,val) <- getExprVals xs', val == target]


--- ==== ---
--- MAIN ---
--- ==== ---

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)
