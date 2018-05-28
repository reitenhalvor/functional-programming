import Data.Tuple

--- INSTRUCTIONS ----
{-
There is defined a function <f :: String -> Symbol> that returns the symbol.
An error will occour if other characters than {a,b} are inputted.

There is also defined a main :: IO() that can be used to iteratively input words.
You are free to choose if you wish to access the method f directly, or if you wish
to utilize the main method.
-}

-- declaring data types
type State = Int
data Symbol = A | B | Blank
  deriving (Eq, Ord)

data Direction = I | R | L
  deriving (Eq)

-- tape consisting of symbols
data Tape = Tape {  leftSyms :: [Symbol],   -- symbols left of tape head
                    currSym :: Symbol,      -- symbols under tape head
                    rightSyms :: [Symbol]   -- symbols right of tape head
                  }

-- defines a static Turing machine
data TM = TM {  q_init :: State,
                q_accept :: State,
                q_reject :: State,
                transitions :: State -> Symbol -> (State, Symbol, Direction)
              }

-- RunningTM defines a running machine on a tape with a given state
data RunningTM = RunningTM {  static_tm :: TM,
                              tape :: Tape,
                              state :: State,
                              halted :: Bool
                            }

-- declaring show instances
instance Show Symbol where
  show A = "a"
  show B = "b"
  show Blank = "X"

instance Show Tape where
  show (Tape left curr right) = show left ++ " <- " ++ show curr ++ " -> " ++ show right

instance Show TM where
  show (TM q_init q_acc q_rej trans) = "(q_init: " ++ show q_init ++ ", q_accept:" ++ show q_acc ++ ", q_reject:" ++ show q_rej ++ ")"

instance Show RunningTM where
  show (RunningTM tm tape state halt) = "Tape: " ++ show tape ++ "\nState: q" ++ show state ++ ", Halt: " ++ show halt

-- redefining some functions to my needs
-- tail to handle []
tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

-- init to handle []
init' :: [a] -> [a]
init' [] = []
init' xs = take (length xs - 1) xs

--redefine tuple accessors for 3-tuples
frst3 (a, _, _) = a
scnd3 (_, a, _) = a
thrd3 (_, _, a) = a

-- defining functions
-- updates a given tape by writing symbol and moving in direction
updateTape :: Tape -> Symbol -> Direction -> Tape
updateTape tape sym dir
    | dir == L = Tape { leftSyms = init' l,
                        currSym = if null l then sym else last l,
                        rightSyms = [sym] ++ r
                      }
    | dir == R = Tape { leftSyms = l ++ [sym],
                        currSym = if null r then sym else head r,
                        rightSyms = tail' r
                      }
    | otherwise = tape
    where l = leftSyms tape
          r = rightSyms tape

-- takes a string as input and returns the equivalent in Symbols, if all valid
stringToSymbols :: String -> [Symbol]
stringToSymbols [] = []
stringToSymbols (x:xs)
    | x == 'a' = A : stringToSymbols xs
    | x == 'b' = B : stringToSymbols xs
    | otherwise = error "Only {a,b} are valid symbols" -- then it is invalid

-- makes a Tape out of a String, adding 5 blank symbols in left and right end
makeTape :: String -> Tape
makeTape str = Tape {  leftSyms = (replicate 5 Blank),
                       currSym = if null syms then Blank else head syms,
                       rightSyms = tail' syms ++ (replicate 5 Blank)
                    }
                    where
                      syms = stringToSymbols str

-- advance a TM one step
advanceTM :: RunningTM -> RunningTM
advanceTM (RunningTM tm tape currState halted) =
    if (halted)
      then
        RunningTM tm tape currState halted -- return the input
      else
        RunningTM tm updated_tape new_state shouldHalt
      where
        next = (transitions tm) currState (currSym tape)
        new_state = frst3(next)
        write_sym = scnd3(next)
        move_dir = thrd3(next)

        updated_tape = updateTape tape write_sym move_dir
        shouldHalt = new_state == (q_accept tm) || new_state == (q_reject tm)


-- runTM will recursively call advanceTM and return a True if end state is q_accept, else False
runTM :: RunningTM -> Bool
runTM runningtm =
    if (halted runningtm)
      then (state runningtm) == (q_accept (static_tm runningtm))
    else
      runTM (advanceTM runningtm) -- continue


-- **** defining a specific TM **** --
-- this TM will check if input is a palindrome

-- defining all the states and transitions
transitions_palindrome :: State -> Symbol -> (State, Symbol, Direction)
transitions_palindrome q s
  | q == 0 && s == A = (1, Blank, R)
  | q == 0 && s == B = (2, Blank, R)
  | q == 0 && s == Blank = (6, Blank, I) -- accept

  | q == 1 && s == A = (1, A, R)
  | q == 1 && s == B = (1, B, R)
  | q == 1 && s == Blank = (3, Blank, L)

  | q == 2 && s == A = (2, A, R)
  | q == 2 && s == B = (2, B, R)
  | q == 2 && s == Blank = (4, Blank, L)

  -- check if right end is A, else reject
  | q == 3 && s == A = (5, Blank, L)
  | q == 3 && s == B = (7, B, I)
  | q == 3 && s == Blank = (6, Blank, I) -- accept

  -- check if right end is B, else reject
  | q == 4 && s == A = (7, A, I)
  | q == 4 && s == B = (5, Blank, L)
  | q == 4 && s == Blank = (6, Blank, I) -- accept

  -- move back to start
  | q == 5 && s == A = (5, A, L)
  | q == 5 && s == B = (5, B, L)
  | q == 5 && s == Blank = (0, Blank, R)

  -- did find palindrome, halt
  | q == 6 && s == A = (6, A, I)
  | q == 6 && s == B = (6, B, I)
  | q == 6 && s == Blank = (6, Blank, I)

  -- did find palindrome, halt
  | q == 7 && s == A = (7, A, I)
  | q == 7 && s == B = (7, B, I)
  | q == 7 && s == Blank = (7, Blank, I)

-- defining the TM
tm_palindrome = TM {  q_init = 0,
                      q_accept = 6,
                      q_reject = 7,
                      transitions = transitions_palindrome
                   }

-- defining a function f that takes a String as input
f :: String -> Symbol
f word = if bol then A else B
  where bol = runTM (RunningTM tm_palindrome (makeTape word) (q_init tm_palindrome) False)

main :: IO()
main = do
    putStrLn "Enter word to test if palindrome (q to exit): "
    word <- getLine
    if word == "q"
      then return ()
      else do
        print (f word)
        main
