-- Victor Covalski 07/2017
--Types
data E = Num Int | TRUE | FALSE | Soma E E | Mult E E |
		 And E E | Not E | Or E E | If E E E | Leq E E
	deriving (Eq,Show)

data T = INT | BOOL
	deriving (Eq,Show)


--Typechecking rule
typeCheck:: E -> T
typeCheck (Num n) = INT
typeCheck TRUE  = BOOL
typeCheck FALSE = BOOL
typeCheck (Soma e1 e2) = case (typeCheck e1) of
						INT -> case (typeCheck e2) of
								INT -> INT
								x	-> error(show e2 ++ " has type " ++ show x ++ " and should have type INT")
						x	-> error (show e1 ++ " has type " ++ show x ++ " and should have type INT")
typeCheck (Mult e1 e2) = case (typeCheck e1) of
						INT -> case (typeCheck e2) of
								INT -> INT
								x	-> error(show e2 ++ " has type " ++ show x ++ " and should have type INT")
						x	-> error (show e1 ++ " has type " ++ show x ++ " and should have type INT")
typeCheck (And e1 e2) = case (typeCheck e1) of
						BOOL -> case (typeCheck e2) of
								BOOL -> BOOL
								x	-> error(show e2 ++ " has type " ++ show x ++ " and should have type BOOL")
						x	-> error (show e1 ++ " has type " ++ show x ++ " and should have type BOOL")
typeCheck (Or e1 e2) = case (typeCheck e1) of
						BOOL -> case (typeCheck e2) of
								BOOL -> BOOL
								x	-> error(show e2 ++ " has type " ++ show x ++ " and should have type BOOL")
						x	-> error (show e1 ++ " has type " ++ show x ++ " and should have type BOOL")
typeCheck (Not e)
				| typeCheck e == BOOL = BOOL
				| otherwise			  = error (show e ++ " should have type BOOL")
typeCheck (Leq e1 e2) = case (typeCheck e1) of
						INT -> case (typeCheck e2) of
								INT -> BOOL
								x	-> error(show e2 ++ " has type " ++ show x ++ " and should have type INT")
						x	-> error (show e1 ++ " has type " ++ show x ++ " and should have type INT")
typeCheck (If e1 e2 e3) = if ( typeCheck e1 == BOOL)
							then case (typeCheck e2) of
									INT -> case (typeCheck e3) of
											INT -> INT
											x	-> error(show e3 ++ " has type " ++ show x ++ " and should have type INT")
									BOOL -> case (typeCheck e3) of
											BOOL -> BOOL
											x	-> error(show e3 ++ " has type " ++ show x ++ " and should have type BOOL")
							else
								error (show e1 ++ " should have type BOOL")


--Tests
prog1:: E
prog1 = Soma (Soma (Num 2) (Num 5)) (Num 12)
-- INT
prog2:: E
prog2 = If (Leq (Num 3) (Num 5)) (Soma (Num 2) (Num 5)) (Mult (Num 2) (Num 2))
-- INT

prog3:: E
prog3 = If (Soma (Num 2) (Num 3)) (And TRUE TRUE) (Or FALSE FALSE)
-- *** Exception: Soma (Num 2) (Num 3) should have type BOOL

prog4:: E
prog4 = If (Or TRUE FALSE) (Num 2) FALSE
-- *** Exception: FALSE has type BOOL and should have type INT