data E = Num Int | Soma E E | Mult E E
	deriving (Eq,Show)

data T = INT
	deriving (Eq,Show)

typeCheck:: E -> T
typeCheck (Num n) = INT
typeCheck (Soma e1 e2) 
				| typeCheck e1 == INT = typeCheck e2
				| otherwise		      = error (show e1 ++ "Should have type INT")
typeCheck (Mult e1 e2)
				| typeCheck e1 == INT = typeCheck e2
				| otherwise			  = error (show e1 ++ "Should have type INT")


prog1:: E
prog1 = Soma (Soma (Num 4) (Num 5)) (Num 12)