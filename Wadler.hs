-- here 'Con' stands for 'constnat'
data Term = Con Int | Div Term Term deriving (Show)

-- part 1
eval 		:: Term -> Int
eval (Con a)	= a
eval (Div t u)	= eval t `quot` eval u

-- part 2
data M a	= Raise Exception | Return a deriving (Show)
type Exception 	= String

eval'		:: Term -> M Int
eval' (Con a)	= Return a
eval' (Div t u)	= case eval' t of 
			Raise e -> Raise e
			Return a ->
				case eval' u of
					Raise e -> Raise e
					Return b ->
						if b == 0
							then Raise "divide by zero"
							else Return (a `quot` b)
-- part 3
type S a = State -> (a, State)
type State = Int


eval''			:: Term -> S Int
eval'' (Con a) x 	= (a, x)
eval'' (Div t u) x 	= let (a, y) = eval'' t x 
			      (b, z) = eval'' u y in
			  (a `quot` b, z + 1)

main = do
	-- answer, error:: Term
	let answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
	let error = (Div (Con 1 ) (Con 0 ))
	print answer
	print error 

	let a = eval answer
	let e = eval error
	print a
	print e
