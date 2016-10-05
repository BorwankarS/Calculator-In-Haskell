{-
  Name: Saurabh Borwankar
  Date: 02/15/2016
  Description: This program performs arithmatic operations like addition,subtraction, multiplication and powerof on big numbers.
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] carry = if carry == 1 then [1] else []
bigAdd' (x:xs) ([]) 0 = [x] ++ bigAdd' xs [] 0
bigAdd' (x:xs) ([]) 1 = if x+1 >999 then [(x+1) `mod` 1000 ] ++ bigAdd' xs [] 1 else [(x+1)] ++ bigAdd' xs [] 0
bigAdd' ([]) (y:ys) 0 = [y] ++ bigAdd' [] ys 0
bigAdd' ([]) (y:ys) 1 = if(y+1)>999 then [(y+1) `mod` 1000 ] ++ bigAdd' [] ys 1 else [(y+1)] ++ bigAdd' [] ys 0
bigAdd' (x:[]) (y:[]) 0 = if x+y >999 then [ (x + y) `mod` 1000 ] ++ bigAdd' [] [] 1 else [ x+y ]
bigAdd' (x:[]) (y:[]) 1 = if x+y > 999 then [(x + y + 1) `mod` 1000 ] ++ [1] else [ x + y + 1 ]
bigAdd' (x:xs) (y:ys) z = if x + y > 999 then [(x + y + z) `mod` 1000 ] ++ (bigAdd' xs ys 1) else [ x + y + z ] ++ ( bigAdd' xs ys 0)


bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] _ = error "Empty List"
bigSubtract' ([x]) [] 1 = [x-1]
bigSubtract' ([x]) [] 0 = [x]
bigSubtract' ([x]) ([y]) 0 = if x-y < 0 then error "Negative numbers not supported " else [ x - y ]
bigSubtract' ([x]) ([y]) 1 = if (x-y-1) < 0 then error "Negative numbers not supported" else [ (x - y - 1) ]
bigSubtract' (x:xs) (y:ys) z = if x - y - z < 0 then [ (x - y - z + 1000) ] ++ (bigSubtract' xs ys 1) else [ x - y - z] ++ (bigSubtract' xs ys 0)

bigEq :: BigNum -> BigNum -> Bool
bigEq [] [] = error "Empty List"
bigEq (x:[]) (y:[]) = if x == y then True else False
bigEq (x:xs) (y:ys) = if x == y then bigEq xs ys else False

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply ([])(ys) = []
bigMultiply (x:[])(ys) = bigMultiply' x ys 0
bigMultiply (x:xs)(ys) = bigAdd ( [0]++(bigMultiply xs ys) ) ( bigMultiply' x ys 0)

bigMultiply' :: Block -> BigNum -> Block -> BigNum
bigMultiply' (x)([]) 0 = []
bigMultiply' (x)([]) z = [z `mod` 1000] ++ bigMultiply' x [] (z `div` 1000)
bigMultiply' (x)(y:ys) z = [( ( ( x * y ) + z ) `mod` 1000)] ++ ( bigMultiply' x ys ( ( ( x * y ) + z ) `div` 1000))

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf [] [] = error "Empty List"
bigPowerOf x [0] = [1]
bigPowerOf x [1] = x
bigPowerOf x y = bigMultiply x (bigPowerOf x (bigDec y))
--bigPowerOf _ _ = error "Your code here"

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]
