module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = prime n

prime :: Integer -> Bool
prime n = if n == 1 then False else isPrime n 2

isPrime :: Integer -> Integer -> Bool
isPrime m i
  | i * i > m = True
  | m `rem` i == 0 = False
  | otherwise = isPrime m (i + 1)

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 x = map (\d -> (d, factorize d x)) (primeDivisors x)

factorize :: Integer -> Integer -> Int
factorize divisor number
  | mod(number) divisor == 0 = 1 + factorize divisor (div(number) divisor)
  | otherwise = 0

primeDivisors :: Integer -> [Integer]
primeDivisors x = filter isPrimeDivisors (allDivisors x)

isPrimeDivisors :: Integer -> Bool
isPrimeDivisors 1 = False
isPrimeDivisors 2 = True
isPrimeDivisors n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

primes :: [Integer]
primes = 2 : filter isPrimeDivisors [3, 5 ..]


------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = sum (removeSelf n (allDivisors n)) == n

removeSelf :: Integer -> [Integer] -> [Integer]
removeSelf _ [] = []
removeSelf x (y : ys)
  | x == y = removeSelf x ys
  | otherwise = y : removeSelf x ys

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = sorting (allDivisors n)

sorting :: Ord a => [a] -> [a]
sorting [] = []
sorting (p : xs) = (sorting b) ++ [p] ++ (sorting c)
  where
    b = filter (< p) xs
    c = filter (>= p) xs

allDivisors :: Integer -> [Integer]
allDivisors 1 = [1]
allDivisors k =
  k :
  concatMap
    (\x -> [x] ++ if div(k) x == x then [] else [div(k) x])
    ( filter (\x -> mod(k) x == 0) $ takeWhile (\x -> x * x <= k) [2 ..])
    ++ [1]


------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product $ map count (words str)

count :: String -> Integer
count xs = toInteger (length (filter (== 'i') xs))

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 input = do
   let leftN =  read $ takeWhile (/= '-') input
   let rightM =  read $ takeWhile (/= ':') $ tail $ dropWhile (/= '-') input
   let parseStr = tail $ dropWhile (/= ' ') input
   getMaybeString leftN rightM parseStr

getMaybeString :: Int -> Int -> String -> Maybe String
getMaybeString leftN rightM parseString 
  | leftN > (length parseString)  || rightM > (length parseString) = Nothing
  | leftN > rightM = (Just (reverseStr (take leftN  $ drop (rightM - 1) parseString)))
  | otherwise = (Just (take rightM  $ drop (leftN - 1) parseString))


reverseStr :: [a] -> [a]  
reverseStr = foldl (\acc x -> x : acc) [] 

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = fractionalPortion (sqrt (1 + 8 * fromInteger num)) == 0
  where
    fractionalPortion x = x - fromIntegral (floor x)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 n = reversal n == n

reversal :: Integral a => a -> a
reversal = x 0
  where
    x a 0 = a
    x a b = let (q, r) = quotRem(b) 10 in x (a * 10 + r) q

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 a b = sum (allDivisors a) == a + b && sum (allDivisors b) == a + b

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 summ (h : t) = fixedCurrent h t
    where
        fixedCurrent :: Int -> [Int] -> Maybe (Int, Int)
        fixedCurrent _ [] = prob27 summ t
        fixedCurrent a (b : c) =
            if a + b == summ
            then Just (a, b)
            else fixedCurrent a c

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = maximum [x * y |x <- [a .. b],y <- [a .. b], (prob25 . toInteger)  (x * y)]
               where
                   a = 10 ^ (k - 1)
                   b = 10 ^ k - 1

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 k = head (filter (\t -> length (allDivisors t) >= k) triangleNumbers)

triangleNumbers :: [Integer]
triangleNumbers = map (\n -> n * (n + 1) `div` 2) [0..]

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
