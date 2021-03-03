module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 colorPart = x >= 0 && x <= 255
    where x = prob9 colorPart

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color colorPart = case colorPart of
  Red x -> color { red = red color + x }
  Blue x -> color { blue = blue color + x }
  Green x -> color { green = green color + x }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 colorPart = case colorPart of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color
  | red color > green color && red color > blue color = Just (Red (red color))
  | green color > blue color && green color > red color = Just (Green (green color))
  | blue color > green color && blue color > red color = Just (Blue (blue color))
  | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = leftSum + (root tree) + rightSum
    where
        leftSum = maybe 0 prob11 (left tree)
        rightSum = maybe 0 prob11 (right tree)

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 = checkTree

checkTree :: Ord a => Tree a -> Bool
checkTree tree = checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
checkRight Nothing x = True
checkRight (Just tree) parent = root tree >= parent && checkTree tree

checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
checkLeft Nothing x = True
checkLeft (Just tree) parent = root tree < parent && checkTree tree

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 a tree = findTree a (Just tree)

findTree :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
findTree a Nothing = Nothing
findTree a (Just tree)
  | a > root tree = findTree a (right tree)
  | a < root tree = findTree a (left tree)
  | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 t = case enumerating (Just t) 1 of
    (Just enumerated, _) -> enumerated

enumerating :: Maybe (Tree ()) -> Int -> (Maybe (Tree Int), Int)
enumerating Nothing i = (Nothing, i)
enumerating (Just (Tree l () r)) i = (Just $ Tree l' current r', current + 1)
    where
        (r', afterRight) = enumerating r i
        (l', afterLeft) = enumerating l afterRight
        current = afterLeft

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree rotateLeft (right tree)
  where rotateLeft subtree = subtree {left = Just tree {right = left subtree}}

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rotateRight (left tree)
  where rotateRight subtree = subtree {right = Just tree {left = right subtree}}

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 tree = case buildBalanced (toList tree) of
                   Just a -> a
                   Nothing -> tree
 
buildBalanced :: [a] -> Maybe (Tree a)
buildBalanced [] = Nothing
buildBalanced elts =
  Just (Tree
    (buildBalanced $ take half elts)
    (elts !! half)
    (buildBalanced $ drop (half + 1) elts))
  where
    half = length elts `quot` 2 

toList :: Tree a -> [a]
toList tree = maybeToList (left tree) ++ [root tree] ++ maybeToList (right tree)
  where
    maybeToList (Just x) = toList x
    maybeToList Nothing = []