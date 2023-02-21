-- Attempt to solve excellent OCaml exercises:
-- https://ocaml.org/problems


last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

lastTwo :: [a] -> Maybe [a]
lastTwo [] = Nothing
lastTwo [x, y] = Just [x, y]
lastTwo (x:xs) = lastTwo xs

listNth :: [a] -> Int -> Maybe a
listNth [] _ = Nothing
listNth [x] 0 = Just x
listNth [x] _ = Nothing
listNth (x:xs) k = if k == 0 then Just x else listNth xs (k-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = reverse' x == x

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:xs))
  | x == y = compress (y:xs)
  | otherwise = x : compress (y:xs)


packSub :: Eq a => [a] -> [a] -> [[a]]
packSub [] [x, y]
  | x == y = [[x, y]]
  | otherwise = [[x], [y]]
packSub acc [x, y]
  | x == y = [acc ++ [x, y]]
  | otherwise = [acc ++ [x], [y]]
packSub [] (x:(y:xs))
  | x == y = packSub [x] (y:xs)
  | otherwise = [x] : packSub [] (y:xs)
packSub acc (x:(y:xs))
  | x == y = packSub (acc ++ [x]) (y:xs)
  | otherwise = (acc ++ [x]) : packSub [] (y:xs)

pack :: Eq a => [a] -> [[a]]
pack = packSub []
