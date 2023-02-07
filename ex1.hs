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

pack :: Eq a => [a] -> [a]
pack [] = []
pack [x] = [x]
pack (x:(y:xs))
  | x == y = [x, y]