main :: IO()
main = getLine >>= \n -> go (read n :: Int)
    where go n = case n of
                    0 -> return ()
                    _ -> getLine >>= putStrLn . getRPN >>= \x -> go (n-1)

opts = [('#',0), ('(',1), (')',2), ('+',3), ('-',4), ('*',5), ('/',6), ('^',7)]

getRPN :: String -> String
getRPN infixstr = go infixstr ['#'] []
    where 
        go :: String -> [Char] -> [Char] ->[Char]
        go [] s1 s2 = s2 ++ reverse (tail s1)
        go infixstr s1 s2 
            | elem e ['a'..'z'] = go (tail infixstr) s1 (s2 ++ [e])
            | otherwise = go (tail infixstr) nxt_s1 (s2 ++ s2_append)
            where 
                e = head infixstr
                nxt_s1 = chk_s1 s1 e
                chk_s1 s1 e = case e of
                        '(' -> s1 ++ [e]
                        ')' -> if (last s1 == '(') then init s1 else chk_s1 (init s1) e
                        _ -> if (head [b | (a,b) <- opts, a == e] >= head [b | (a,b) <- opts, a == last s1]) then s1 ++ [e] else chk_s1 (init s1) e
                s2_append = reverse [a | (a,b) <- zipWith (\a -> \b -> (a,b)) (take max_length (s1++repeat '$')) (take max_length (nxt_s1++repeat '$')), a /= b , a /= '$', a /= '(']
                max_length = max (length s1) (length nxt_s1)

