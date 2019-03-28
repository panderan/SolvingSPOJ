main :: IO()
main = getLine >>= \x -> go (read x :: Integer)
    where go n = case n of
                    0 -> return ()
                    _ -> getLine >>= putStrLn . getTNP >>= \x -> go (n-1)

getTNP :: String -> String
getTNP in_str = go in_str (increase_pdm in_str 0)
    where
        go in_str out_str = if ((read out_str::Integer) > (read in_str::Integer)) then out_str
                            else go in_str (increase_pdm out_str 1)

increase_pdm :: String -> Integer -> String
increase_pdm str increment = if (remnum == 0) then increased_hstr ++ (reverse increased_hstr)
                             else increased_hstr ++ (reverse . init $ increased_hstr)
    where
        str' = if ([x | x <- str, x /= '9'] == []) then show ((read str::Integer) + 1)
               else str
        increment' = if ([x | x <- str, x /= '9'] == []) then increment - 1
                     else increment 
        remnum = rem (length str') 2
        hlen = (div (length str') 2) + remnum
        hstr = take hlen str'
        increased_hstr = show ((read hstr::Integer) + increment')
