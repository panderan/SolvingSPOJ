main :: IO ()
main = 
    getLine >>= \x -> 
        if (read x::Int) == (42::Int) then return () 
                                      else putStrLn x >>= \_ -> main
