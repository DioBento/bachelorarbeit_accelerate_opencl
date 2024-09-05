-- testing the cli tool argon

main :: IO ()
main = do
  let x = test1 6
  test1 4
  test2 25
  test2 14
  test2 1
  let a = test2 x > 5 ? (1, 0)
  putStrLn "Done."

test1 x = if x > 5
          then x `div` 2
          else x

test2 x | x > 20    = x `div` 4
        | x >  5    = x `div` 2
        | otherwise = x

(?) :: Bool -> (a, a) -> a
cond ? (a, b) | cond      = a
              | otherwise = b
