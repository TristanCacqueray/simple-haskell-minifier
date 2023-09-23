module MoonLander where
pat = [0::Int,88..]
tup = (0, True)
step pos =
  let (x,_) = tup
      hit m n = abs (m - n) < 1
      (res,_) = (hit 0 x, hit 1 x)
   in (pos, res)
main
  | step 0 == (0, True) = pure ()
  | otherwise = putStrLn "Ooops"
