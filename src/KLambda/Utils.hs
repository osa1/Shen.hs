module KLambda.Utils where

import qualified Data.Vector.Mutable as MV

toList :: MV.IOVector a -> IO [a]
toList vec = iter vec 0
  where
    iter :: MV.IOVector a -> Int -> IO [a]
    iter vec n =
      if n < MV.length vec
        then do
          e    <- MV.unsafeRead vec n
          rest <- iter vec (n+1)
          return (e:rest)
          else return []
