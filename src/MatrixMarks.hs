module Main where

import Criterion.Main
import Data.Packed.Matrix as M
import Numeric.LinearAlgebra.Algorithms   as H  -- hmatrix
import Numeric.LinearAlgebra.Matrix       as LA -- lin-alg
import Numeric.LinearAlgebra.Matrix.Mat44       -- lin-alg

main = do
  let m = (4><4)[1,0,0,0
                ,0,1,0,0
                ,0,0,1,0
                ,0,0,0,1 :: Double]
  let n = Mat44 1 0 0 0
                0 1 0 0
                0 0 1 0
                0 0 0 (1 :: Double)
  putStrLn $ "hmatrix/det:" ++ show (H.det m)
  putStrLn $ "haray/det:" ++ show (LA.det n)
  defaultMain
    [ bgroup "det" [ bench "hmatrix" $ whnf H.det m
                   , bench "haray" $ whnf LA.det n]
    , bgroup "inv" [ bench "hmatrix" $ whnf H.inv m
                   , bench "haray" $ whnf LA.inv44 n]
    ]
