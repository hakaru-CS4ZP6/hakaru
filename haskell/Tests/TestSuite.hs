module Main(main) where

import System.Exit (exitFailure)
import System.Environment (lookupEnv)

import qualified Tests.ASTTransforms as TR
import qualified Tests.Parser        as P
import qualified Tests.Pretty        as Pr
import qualified Tests.TypeCheck     as TC
import qualified Tests.Simplify      as S
import qualified Tests.Disintegrate  as D
import qualified Tests.Sample        as E
import qualified Tests.RoundTrip     as RT
import qualified Tests.Relationships as REL

import Test.HUnit

-- master test suite

ignored :: Assertion
ignored = putStrLn "Warning: maple tests will be ignored"

simplifyTests :: Test -> Maybe String -> Test
simplifyTests t env =
  case env of
    Just _  -> t
    Nothing -> test ignored

allTests :: Maybe String -> Test
allTests env = test $
  [ TestLabel "RoundTrip"    (simplifyTests RT.allTests env)
  ]

main :: IO ()
main = mainWith allTests (fmap Just . runTestTT)

mainWith :: (Maybe String -> Test) -> (Test -> IO (Maybe Counts)) -> IO ()
mainWith mkTests run = do
    env <- lookupEnv "LOCAL_MAPLE"
    run (mkTests env) >>=
      maybe (return ()) (\(Counts _ _ e f) -> if (e>0) || (f>0) then exitFailure else return ())
