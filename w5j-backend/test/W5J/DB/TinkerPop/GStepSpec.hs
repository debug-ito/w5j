module W5J.DB.TinkerPop.GStepSpec (main,spec) where

import Language.Haskell.Interpreter
  ( loadModules, OptionVal((:=)), set, searchPath,
    setTopLevelModules, runInterpreter, InterpreterError,
    typeChecks
  )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Logic typeclass" $ do
  checkLogicCompatible "Filter" "Filter" True

toErrString :: Either InterpreterError a -> Either String a
toErrString (Right a) = Right a
toErrString (Left e) = Left $ show e

checkLogicCompatible :: String -> String -> Bool -> Spec
checkLogicCompatible child parent expected = specify label $ doCheck
  where
    label = child ++ " -> " ++ parent
    doCheck = do
      result <- fmap toErrString $ runInterpreter isTypeOK
      result `shouldBe` Right expected
    isTypeOK = do
      set [searchPath := ["src"]]
      loadModules ["src/W5J/DB/TinkerPop/GStep.hs"]
      setTopLevelModules ["W5J.DB.TinkerPop.GStep"]
      typeChecks code
    code = "let f :: GStep " ++ child ++ " s s -> GStep " ++ parent ++ " s s; "
           ++ "f = gFilter; "
           ++ "child :: GStep " ++ child ++ " s s; "
           ++ "child = undefined; "
           ++ "in f child"
