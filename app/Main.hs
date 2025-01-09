{-# LANGUAGE DeriveFunctor #-}

module Main where

import Parser 
import Eval 
import Control.Monad.Free 
import Text.Megaparsec
import Text.Megaparsec.Error 
import Data.Void (Void) 
import Data.Either (fromRight)
import System.IO 
import Control.Monad
import System.Exit

data CalcF next = 
    GetUserInput (String -> next)
    | ParseUserInput String (String -> next)
    | EvalProp       Proposition (Proposition -> next)
    | ShowEval       Bool next 
    deriving (Functor)
data IfExit = DoExit
            | DontExit 
            deriving (Eq)

type Calculator = Free CalcF 

getUserInput :: Calculator String 
getUserInput = liftF $ GetUserInput id

parseUserInput :: String -> Calculator (Either (ParseErrorBundle String Void) (Either CalcCommand Proposition)) 
parseUserInput s = liftF $ ParseUserInput s (parse parseCalcInput "")

evalProp :: Proposition -> Calculator Bool 
evalProp prop = liftF $ EvalProp prop eval

showEval :: Bool -> Calculator () 
showEval bool = liftF $ ShowEval bool () 

calculator :: Calculator IfExit
calculator = do 
    input         <- getUserInput 
    eitherCalcIn  <- parseUserInput input 
    case eitherCalcIn of 
        Left parseerror -> calculator
        Right r         -> 
            case r of 
                Left _ -> return DoExit
                Right r' -> do 
                  evaluation    <- evalProp r'
                  showEval evaluation 
                  return DontExit

runCalculator :: Calculator a -> IO a 
runCalculator calc = 
    case calc of 
        (Pure a) -> return a 
        (Free f) -> 
            case f of 
                (GetUserInput next) -> do 
                    putStr "propmagic: "
                    hFlush stdout 
                    input <- getLine 
                    runCalculator $ next input 
                (ParseUserInput s next) -> do 
                    --putStrLn "Parsing..."
                    runCalculator $ next s 
                (EvalProp prop next)    -> do 
                    -- putStrLn "Evaluating"  
                    runCalculator $ next prop 
                (ShowEval b next)       -> do 
                    putStr "Evaluation: "
                    hFlush stdout 
                    putStrLn $ show b 
                    runCalculator next

                    
main :: IO ()
main = do 
    exit <- runCalculator calculator
    if exit == DoExit then exitSuccess else main 
