module Main

import System
import Data.Vect

data VectUnknown : Type -> Type where
     MkUnknownVect : (len : Nat) -> Vect len a -> VectUnknown a

Show (VectUnknown String) where
     show (MkUnknownVect len v) = show v

total
countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

readNumber : String -> Maybe Nat
readNumber input = case all isDigit (unpack input) of
                   False => Nothing
                   True => Just (cast input)

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

EmptyUnlimitedVect : VectUnknown String
EmptyUnlimitedVect = (MkUnknownVect _ [])

unlimitedVectLen' : VectUnknown String -> IO (VectUnknown String)
unlimitedVectLen' (MkUnknownVect len v) = do x <- getLine
                                             if ( x == "") then pure (MkUnknownVect _ v)
                                                           else unlimitedVectLen' (MkUnknownVect _ (x :: v)) 

unlimitedVectPrint : VectUnknown String -> IO ()
unlimitedVectPrint x = do (printLn x)

||| (>>=) (unlimitedVectLen' EmptyUnlimitedVect) (\v => unlimitedVectPrint v)
unlimitedVectTest : IO ()
unlimitedVectTest = do putStrLn "Enter the sequentially strings and press Enter when input will be completed:"
                       v <- (unlimitedVectLen' EmptyUnlimitedVect)
                       unlimitedVectPrint v

readVect' : IO (n : Nat ** Vect n String)
readVect' = do x <- getLine
               if (x == "") then pure (_ ** [])
                            else do (n' ** xs) <- readVect'
                                    pure ((S n') ** (x :: xs))

mypair : (Int, String)
mypair = (94, "Pages")

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])

zipInputs : IO ()
zipInputs = do putStrLn "Input first vector: "
               (len1 ** xs) <- readVect'
               putStrLn "Input second vector: "
               (len2 ** ys) <- readVect'
               case exactLength len1 ys of
                    Nothing => putStrLn ("Vectors has been with equal length. " ++ (show len1) ++ " != " ++ (show len2))
                    Just zs => printLn (zip xs zs)

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k  (next + acc)

main : IO ()
main = do
     x <- getLine
     case readNumber x of
          Nothing => putStrLn ("Wrong input: " ++ (the String x))
          Just n => putStrLn ("Number is " ++ (show n))
