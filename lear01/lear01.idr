module Main

import Data.Vect

%default total

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car _) = 4
wheels (Bus _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus _) = Bus 200

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip' : Vect n a -> Vect n b -> Vect n (a , b)
zip' [] [] = []
zip' (x :: xs) (y :: ys) = (x, y) :: zip' xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x l = case integerToFin x n of
                   Nothing => Nothing
                   Just f => Just (index f l)

vectTake : (m : Fin n) -> Vect n a -> Vect (finToNat m) a
vectTake FZ (x :: xs) = []
vectTake (FS y) (x :: xs) = x :: vectTake y xs

sumEntries : Num a => (m : Fin n) -> Vect n a -> Vect n a -> a
sumEntries FZ (x :: xs) (y :: ys) = (x + y)
sumEntries (FS x) (y :: xs) (z :: ys) = (sumEntries x xs ys)

sumEntries' : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries' {n} pos l1 l2 = case integerToFin pos n of
                            Nothing => Nothing
                            Just f => Just ((index f l1) + (index f l2))

main : IO ()
main = putStrLn (cast 'x')
