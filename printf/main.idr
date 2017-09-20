module Main

import Data.List

data Token : Type where
     TokenString : Token
     TokenNumber : Token
     TerminalWord : String -> Token
     TerminalChar : Char -> Token

tokenize : (xs : List Char) -> List Token
tokenize [] = []
tokenize ('%' :: 'd' :: chars) = TokenNumber :: tokenize(chars)
tokenize ('%' :: 's' :: chars) = TokenString :: tokenize(chars)
tokenize (c :: chars) = (TerminalChar c) :: tokenize(chars)

stringToTokens : String -> List Token
stringToTokens s = tokenize . unpack $ s

Formatter : (List Token) -> Type
Formatter [] = String
Formatter (x :: xs) = case x of
                           TokenNumber => (i : Int) -> Formatter xs
                           TokenString => (s : String) -> Formatter xs
                           _ => Formatter xs

printFmt : (tokens : List Token) -> (acc : String) -> Formatter tokens
printFmt [] acc = acc
printFmt (x :: xs) acc = case x of
                                       TokenNumber => (\i => printFmt xs (acc ++ show i))
                                       TokenString => \s => printFmt xs (acc ++ s)
                                       (TerminalWord w) => printFmt xs (acc ++ w)
                                       (TerminalChar c) => printFmt xs (acc ++ (strCons c ""))

||| поддерживает следующие шаблоны:
|||
||| - %d - целое число
||| - %s - строка
||| - %v - структура поддерживающая интерфейс Show
|||
||| Пример использования:
|||
||| printf "Привет мир от %s" "Ивана"
|||
||| printf "Я получил %d монет от %s" 100 "Михаила"
|||
||| printf "Структура Nat: %v" (S (S (S N)))
|||
printf : (template : String) -> Formatter (stringToTokens template)
printf template = printFmt (stringToTokens template) ""

main : IO ()
main = do
     putStrLn (printf "Hello, %s !" "UserName")
     putStrLn (printf "I have %d%s" 100 "$")
     putStrLn (printf "%s %d %s %d" "a" 10 "b" 10)

     -- Ошибочные случаи, когда мы не пройдем Type Checker:

     -- Type mistmatch beetween String -> String and String
     -- putStrLn (printf "%s %s" "hello")

     -- Type mistmatch between String and Int (expected type)
     -- putStrLn (printf "%d %s" "arg" 1)
