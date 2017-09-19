module Main

import Data.List

data Token : Type where
     TokenString : Token
     TokenNumber : Token
     TerminalWord : String -> Token
     TerminalChar : Char -> Token

splitOnWords : String -> List String
splitOnWords s = split (\ch => if ch == ' ' then True else False) s

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

-- printf template args = printFmt template (tokenize (splitOnWords template)) "" args

main : IO ()
main = do
     putStrLn "Hello World"
