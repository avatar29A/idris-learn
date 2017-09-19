module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

data Command = Add String
               | Get Integer
               | Size
               | Quit

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData _ items) item = MkData _ (add item items)
           where
            add : String -> Vect old String -> Vect (S old) String
            add item [] = [item]
            add item (x :: xs) = x :: add item xs

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" _ = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing


parse : (input : String) -> Maybe Command
parse input = case Strings.span (/= ' ') input of
              (cmd, args) => parseCommand cmd (Strings.ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Nothing
                              Just f => Just (index f store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n ", addToStore store item)
                              Just (Get pos) => getEntry pos store
                              Just Size => let (MkData size _) = store in Just (("Size = " ++ show size ++ "\n"), store)
                              Just Quit => Nothing

main : IO ()
main = replWith (MkData _ ["Moscow", "Kurgan", "Ekaterinburg"]) "Command: " processInput


Enter the sequantally strings and press Enter when input will be compleated.
