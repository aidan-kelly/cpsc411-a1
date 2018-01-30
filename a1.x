--Aidan Kelly
--10173966
--CPSC 411 A1.
--A simple lexer for the Minisculus language.

{
module Lexer where

import Data.List
import Data.Char
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

--If we find any of these strings, we turn them into tokens!
tokens :-
  $white+				        	        ; 
  "%" .* 					                ;
  "/*"						                {\p s -> LCOMMENT p}
  "*/"						                {\p s -> RCOMMENT p}
  "if"						                {\p s -> IF p}
  "then"                          {\p s -> THEN p}
  "while"                         {\p s -> WHILE p}
  "do"                            {\p s -> DO p}
  "input"                         {\p s -> INPUT p}
  "else"                          {\p s -> ELSE p}
  "begin"                         {\p s -> BEGIN p}
  "end"                           {\p s -> END p}
  "write"                         {\p s -> WRITE p}
  $digit+				                  {\p s -> NUM p s}
  $alpha [$alpha $digit \_ \' \.]*		{\p s -> ID p s}
  "+"                             {\p s -> ADD p}
  ":="                            {\p s -> ASSIGN p}
  "-"                             {\p s -> SUB p}
  "*"                             {\p s -> MUL p}
  "/"                             {\p s -> DIV p}
  "("                             {\p s -> LPAR p}
  ")"                             {\p s -> RPAR p}
  ";"                             {\p s -> SEMICOLON p}  
  .							                  {\p s -> ERROR p s}
{

--function that allows us to find position of errors.
getErrorPosn :: Token -> AlexPosn
getErrorPosn (ERROR a b) = a
getErrorPosn (RCOMMENT p) = p

--function to deal with nested comments/multiline comments
comment :: [Token] -> Int -> [Token]

--base case for the empty list
comment [] counter = if (counter /= 0) then error("\nMismatched comments") else []
comment (head:tail) counter

    --If we find an error token, we stop and print out where we found it
    | (show head == "ERROR") = error("\nError found at: " ++ show (getErrorPosn head))

    --If we find a LCOMMENT we increment the counter
    | (show head == "LCOMMENT") && (counter == 0) = comment tail (counter+1)
    | (show head == "LCOMMENT") && (counter /= 0) = comment tail (counter+1)

    --If we find a RCOMMENT and the counter == 0, we know comments are mismatched
    | (show head == "RCOMMENT") && (counter == 0) = error ("\nMismatched comments: " ++ show (getErrorPosn head))

    --If we find a RCOMMENT and the counter /= 0, we decrement the counter
    | (show head == "RCOMMENT") && (counter /= 0) = comment tail (counter -1)

    --If we find any other token and the counter == 0 we check next token
    | (show head /= "LCOMMENT") && (show head /= "RCOMMENT") && (counter == 0) = [head] ++ (comment tail counter)

    --If we find any other token and the counter /= 0 we remove as its part of a comment.
    | (show head /= "LCOMMENT") && (show head /= "RCOMMENT") && (counter /= 0) = (comment tail counter)
    | otherwise = error("Something went wrong.")

--Sets up all of the needed Tokens    
data Token
  = IF AlexPosn
  | THEN AlexPosn
  | WHILE AlexPosn
  | DO AlexPosn
  | INPUT AlexPosn
  | ELSE AlexPosn
  | BEGIN AlexPosn
  | END AlexPosn
  | WRITE AlexPosn
  | ID AlexPosn String
  | NUM AlexPosn String
  | ADD AlexPosn
  | ASSIGN AlexPosn
  | SUB AlexPosn
  | MUL AlexPosn
  | DIV AlexPosn
  | LPAR AlexPosn
  | RPAR AlexPosn
  | SEMICOLON AlexPosn
  | LCOMMENT AlexPosn
  | RCOMMENT AlexPosn
  | ERROR AlexPosn String
  deriving (Eq)

--Edited the show of each token to leave out the position.
instance Show Token where
  show (IF a) = "IF"
  show (THEN a) = "THEN"
  show (WHILE a) = "WHILE"
  show (DO a) = "DO"
  show (INPUT a) = "INPUT"
  show (ELSE a) = "ELSE"
  show (BEGIN a) = "BEGIN"
  show (END a) = "END"
  show (WRITE a) = "WRITE"
  show (ID a b) = "ID " ++ show b
  show (NUM a b) = "NUM " ++ show b
  show (ADD a) = "ADD"
  show (ASSIGN a) = "ASSIGN"
  show (SUB a) = "SUB"
  show (MUL a) = "MUL"
  show (DIV a) = "DIV"
  show (LPAR a) = "LPAR"
  show (RPAR a) = "RPAR"
  show (SEMICOLON a) = "SEMICOLON"
  show (LCOMMENT a) = "LCOMMENT"
  show (RCOMMENT a) = "RCOMMENT"
  show (ERROR a b) = "ERROR"

--main function that reads our input and breaks it into tokens
lexer :: String -> IO [Token]
lexer file = do
  s <- readFile file
  return (comment (alexScanTokens s) 0)
}