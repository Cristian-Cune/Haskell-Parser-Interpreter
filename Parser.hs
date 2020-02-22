module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState

-- Definire Program

data ClassInfo = ClassInfo  { className :: String
                            , class_data :: ClassState
                            , parent :: String
                            } deriving Show

data Program = Program  { classes :: [ClassInfo]
                        , global :: ClassInfo
                        } deriving Show

data Instruction = Instruction { ins ::String } deriving Show
initEmptyProgram :: Program
initEmptyProgram = Program [] (ClassInfo "Global" initEmptyClass "Global")

getVars :: Program -> [[String]]
getVars prog = getValues (class_data (global prog)) Var

getClasses :: Program -> [String]
getClasses prog =  "Global":(map (className) (classes prog))

--obtine clasa cu numele name
getSearchedClass :: String -> Program -> ClassInfo
getSearchedClass name prog = if name /= "Global" then head (filter (\x -> (className x) == name ) (classes prog))
                                                 else (global prog)
getParentClass :: String -> Program -> String

getParentClass name prog = parent ( getSearchedClass name prog)
                      

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass name prog = if isClass prog name then getValues (class_data (getSearchedClass name prog)) Func
                                                  else []
--sparge strigul l dupa c
splitBy:: Char -> String -> [String]
splitBy c [] = []
splitBy c l = if (fst res /= "") then (fst res):[] ++ (splitBy c (drop 1 (snd res)))
                                 else (splitBy c (drop 1 (snd res)))
            where
                res = (span (/= c) l)
parse :: String -> [Instruction]
parse s = map (Instruction) (splitBy '\n' s)

--verifica daca name e numele unei clase
isClass :: Program -> String -> Bool
isClass prog name = elem name (getClasses prog)

--parseaza o functie
parseLine:: String -> [String]
parseLine s = splitBy ' ' $ map (\x -> if (x == ':') ||
                                              (x == ',') ||
                                              (x == '(') ||
                                              (x == ')') ||
                                              (x == '=') then ' ' else x) s
--verifica daca o functie este valida
validFunc :: [String] -> Program-> Bool
validFunc l prog = checkRet && checkParam && checkClass
            where 
                checkRet = isClass prog (head l)
                checkParam = if elem False (map (\x -> (isClass prog x)) (drop 3 l)) then False else True
                checkClass = if isClass prog (l!!1) then True else False

reorder:: [String] -> [String]
reorder l = (l !! 2):(l !! 0):(drop 3 l)

--insereaza o functie intr-o clasa
insertFunc::[String] -> ClassInfo -> ClassInfo
insertFunc l_func classIn = ClassInfo (className classIn) (insertIntoClass (class_data classIn) Func (reorder l_func)) (parent classIn)

--insereaza o functie in program
insertFuncInProg:: [String] -> Program -> Program
insertFuncInProg  l_func prog = if validFunc l_func prog then good_prog else prog
                                where
                                good_prog = if l_func !! 1 == "Global"  then Program (classes prog) (insertFunc l_func (global prog))
                                                                        else Program (map (\x -> if className x == l_func !! 1
                                                                                                        then insertFunc l_func x
                                                                                                        else x ) (classes prog)) (global prog)

                                                                                            
interpret :: Instruction -> Program -> Program
interpret instruct prog
                    | (head l == "class") = if elem (className new_class) (getClasses prog) then prog else  Program  (new_class : (classes prog)) (global prog)            
                    | (head l == "newvar") = if isClass prog (l!!2) then Program (classes prog) 
                                                                        (ClassInfo "Global" 
                                                                        (insertIntoClass (class_data (global prog)) Var [l!!1,l!!2])
                                                                        "Global")
                                                                    else prog
                    |otherwise = insertFuncInProg l prog
                        where
                            l = parseLine (ins instruct)
                            name = l !! 1
                            new_class = if (length l == 4) &&
                                           (head l == "class") then ClassInfo name initEmptyClass (if isClass prog (l!!3) then l!!3 else "Global")
                                                               else ClassInfo name initEmptyClass "Global"

getMaybe (Just a) = a

--intoarce functia corespunzatoare de pe lantul de mostenire
validCall :: String -> String -> Program -> [Expr] -> [String]

validCall class_name func prog param
                    | searched == [] = (if class_name == "Global" then [] else validCall (getParentClass class_name prog) func prog param)
                    | elem Nothing paramTypes = []
                    | found == [] = validCall (getParentClass class_name prog) func prog param
                    | otherwise =  head found
                        where
                            searched = (filter (\x -> head x == func) (getFuncsForClass class_name prog))--lista cu functii ce au numele func
                            found = filter (\x -> (drop 2 x) == map (getMaybe) paramTypes) searched
                            paramTypes = map (\x -> infer x prog) param


infer :: Expr -> Program -> Maybe String
infer (Va s) prog = if result == [] then Nothing else Just (last (head result))
                where result = (filter (\x -> head x == s) (getVars prog))

infer (FCall var func param) prog 
                        | searchedClass == Nothing = Nothing
                        | searchedFunc == [] = Nothing
                        | otherwise  = Just (searchedFunc !! 1)
                            where 
                                searchedClass =  infer (Va var) prog
                                searchedFunc = validCall (getMaybe searchedClass) func prog param




