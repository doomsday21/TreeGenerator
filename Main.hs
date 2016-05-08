import System.IO  
import System.Directory  
import Data.List  
import Data.Char
import Binary (BinaryTree(..))
import Binary
import BST (convertToBST)
import RBTree (C(..))
import RBTree (RB(..))
import RBTree
import Control.Monad

output [] = [] 
output lst@(x:xs) 
	| x == "00" = [ -100 ] ++ output(xs)
	|otherwise =  [ read x :: Int ] ++ output(xs)

main = do
	putStrLn "Welcome to TreeGenerator!"
	putStrLn "Please enter numbers separated by space :"
	input <- getLine
	putStrLn " You can generate a Binary Tree (1) , BST (2) and RB Tree (3)."
	putStrLn "Choose type : "
	ty <- getLine
	when (read ty == 1) $
		writeFile "Binary.tex" (startStuff++outputTex' (convertToBinary (output (words input)))++endStuff)
	when (read ty == 2) $
		writeFile "BST.tex" (startStuff++outputTex' (convertToBST (output (words input)))++endStuff)
	when (read ty == 3) $
		writeFile "RB.tex" (startStuff++outputRBTex (convertToRB (output (words input)))++endStuff)


startStuff :: String
startStuff = "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows}\n\n\\tikzset{\n  treenode/.style = {align=center, inner sep=0pt, text centered,\n    font=\\sffamily},\n  arn_n/.style = {treenode, circle, white, font=\\sffamily\\bfseries, draw=black,\n    fill=black, text width=1.5em},% arbre rouge noir, noeud noir\n  arn_r/.style = {treenode, circle, red, draw=red, \n    text width=1.5em, very thick},% arbre rouge noir, noeud rouge\n  arn_x/.style = {treenode, rectangle, draw=black,\n    minimum width=0.5em, minimum height=0.5em}% arbre rouge noir, nil\n}\n\n\\begin{document}\n\\begin{tikzpicture}[->,>=stealth',level/.style={sibling distance = 5cm/#1,\n  level distance = 1.5cm}]"
endStuff :: String
endStuff = ";\\end{tikzpicture}\n\\end{document}\n"


outputTex' :: BinaryTree -> String
outputTex' bt@(N val lt rt) = "\n \\node [arn_n] {"++(show val)++"}\n "++processChildren lt ++ processChildren rt

outputRBTex :: RB Int -> String
outputRBTex rb@(Node col val lt rt) = "\n \\"++(colorTex rb)++" {"++(show val)++"}\n "++processRBChildren lt ++ processRBChildren rt



processRBChildren :: RB Int -> String
processRBChildren Empty = "node [arn_n] {}\n"
processRBChildren rb@(Node col val lt rt)
	| (hasRBChildren rt) && (hasRBChildren lt) = "child { "++colorTex rb++" {"++(show val)++"} "++processRBChildren lt ++ " " ++ processRBChildren rt ++ "}\n"
	| hasRBChildren lt = "child { "++colorTex rb++" {"++(show val)++"}" ++ processRBChildren lt ++ writeRBTex rt ++ "}\n"
	| hasRBChildren rt = "child { "++colorTex rb++" {"++(show val)++"}" ++ writeRBTex lt ++ processRBChildren rt ++ "}\n"
	| (hasRBRight rb) && (hasRBLeft rb) = "child { "++colorTex rb++" {"++(show val)++"}" ++ writeRBTex lt ++ writeRBTex rt ++ "}\n"
	| (hasRBLeft rb) = "child { "++colorTex rb++" {"++(show val)++"}" ++ writeRBTex lt ++ " child { node [arn_x] {}} " ++ "}\n"
	| (hasRBRight rb)= "child { "++colorTex rb++" {"++(show val)++"}" ++ " child { node [arn_x] {}} " ++ writeRBTex rt ++ "}\n"
	| otherwise =  "child { "++colorTex rb++" {"++(show val)++"}}\n" 

colorTex :: RB Int -> String
colorTex Empty = "node [arn_x]"
colorTex rb@(Node col val lt rt)
	| col == R = "node [arn_r]"
	| col == B = "node [arn_n]"

processChildren :: BinaryTree -> String
processChildren E = "node [arn_x] {}\n"
processChildren bt@(N val lt rt)
	| (hasChildren rt) && (hasChildren lt) = "child { node [arn_n] {"++(show val)++"} "++processChildren lt ++ " " ++ processChildren rt ++ "}\n"
	| hasChildren lt = "child { node [arn_n] {"++(show val)++"}" ++ processChildren lt ++ writeTex rt ++ "}\n"
	| hasChildren rt = "child { node [arn_n] {"++(show val)++"}" ++ writeTex lt ++ processChildren rt ++ "}\n"
	| (hasRight' bt) && (hasLeft' bt) = "child { node [arn_n] {"++(show val)++"}" ++ writeTex lt ++ writeTex rt ++ "}\n"
	| (hasLeft' bt) = "child { node [arn_n] {"++(show val)++"}" ++ writeTex lt ++ " child { node [arn_x] {}} " ++ "}\n"
	| (hasRight' bt) = "child { node [arn_n] {"++(show val)++"}" ++ " child { node [arn_x] {}} " ++ writeTex rt ++ "}\n"
	| otherwise =  "child {node [arn_n] {"++(show val)++"}}\n" 


writeTex :: BinaryTree -> String
writeTex E = "child {node [arn_x] {}}"
writeTex bt@(N val lt rt) = processChildren	bt


writeRBTex :: RB Int -> String
writeRBTex Empty = "child {node [arn_x] {}}"
writeRBTex rb = processRBChildren rb

zz = N 5 (N 4 E E) (N 6 E E)

zz2 = N 5 (N 4 (N 10 E E) (N 7 E E)) (N 9 E (N (-1) E E))

zz3 = N 33 (N 15 (N 10 (N 3 E E) (N 1 E E)) (N 20 (E) (E))) (N 47 (N 38 (E) (N 4 E E)) (N 51 (N 5 E E) (E)))

zz4 = N 33 (N 15 (N 10 (N 3 (N 15 E E) (N 15 E E)) (N 1 (N 15 E E) (N 15 E E))) (N 20 ((N 15 E E)) (E))) (N 47 (N 38 (E) (N 4 E (N 15 E E))) (N 51 (N 5 (N 15 E E) (N 15 E E)) ((N 15 E (N 15 E E)))))

zz5 = N 1 (N 2 (N 4 E E) (N 5 (N (-100) E E) (N (-100) E E))) (N 3 (N 6 (N (-100) E E) (N (-100) E E)) (N 7 E E))

zz6 = N 1 (N 2 (N 4 (N 8 E E) (N 9 (N (-100) E E) (N (-100) E E))) (N 5 (N 10 E E) (N 11 E E))) (N 3 (N 6 (N 12 E E) (N 13 E E)) (N 7 (N 14 (N (-100) E E) (N (-100) E E)) (N 15 E E)))

zzr1 = Node B 6 (Node B 2 (Node B 1 Empty Empty) (Node B 4 (Node R 3 Empty Empty) (Node R 5 Empty Empty))) (Node B 19 (Node R 12 (Node B 10 (Node R 8 Empty Empty) Empty) (Node B 13 Empty Empty)) (Node R 24 (Node B 22 (Node R 21 Empty Empty) (Node R 23 Empty Empty)) (Node B 25 Empty Empty)))
zzr2 = Node B 6 (Node B 2 (Node B 1 Empty Empty) (Node B 4 (Node R 3 Empty Empty) (Node R 5 Empty Empty))) (Node B 19 (Node R 12 (Node B 10 (Node R 8 Empty Empty) Empty) (Node B 13 Empty Empty)) (Node R 24 (Node B 22 (Node R 21 Empty Empty) (Node R 23 Empty Empty)) (Node B 25 Empty Empty)))