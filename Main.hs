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

output:: [String] -> [Int]
--Input : list of string of number ["1" , "2"]
--converts each element to int  
--returns: list of integers [1 ,2]
output [] = [] 
output lst@(x:xs) 
	| x == "00" = [ -100 ] ++ output(xs)
	|otherwise =  [ read x :: Int ] ++ output(xs)


--Main takes users input and the type of tree they want to generate
--It generates the latex code for it
main = do
	putStrLn "Welcome to TreeGenerator!"
	putStrLn " You can generate a Binary Tree (1) , BST (2) and RB Tree (3)."
	putStrLn "Choose type : "
	ty <- getLine
	putStrLn "Please enter numbers separated by space :"
	input <- getLine
	when (read ty == 1) $
		writeFile "Binary.tex" (startStuff++outputTex' (convertToBinary (output (words input)))++endStuff)
	when (read ty == 2) $
		writeFile "BST.tex" (startStuff++outputTex' (convertToBST (output (words input)))++endStuff)
	when (read ty == 3) $
		writeFile "RB.tex" (startStuff++outputRBTex (convertToRB (output (words input)))++endStuff)



startStuff :: String
--writes to the file the basic tex code which incluse
--type of document being created , packages included and basic initializations
startStuff = "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows}\n\n\\tikzset{\n  treenode/.style = {align=center, inner sep=0pt, text centered,\n    font=\\sffamily},\n  arn_n/.style = {treenode, circle, white, font=\\sffamily\\bfseries, draw=black,\n    fill=black, text width=1.5em},% arbre rouge noir, noeud noir\n  arn_r/.style = {treenode, circle, red, draw=red, \n    text width=1.5em, very thick},% arbre rouge noir, noeud rouge\n  arn_x/.style = {treenode, rectangle, draw=black,\n    minimum width=0.5em, minimum height=0.5em}% arbre rouge noir, nil\n}\n\n\\begin{document}\n\\begin{tikzpicture}[->,>=stealth',level/.style={sibling distance = 5cm/#1,\n  level distance = 1.5cm}]"


endStuff :: String
--this fucntion closes tags and marks the completion of latex file
endStuff = ";\\end{tikzpicture}\n\\end{document}\n"


outputTex' :: BinaryTree -> String
--generates the latex code for a Binary tree
outputTex' bt@(N val lt rt) = "\n \\node [arn_n] {"++(show val)++"}\n "++processChildren lt ++ processChildren rt

outputRBTex :: RB Int -> String
--generates the latex code for a RB tree
outputRBTex rb@(Node col val lt rt) = "\n \\"++(colorTex rb)++" {"++(show val)++"}\n "++processRBChildren lt ++ processRBChildren rt

processRBChildren :: RB Int -> String
--helper function
--generates code for the subtrees in the given RB tree
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
--helper function
--generate code depending of the color of the node
colorTex Empty = "node [arn_x]"
colorTex rb@(Node col val lt rt)
	| col == R = "node [arn_r]"
	| col == B = "node [arn_n]"

processChildren :: BinaryTree -> String
--helper function
--generates code for the subtrees in the Binary tree
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
--helper function
writeTex E = "child {node [arn_x] {}}"
writeTex bt@(N val lt rt) = processChildren	bt


writeRBTex :: RB Int -> String
--helper function
writeRBTex Empty = "child {node [arn_x] {}}"
writeRBTex rb = processRBChildren rb
