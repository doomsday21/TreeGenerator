module Binary  
(BinaryTree(..), 
root, left, right, 
isLeft    , isRight,
hasRight  , hasLeft,
hasRight' , hasLeft', hasChildren,
calLeft   , calRight,
leftInsert, rightInsert,
lrInsert  ,  rlInsert,
inL, inR  , inL', inR',
convertToBinary
) where  
  
data BinaryTree = E | N Int (BinaryTree) (BinaryTree) deriving (Show,Eq)

root E = -11111
root rb@(N val lt rt) = val

left E = E
left rb@(N val lt rt) = lt


right E = E
right rb@(N val lt rt) = rt



hasLeft' E = False
hasLeft' bt@(N _ lt rt) = lt /= E && (root lt /= -100)

hasRight' E = False
hasRight' bt@(N _ lt rt) = rt /= E && (root rt /= -100)

hasChildren E = False
hasChildren bt@(N n lt rt) = (((hasRight' rt) || (hasLeft' rt)) || ((hasRight' lt) || (hasLeft' lt)))


isLeft bt@(N _ lt rt) n
	| (root lt) == n = True
	| otherwise = False


isRight bt@(N _ lt rt) n
	| (root rt) == n = True
	| otherwise = False


hasLeft bt@(N _ lt rt) n = lt /= E

hasRight bt@(N _ lt rt) n = rt /= E

calLeft:: Int -> [Int]-> Int
--takes input the root node of the tree or the subtree
--calculates the index of its left child in the list 
--outputs the left child value
calLeft i lst 
    | 2*i +1 > length lst-1 = -100 
    | otherwise =  lst!! (2*i +1)

calRight:: Int -> [Int]-> Int
--takes input the root node of the tree or the subtree
--calculates the index of its right child in the list 
--outputs the right child value
calRight i lst 
    | 2*i +2 > length lst-1 = -100 
    | otherwise =  lst!! (2*i +2)

leftInsert::BinaryTree -> Int ->BinaryTree
--inserts the child at leftmost spot 
leftInsert E n = N n E E 
leftInsert bst@(N x lt rt) n =N x ( leftInsert lt n) rt

rightInsert::BinaryTree -> Int ->BinaryTree
--inserts the child at rightmost spot
rightInsert E n = N n E E 
rightInsert bst@(N x lt rt) n =N x lt ( rightInsert rt n)

lrInsert:: BinaryTree->Int -> Int -> Int-> BinaryTree
--inserts the child at left right position
lrInsert b@(N n lt E) x y z= N n lt (N x (N y E E ) (N z E E))
lrInsert b@(N n lt rt) x y z= N n (lrInsert lt x y z) rt

rlInsert:: BinaryTree-> Int -> Int -> Int-> BinaryTree 
--inserts the child at right left position
rlInsert b@(N n E rt) x y z = N n (N x (N y E E ) (N z E E)) rt
rlInsert b@(N n lt rt) x y z= N n lt (rlInsert rt x y z) 

inL :: BinaryTree -> Int->[Int] -> BinaryTree
--adds all left nodes in left subtree
inL t i lst 
    |calLeft i lst /= -100 = inL (leftInsert t (calLeft i lst)) (2*i+1) lst
    |otherwise = t 

inL' :: BinaryTree -> Int-> [Int] -> BinaryTree
--adds the left right nodes in the left subtree
inL' t i lst 
    |calLeft i lst /= -100 = inL' (rlInsert t (calLeft i lst) (calLeft (2*i +1) lst) (calRight (2*i +1) lst)) (2*i+2) lst
    |otherwise = t 

inR' :: BinaryTree -> Int-> [Int] -> BinaryTree
--adds the right left nodes in the right subtree
inR' t i lst 
    |calRight i lst /= -100 = inR' (lrInsert t (calRight i lst) (calLeft (2*i +2) lst) (calRight (2*i +2) lst))  (2*i+1) lst
    |otherwise = t 

inR :: BinaryTree -> Int -> [Int] -> BinaryTree
--adds all the right nodes in the right subtree
inR t i lst 
    |calRight i lst /= -100 = inR (rightInsert t (calRight i lst)) (2*i+2) lst
    |otherwise = t 

convertToBinary:: [Int]-> BinaryTree 
--converts an array of Ints into a binary tree
convertToBinary lst@(x:xs) = inL' (inR' ( inR (inL (N x E E) 0 lst) 0 lst ) 1 lst ) 2 lst
