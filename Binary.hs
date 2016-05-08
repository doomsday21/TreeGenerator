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
calLeft i lst 
    | 2*i +1 > length lst-1 = -100 
    | otherwise =  lst!! (2*i +1)

calRight:: Int -> [Int]-> Int
calRight i lst 
    | 2*i +2 > length lst-1 = -100 
    | otherwise =  lst!! (2*i +2)

leftInsert::BinaryTree -> Int ->BinaryTree
leftInsert E n = N n E E 
leftInsert bst@(N x lt rt) n =N x ( leftInsert lt n) rt

rightInsert::BinaryTree -> Int ->BinaryTree
rightInsert E n = N n E E 
rightInsert bst@(N x lt rt) n =N x lt ( rightInsert rt n)

lrInsert:: BinaryTree->Int -> Int -> Int-> BinaryTree
lrInsert b@(N n lt E) x y z= N n lt (N x (N y E E ) (N z E E))
lrInsert b@(N n lt rt) x y z= N n (lrInsert lt x y z) rt

rlInsert:: BinaryTree-> Int -> Int -> Int-> BinaryTree 
rlInsert b@(N n E rt) x y z = N n (N x (N y E E ) (N z E E)) rt
rlInsert b@(N n lt rt) x y z= N n lt (rlInsert rt x y z) 

inL :: BinaryTree -> Int->[Int] -> BinaryTree
inL t i lst 
    |calLeft i lst /= -100 = inL (leftInsert t (calLeft i lst)) (2*i+1) lst
    |otherwise = t 

inL' :: BinaryTree -> Int-> [Int] -> BinaryTree
inL' t i lst 
    |calLeft i lst /= -100 = inL' (rlInsert t (calLeft i lst) (calLeft (2*i +1) lst) (calRight (2*i +1) lst)) (2*i+2) lst
    |otherwise = t 

inR' :: BinaryTree -> Int-> [Int] -> BinaryTree
inR' t i lst 
    |calRight i lst /= -100 = inR' (lrInsert t (calRight i lst) (calLeft (2*i +2) lst) (calRight (2*i +2) lst))  (2*i+1) lst
    |otherwise = t 

inR :: BinaryTree -> Int -> [Int] -> BinaryTree
inR t i lst 
    |calRight i lst /= -100 = inR (rightInsert t (calRight i lst)) (2*i+2) lst
    |otherwise = t 

convertToBinary:: [Int]-> BinaryTree 
convertToBinary lst@(x:xs) = inL' (inR' ( inR (inL (N x E E) 0 lst) 0 lst ) 1 lst ) 2 lst



--convertToBinary:: BinaryTree -> [Int] -> BinaryTree
--convertToBinary tree [] = tree
--convertToBinary tree lst@(x:xs)
--    | length lst/= 0 = convertToBinary (iNsert (tree) (take 2 xs)) (drop 2 xs)  
--    |otherwise = tree 

--iNsert:: BinaryTree -> [Int] -> BinaryTree
--iNsert tree  [-100 , -100] = tree
--iNsert tree lst
--    | lst!! 0 == -100 = rightInsert tree (lst!!1)
--    | lst!!1 == -100 = leftInsert tree (lst!!0)
--    |otherwise  =  rightInsert ( leftInsert tree (lst!!0) ) (lst!!1)

--convertToBinary':: [Int] -> BinaryTree
--convertToBinary' [] = E
--convertToBinary' lst@(x:xs) = iNsert' (N x E E ) xs 


--iNsert':: BinaryTree -> [Int] -> BinaryTree
--iNsert' tree [] = tree
--iNsert' tree lst 
--    | length lst/= 0 && lst!! 0 == -100 = iNsert' (rightInsert tree (lst!!1) ) (drop 2 lst)
--    | length lst/= 0 && lst!!1 == -100 = iNsert' (leftInsert tree (lst!!0)) (drop 2 lst)
--    |otherwise  =  iNsert' (rightInsert ( leftInsert tree (lst!!0) ) (lst!!1)) (drop 2 lst)
----insert1 :: BinaryTree -> Int -> BinaryTree
----insert1 E  n = N n E E 
----insert1 t@(N x lt rt) n 
----    | lt == E = N x (insert1 lt n) rt
----    | rt == E = N x lt (insert1 rt n)
----    | lt/=E && rt == E = 


--bstFind:: BinaryTree ->Int->Bool 
--bstFind (E) _ = False;
--bstFind (N x lt rt) n
--    |x==n = True
--    |bstFind lt n /= False = bstFind lt n
--    |bstFind rt n /= False = bstFind rt n 