module RBTree(C(..),RB(..),
convertToRB, 
hasRBLeft,
hasRBRight,
hasRBChildren, 
convert'
  )where 



data C = R | B deriving (Show, Eq, Ord)
data RB a = Empty | Node C a (RB a) (RB a) deriving (Show, Eq, Ord)


hasRBLeft Empty = False
hasRBLeft bt@(Node _ _ lt rt) = lt /= Empty && (root lt /= -100)

hasRBRight Empty = False
hasRBRight bt@(Node _ _ lt rt) = rt /= Empty && (root rt /= -100)

hasRBChildren Empty = False
hasRBChildren bt@(Node n c lt rt) = (((hasRBRight rt) || (hasRBLeft rt)) || ((hasRBRight lt) || (hasRBLeft lt)))


{- Trees for debugging purposes. 
zz = Node B 2 (Node B 1  Empty E) (Node B 6 (Node R 4  Empty E) (Node R 24 (Node R 12 Empty E) E))
zz2 = fixit zz 15
zz3 = insert zz2 15

The question in the pdf.
question2 = (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' (insert' Empty 1) 24) 2) 6) 4) 12) 25) 19) 10) 5) 3) 13) 8) 21) 23) 22)
-}

--This helper function returns the color of a given RB tree.
color Empty = B
color rb@(Node col val lt rt) = col

--This helper function returns the value of a given RB tree.
root Empty = -1
root rb@(Node col val lt rt) = val

--This helper function returns the left sub-tree of a given RB tree.
left Empty = Empty
left rb@(Node col val lt rt) = lt

--This helper function returns the right sub-tree of a given RB tree.
right Empty = Empty
right rb@(Node col val lt rt) = rt

-- Our main insert function. which first calls insert to insert n into the tree while holding the BST property. 
-- Then secondly calls fixit on the inserted node to maintain the RB-Tree property.
insert' rb n = fixit (insert rb n) n


-- This function adds n to our rb tree while maintaining just the BST property. called in insert'
insert Empty n = Node R n Empty Empty
insert rb@(Node col val lt rt) n
	| val==n = rb
    | val>n  = Node col val (insert lt n) rt
    | val<n  = Node col val lt (insert rt n)


-- This function rotates our RB tree to the left on node n.
rotateL :: RB Int-> Int -> RB Int  
rotateL Empty _ = Empty
rotateL rb@(Node col val lt rt) n 
	| val == n = Node (color rt) (root rt) (Node col val lt (left rt) ) (right rt)
	| val < n = Node col val lt (rotateL rt n)
	| val > n = Node col val (rotateL lt n) rt

-- This function rotates our RB tree to the right on node n.
rotateR:: RB Int-> Int -> RB Int  
rotateR Empty _ = Empty
rotateR rb@(Node col val lt rt) n
 | val == n = Node (color lt) (root lt) (left lt) (Node col val (right lt) rt)
 | val < n = Node col val lt (rotateR rt n)
 | val > n = Node col val (rotateR lt n) rt

-- This function takes RB and a node n as argument whose parent we need to find
-- and returns the whole tree for the parent. Makes it easier to find Uncle/Grandparent.

getParent:: RB Int -> Int -> RB Int
getParent Empty _ = Empty
getParent rb@(Node col val lt rt) n
 | val == n = Empty
 | root(lt) == n = Node col val lt rt
 | root(rt) == n = Node col val lt rt
 | n<val = getParent lt n
 | n>val = getParent rt n

-- This function returns the uncle's subtree. 

-- ASSUMING 
---value exists in the table.
---grandparent exists.
---It is only called if the above conditions are met.
---Where rb is the grandparent's tree and p is the parent's value.

getUncle :: RB Int -> RB Int -> RB Int
getUncle Empty _ = Empty
getUncle rb@(Node col val lt rt) p
 | val == (root p) = Empty
 | root(lt) == (root p) = rt
 | root(rt) == (root p) = lt
 | otherwise = Node B (-100) Empty Empty


-- This is the most useful function!
-- It takes a RB tree and a node n along with a color c as input. and then returns the new tree with node n's color set to c.
-- Used to color different nodes.
colorNode :: RB Int -> Int -> C -> RB Int
colorNode Empty _ _ = Empty
colorNode rb@(Node col val lt rt) n c
  | val ==n = Node c val lt rt
  | val < n = Node col val lt (colorNode rt n c)
  | val > n = Node col val (colorNode lt n c) rt 
  |otherwise = rb

-- We are giving it the parent as the rb tree and an Int n where n is the child's value. and find whether the child is on the left or the right.
isLeft :: RB Int -> Int -> Bool
isLeft rb@(Node col val lt rt ) n 
 |n<val = True
 |n>=val = False 


-- This function does the trivial coloring of coloring the parent blue and parent's parent red. 
-- And then rotate right/left depending on our condition whether's it's the right sub-tree or the left-subtree
colorit :: RB Int -> Int -> Int -> RB Int
colorit rb n i
	| i == 1 = rotateR (colorNode (colorNode rb p B) (root (getParent rb p)) R) (root (getParent rb p))
	| otherwise = rotateL (colorNode (colorNode rb p B)  (root (getParent rb p)) R) (root (getParent rb p))
	where p = root (getParent rb n)

-- This is our fixit code. It takes a RB tree and a node n on which fixing needs to be done and then apply the algorithm which was discussed in class on it.
fixit :: RB Int -> Int -> RB Int
fixit rb@(Node col val lt rt) n 
 | val == n =  Node B n lt rt
 | color(getParent rb n) == B = rb
 | color(u) == R = fixit (colorNode (colorNode (colorNode rb (root p) B) (root u) B) (root g) R) (root g) -- if uncle is red.
 | color(u) == B && isLeft g (root p) &&  not( isLeft p n) = colorit (rotateL rb (root p)) (root p) 1 --LR
 | color(u) == B && not (isLeft g (root p)) &&  ( isLeft p n) = colorit (rotateR rb (root p)) (root p) 0 --RL 
 | color(u) == B && (isLeft g (root p)) &&  (isLeft p n) = colorit rb n 1 --LL
 | color(u) == B && not (isLeft g (root p)) && not ( isLeft p n) = colorit rb n 0 --RR
 where p=getParent rb n 
       g=getParent rb (root p) 
       u=getUncle g p


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--convention : list of tuples with value followed by color 0(B) or 1 (R)
-- (Int , Char)

--insertL::RB Int-> (Int, Int) -> RB Int
--insertL Empty t@(n,col) 
--    | col== 1 =  Node R n Empty Empty 
--    | col== 0 = Node B n Empty Empty
--insertL bst@(Node c x lt rt) t =  Node c x ( insertL lt t ) rt

--insertR::RB Int-> (Int, Int) ->RB Int
--insertR Empty t@(n,col) 
--    | col== 1 =  Node R n Empty Empty 
--    | col== 0 = Node B n Empty Empty 
--insertR bst@(Node c x lt rt) t =  Node c x lt ( insertL rt t ) 


--convertToRB:: RB Int-> [(Int, Int)] -> RB Int
--convertToRB rb [] = rb
--convertToRB rb lst@(x:xs) 
--    | length lst/= 0 && (snd x) == 1= convertToRB (iNSert (Node R (fst x)  (Empty) (Empty)) (take 2 xs)) (drop 2 xs)  
--    | length lst/= 0 && (snd x) == 0= convertToRB (iNSert (Node B (fst x)  (Empty) (Empty)) (take 2 xs)) (drop 2 xs)  
--    | otherwise = rb 


--iNSert:: RB Int-> [(Int,Int)] -> RB Int
--iNSert tree [( -100 , _) , ( -100 , _ )] = tree
--iNSert tree lst
--    | fst (lst!!0) ==  -100  = insertR tree (lst!!1)
--    | fst (lst!!1) ==  -100  = insertL tree (lst!!0)
--    | otherwise  =  insertR ( insertL tree (lst!!0) ) (lst!!1)

convertToRB :: [Int] -> RB Int
convertToRB lst = convert' Empty lst

convert':: RB Int -> [Int] -> RB Int
convert' tree [] = tree
convert' tree lst@(x:xs) 
    |length lst /= 0 = convert' ( fixit (insert tree x) x ) xs --assuming that a tree is in correct order
    |otherwise = tree 