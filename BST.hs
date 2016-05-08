module BST  
(leaf, 
bstFind, 
bstInsert, 
bstRemove, 
findSuccessor, 
leftInsert, 
rightInsert,  
convertToBST, 
convert
) where  
  
import Binary (BinaryTree(..))

type BST = BinaryTree

leaf::Int -> BST
leaf n = N n E E
 
--partb
bstFind:: BST ->Int->Bool 
bstFind (E) _ = False;
bstFind (N x lt rt) n
    |x==n = True
    |n<x = bstFind lt n
    |n>x = bstFind rt n 
 
 
--partc
bstInsert:: BST -> Int ->BST
bstInsert (E) n = N n E E
bstInsert bst@(N x lt rt) n
    |bstFind bst n = bst
    | n<x  = N x (bstInsert lt n) rt
    | n>x  = N x lt (bstInsert rt n)
 
leftInsert:: BST -> Int ->BST
leftInsert (N x E _) n = N x (N n E E) E
leftInsert (E) n = N n E E
leftInsert bst@(N x lt rt) n
    |bstFind bst n = bst
    |otherwise = N x ( leftInsert lt n ) rt

rightInsert:: BST -> Int ->BST
rightInsert (E) n = N n E E
rightInsert (N x _ E) n = N x E (N n E E)
rightInsert bst@(N x lt rt) n
    |bstFind bst n = bst
    |otherwise  = N x lt ( rightInsert rt n )


--partd
bstRemove:: BST -> Int ->BST
bstRemove bst@(E) n=E
bstRemove bst@(N x lt rt) n
    | x==n && lt==E && rt==E = E
    | x==n && lt==E =rt
    | x==n && rt==E =lt
    | x==n = N successor lt  (bstRemove rt successor)
    | x<n = N x lt (bstRemove rt n)
    | x>n = N x (bstRemove lt n) rt
    | otherwise = bst
    where leftmost (N x E _) = x
          leftmost (N x lt _) = leftmost lt
          successor = leftmost rt

findSuccessor:: BST -> Int -> Int
findSuccessor bst@(N x E E) n = x
findSuccessor bst@(N x lt rt) n
    | x==n = findSuccessor  rt n
    | otherwise = findSuccessor lt n 

--CONVERSION 
convertToBST :: [Int] -> BST
convertToBST lst = convert E lst

convert:: BST -> [Int] -> BST
convert tree [] = tree
convert tree lst@(x:xs) 
    |length lst /= 0 = convert ( bstInsert tree x ) xs --assuming that a tree is in correct order
    |otherwise = tree 