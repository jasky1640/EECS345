{-Problem 1-}
--Create the function rotate that takes three elements and a list. 
--It returns a list that is the same as the input list 
--except that each occurrence of the first element is replaced by the second
--the second element is replaced by the third, and the third is replaced by the first.
rotate _ _ _ [] = []
rotate a b c (h:t)
 | a == h    = b:x
 | b == h    = c:x
 | c == h    = a:x
 | otherwise = h:x
 where x = rotate a b c t
 
 {-Problem 2-}
 --squareroot takes two numbers, a value and an iteration. 
 --The iteration will be an integer greater than or equal to 0. 
 --The function will compute the squareroot of the value using iteration rounds of Newton's method, 
 -- starting with an initial value equal to the input value.
 --Newton's method is new = old - ((old * old) - value) / (2 * old)
squareroot a n 
 | n == 0    = a
 | otherwise = x - ((x * x) - a) / (2 * x) 
 where x = squareroot a (n - 1)

 {-Problem 3-}
 --Create a continuation passing version of squareroot_cps
squareroot_cps a n return
 | n == 0    = return a
 | otherwise = squareroot_cps a (n - 1) (\x -> return (x - ((x * x) - a) / (2 * x))) 
 
 {-Problem 4-}
 --Create a type that allows us to have nested lists. 
 --Your type should have two kinds of values, elements and sublists. 
 --For example, the following will be a valid list: 
 --[Element 1,Element 3,SubList [Element 4,SubList [SubList [Element 5],SubList []]],Element 6]
data NestedList t = Element t | SubList [NestedList t] deriving (Show)

 {-Problem 5-}
 --Create the function grotate that takes three values and list containing elements and sublists 
 --and returns a list with the same structure, 
 --but if any "element" is the first input, it is replaced by the second, 
 --if an "element" is the second input, it is replaced by the third, 
 --and if it is the "third" input, it is replaced by the first.
grotate a b c [] = []
grotate a b c [Element t]
 |t == a    = [Element b]
 |t == b    = [Element c]
 |t == c    = [Element a]
 |otherwise = [Element t]
grotate a b c [SubList t] = [SubList (grotate a b c t)]
grotate a b c (h:t) = (grotate a b c [h]) ++ (grotate a b c t)
 
 {-Problem 6-}
 --Using the Tree type created in class, write a function removeMin that takes a Tree as input. 
data BinTree t = Empty | Leaf t | InnerNode t (BinTree t) (BinTree t) deriving (Show)
instance (Eq t) => Eq (BinTree t) where 
  Empty           == Empty = True
  Leaf a          == Empty = False
  InnerNode a l r == Empty = False
 --Assuming the tree is in proper order 
 --(all values in the left child are smaller than the value in the node, 
 --and all the values in the right child are equal or larger than the node), 
 --the function will return a new tree with the smallest value of the tree removed. 
 --if the minimum value is an internal node, you replace the value of the node with the smallest value of the right child. 
 --The resulting tree should not have an internal node with both children empty.
removeMin Empty  = Empty
removeMin (Leaf a) = Empty
removeMin (InnerNode a l r)
 | l == Empty && r == Empty  = Empty
 | l == Empty                = (InnerNode (findMin r) l (removeMin r))
 | otherwise                 = (replaceEmptyNode (InnerNode a (removeMin l) r))
 --findMin is a helper method for removeMin method to find the smallest member in a binTree and return its value
findMin (Leaf a) = a
findMin (InnerNode a l r)
 | l == Empty = a
 | otherwise  = findMin l
 --replaceEmptyNode is a helper method for removeMin method to replace a InnerNode with two Empty child Node with
 --a LeafNode containing the same value in InnerNode
replaceEmptyNode Empty = Empty
replaceEmptyNode (Leaf a) = Leaf a
replaceEmptyNode (InnerNode a l r) 
 | l == Empty && r == Empty = (Leaf a)
 | otherwise                = (InnerNode a (replaceEmptyNode l) (replaceEmptyNode r))
 
 {-Problem 7-}
 --Create the function dotproduct that takes two vectors (lists of numbers) and returns the dot product of the vectors. 
 --Normally, this would crash if the vectors have different lengths. 
 --You will prevent this using the Maybe monad of Haskell. 
 --Namely, if the lists have the same size, dotproduct should return the correct answer embedded in a Maybe,
 --and if they do not, it should return Nothing.
dotproduct a b
 | (length a) == (length b) = Just (vertexproduct a b)
 | otherwise                = Nothing  
--Vertexproduct is a helper method for dotproduct to handle the caculation of correctly-formatted dot product
vertexproduct [] _ = 0
vertexproduct (h1:t1) (h2:t2) = (h1 * h2) + (vertexproduct t1 t2)
 
 {-Problem 8-}
 --Create the function vectormult that takes a row vector (a list of numbers) and matrix (a list of lists of numbers) 
 --and multiplies the vector times the matrix. 
 --The result should be a vector embedded in a Maybe 
 --where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix. 
 --If the length of the vector does not match the number of rows, or if the matrix is not rectangular, 
 --the result should be Nothing.
vectormult a b
 | (length a) /= (length b) = Nothing
 | isRectangular b == False = Nothing 
 | otherwise                = Just (vectormultcalculation a b)
--isRectangular is a helper function for vectormult and matrixmultiply to test if a matrix is rectangular
isRectangular l
 | (tail l) == []                                = True
 | (length (head l)) /= (length (head (tail l))) = False
 | otherwise                                     = isRectangular (tail l)
--vectormultcalculation is a helper method for vectormult and matrixmultiply that
--takes a row vector and a correct rectangular matrix and returns the result list
vectormultcalculation _ [] = [] 
vectormultcalculation a b = (vertexproduct a (getFirstElements b)) : (vectormultcalculation a (removeFirstElement b))
--removeFirstElement is a helper method for vectormultcalculation that removes the first elements of the input list
removeFirstElement [] = []
removeFirstElement l 
 | (tail (head l)) == [] = []
 | otherwise             = (tail (head l)) : (removeFirstElement (tail l))
--getFirstElements is a helper method for vectormultcalculation that takes an input list
--and return a list consisting of the first elements of the sublists of the input lists
getFirstElements [] = []
getFirstElements (h:t) = (head h) : (getFirstElements t)

 {-Problem 9-}
 --matrixmultiply takes two matrices (a list of lists of numbers) and multiplies them. 
 --The result should be embedded in a Maybe, and if any of the matrix dimensions do not fit 
 --or if a matrix is not rectangular, then the result should be Nothing.
matrixmultiply a b
 | isRectangular a == False        = Nothing
 | isRectangular b == False        = Nothing
 | (length (head a)) /= (length b) = Nothing
 | otherwise                       = Just (matrixmultiplycalculation a b)
 --matrixmultiplycalculation is a helper method for matrixmultiply to do the actual matrixmultiply calculation
 --the precondition of the method is that all the input lists are valid (tested by matrixmultiply method)
matrixmultiplycalculation a b 
 | a == []   = []
 | otherwise = (vectormultcalculation (head a) b) : (matrixmultiplycalculation (tail a) b)
 {-Problem 10-}
 --In Haskell, lists are monads. 
 --For example, [1,2,3] >>= (\v -> [2*v]) produces the list [2,3,4]. 
 --In this problem, you are to figure out how that works.
 --Create a list monad that generalizes a list. This will not be a Haskell Monad type, 
 --but instead one of our own creation like the Value type from lecture. 
data List t = Null | Pair t (List t) deriving (Show)
 --Then create a binding function lbind and a return function lreturn to make a list monad. 
lreturn x = Pair x

lbind :: (List t) -> (t -> List t -> List t) -> List t
lbind (Pair t l) f = f t (lbind l f)
lbind Null _ = Null