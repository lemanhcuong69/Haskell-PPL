module BinarySearchTreeADT (
    STree(..),
    findtree,
    inserttree,
    insertMany,
    deletemin,
    deletetree,
    deletemax,
    deletetree2,
    deleteMany,
    preorder,
    inorder,
    postorder
) where
            

data STree a = Nil | Node (STree a) a (STree a) 
    deriving (Ord, Eq, Show)


findtree :: (Ord a) => STree a -> a -> Bool
findtree Nil x = False
findtree (Node tleft y tright) x 
    | x == y = True
    | x < y = findtree tleft x
    | otherwise = findtree tright x


-- Hàm insert: Chèn giá trị x vào cây
inserttree :: (Ord a) => STree a -> a -> STree a
inserttree Nil x = Node Nil x Nil
inserttree (Node tleft y tright) x 
    | x == y = Node tleft x tright -- Không làm gì
    | x < y = Node (inserttree tleft x) y tright
    | otherwise = Node tleft y (inserttree tright x)


-- Chèn nhiều phần tử từ danh sách vào cây
insertMany :: (Ord a) => STree a -> [a] -> STree a
insertMany tree [] = tree
insertMany tree (x:xs) = insertMany (inserttree tree x) xs

--- Xóa giá trị nhỏ nhất trong cây
deletemin :: STree a -> (a, STree a)
deletemin (Node Nil y t2) = (y, t2)
deletemin (Node tleft y tright) = (z, Node tz y tright)
  where (z, tz) = deletemin tleft


-- Xóa giá trị x trong cây
deletetree :: (Ord a) => STree a -> a -> STree a
deletetree Nil x = Nil
deletetree (Node tleft y tright) x 
    | x < y = Node (deletetree tleft x) y tright
    | x > y = Node tleft y (deletetree tright x)
deletetree (Node tlfeft y Nil) x = tlfeft
deletetree (Node tleft y tright) x = Node tleft z tz
    where (z, tz) = deletemin tright


-- Xóa giá trị lớn nhất trong cây
deletemax :: (Ord a) => STree a -> (a, STree a)
deletemax (Node tleft y Nil) = (y, tleft)
deletemax (Node tleft y tright) = (z, Node tleft y tz)
  where (z, tz) = deletemax tright

-- Xóa giá trị x khỏi cây, dùng giá trị lớn nhất từ cây con trái
deletetree2 :: (Ord a) => STree a -> a -> STree a
deletetree2 Nil x = Nil
deletetree2 (Node tleft y tright) x 
    | x < y = Node (deletetree2 tleft x) y tright
    | x > y = Node tleft y (deletetree2 tright x)
deletetree2 (Node Nil y tright) x = tright -- Cây con trái rỗng, trả về cây con phải
deletetree2 (Node tleft y tright) x = Node tz z tright
    where (z, tz) = deletemax tleft -- Lấy giá trị lớn nhất từ cây con trái

-- Xóa nhiều phần tử từ danh sách khỏi cây
deleteMany :: (Ord a) => STree a -> [a] -> STree a
deleteMany tree [] = tree
deleteMany tree (x:xs) = deleteMany (deletetree tree x) xs


-- Perorder: Gốc -> trái -> Phải
preorder :: STree a -> [a]
preorder Nil = []
preorder (Node tleft x tright) = [x] ++ preorder tleft ++ preorder tright

-- Indorder: Trái -> gốc -> phải
inorder :: STree a -> [a]
inorder Nil = []
inorder (Node tleft x tright) = (inorder tleft) ++ [x] ++ (inorder tright)


-- Postorder: Trái -> Phải -> Gốc
postorder :: STree a -> [a]
postorder Nil = []
postorder (Node tleft x tright) = (postorder tleft) ++ (postorder tright) ++ [x]


-- main = do
--     --- Test hàm insertMany
--     let tree = insertMany Nil [5, 3, 8, 1, 4, 7, 9]
--     print tree
--     -- Node (Node (Node Nil 1 Nil) 3 (Node Nil 4 Nil)) 5 (Node (Node Nil 7 Nil) 8 (Node Nil 9 Nil))

--     let tree' = insertMany tree [2, 6]
--     print tree'
--     -- Node (Node (Node Nil 1 (Node Nil 2 Nil)) 3 (Node Nil 4 Nil)) 5 (Node (Node (Node Nil 6 Nil) 7 Nil) 8 (Node Nil 9 Nil))

--     let tree1 = deleteMany tree' [1, 4, 7, 6]
--     print tree1
--     -- Node (Node (Node Nil 2 Nil) 3 Nil) 5 (Node Nil 8 (Node Nil 9 Nil))
--     print(inorder tree1) -- [2,3,5,8,9]
--     print(preorder tree1) -- [5,3,2,8,9]
--     print(postorder tree1) -- [2,3,9,8,5]






