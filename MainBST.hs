import BinarySearchTreeADT

-- Hàm main để kiểm tra các chức năng của module
main :: IO ()
main = do
    let tree = Nil 
    let tree1 = insertMany tree [5, 3, 7, 2, 4, 6, 8]
    print tree1
    -- Node (Node (Node Nil 2 Nil) 3 (Node Nil 4 Nil)) 5 (Node (Node Nil 6 Nil) 7 (Node Nil 8 Nil))
    print (inorder tree1) -- [2,3,4,5,6,7,8]
    print (preorder tree1) -- [5,3,2,4,7,6,8]
    print (postorder tree1) -- [2,4,3,6,8,7,5]

    -- Test findtree
    print (findtree tree1 4) -- True
    print (findtree tree1 9) -- False

    -- Test deleteMany
    let tree2 = deleteMany tree1 [2, 4, 6]
    print(inorder tree2) -- [3,5,7,8]

    -- Test deletemin
    let (minValue, newTree) = deletemin tree2
    print minValue -- 3
    print (inorder newTree) -- [5,7,8]