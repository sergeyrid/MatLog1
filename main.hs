main :: IO ()
main = do
    putStrLn testNoArrows1
    putStrLn testNoArrows2
    putStrLn testNoArrows3
    putStrLn testNoArrows4
    putStrLn testNoArrows5
    putStrLn testNNF1
    putStrLn testNNF2
    putStrLn testNNF3
    putStrLn testNNF4
    putStrLn testNNF5
    putStrLn testDNF1
    putStrLn testDNF2
    putStrLn testDNF3
    putStrLn testDNF4
    putStrLn testCNF1
    putStrLn testCNF2
    putStrLn testCNF3
    putStrLn testCNF4

type Symb = String

infixl 3 :&&
infixl 2 :||
infixl 2 :->
infixl 2 :<->

data Expr = Var Symb
          | Expr :&& Expr
          | Expr :|| Expr
          | Expr :-> Expr
          | Expr :<-> Expr
          | Not Expr
          deriving (Show, Eq)

toNoArrows :: Expr -> Expr
toNoArrows x@(Var _) = x
toNoArrows (x :-> y) = Not (toNoArrows x) :|| toNoArrows y
toNoArrows (x :<-> y) = (Not nx :|| ny) :&& (nx :|| Not ny) where
    nx = toNoArrows x
    ny = toNoArrows y
toNoArrows (x :&& y) = toNoArrows x :&& toNoArrows y
toNoArrows (x :|| y) = toNoArrows x :|| toNoArrows y
toNoArrows (Not x) = Not $ toNoArrows x

toNNF :: Expr -> Expr
toNNF = helper . toNoArrows where
    helper (x :&& y) = helper x :&& helper y
    helper (x :|| y) = helper x :|| helper y
    helper (Not (Not x)) = helper x
    helper (Not (x :&& y)) = helper (Not x :|| Not y)
    helper (Not (x :|| y)) = helper (Not x :&& Not y)
    helper x = x

toDNF :: Expr -> Expr
toDNF = helper . toNNF where
    helper (x :|| y) = helper x :|| helper y
    helper x@((Var _) :&& (Var _))         = x
    helper x@(Not (Var _) :&& (Var _))     = x
    helper x@((Var _) :&& Not (Var _))     = x
    helper x@(Not (Var _) :&& Not (Var _)) = x
    helper ((x :&& y) :&& z) = helper $ x :&& (y :&& z)
    helper ((x :|| y) :&& z) = helper (x :&& z) :|| helper (y :&& z)
    helper (x :&& y@(_ :&& _)) | (_ :&& _) <- dy = x :&& y
                               | otherwise       = helper $ x :&& dy where
                                   dy = helper y
    helper (x :&& (y :|| z)) = helper (x :&& y) :|| helper (x :&& z)
    helper x = x

toCNF :: Expr -> Expr
toCNF = helper . toNNF where
    helper (x :&& y) = helper x :&& helper y
    helper x@((Var _) :|| (Var _))         = x
    helper x@(Not (Var _) :|| (Var _))     = x
    helper x@((Var _) :|| Not (Var _))     = x
    helper x@(Not (Var _) :|| Not (Var _)) = x
    helper ((x :|| y) :|| z) = helper $ x :|| (y :|| z)
    helper ((x :&& y) :|| z) = helper (x :|| z) :&& helper (y :|| z)
    helper (x :|| y@(_ :|| _)) | (_ :|| _) <- cy = x :|| y
                               | otherwise       = helper $ x :|| cy where
                                   cy = helper y
    helper (x :|| (y :&& z)) = helper (x :|| y) :&& helper (x :|| z)
    helper x = x

testNoArrows1 :: String
testNoArrows1 | x == Var "x" = "Passed testNoArrows1"
              | otherwise = "Failed testNoArrows1" where
                  x = toNoArrows $ Var "x"

testNoArrows2 :: String
testNoArrows2 | x == (Var "x" :|| Var "y") = "Passed testNoArrows2"
              | otherwise = "Failed testNoArrows2" where
                  x = toNoArrows $ Var "x" :|| Var "y"

testNoArrows3 :: String
testNoArrows3 | x == (Var "x" :&& Var "y") = "Passed testNoArrows3"
              | otherwise = "Failed testNoArrows3" where
                  x = toNoArrows $ Var "x" :&& Var "y"

testNoArrows4 :: String
testNoArrows4 | x == (Not (Var "x") :|| Var "y") = "Passed testNoArrows4"
              | otherwise = "Failed testNoArrows4" where
                  x = toNoArrows $ Var "x" :-> Var "y"

testNoArrows5 :: String
testNoArrows5 | x == ((Not (Var "x") :|| Var "y") :&& (Var "x" :|| Not (Var "y"))) = "Passed testNoArrows5"
              | otherwise = "Failed testNoArrows5" where
                  x = toNoArrows $ Var "x" :<-> Var "y"

testNNF1 :: String
testNNF1 | x == Var "x" = "Passed testNNF1"
         | otherwise = "Failed testNNF1" where
             x = toNNF $ Var "x"

testNNF2 :: String
testNNF2 | x == (Var "x" :|| Var "y") = "Passed testNNF2"
         | otherwise = "Failed testNNF2" where
             x = toNNF $ Var "x" :|| Var "y"

testNNF3 :: String
testNNF3 | x == (Var "x" :&& Var "y") = "Passed testNNF3"
         | otherwise = "Failed testNNF3" where
             x = toNNF $ Var "x" :&& Var "y"

testNNF4 :: String
testNNF4 | x == (Not (Var "x") :&& Not (Var "y")) = "Passed testNNF4"
         | otherwise = "Failed testNNF4" where
             x = toNNF $ Not $ Var "x" :|| Var "y"

testNNF5 :: String
testNNF5 | x == (Not (Var "x") :|| Var "y") = "Passed testNNF5"
         | otherwise = "Failed testNNF4" where
             x = toNNF $ Not $ Var "x" :&& Not (Var "y")

testDNF1 :: String
testDNF1 | x == Var "x" = "Passed testDNF1"
         | otherwise = "Failed testDNF1" where
             x = toDNF $ Var "x"

testDNF2 :: String
testDNF2 | x == (Var "x" :&& Var "y") = "Passed testDNF2"
         | otherwise = "Failed testDNF2" where
             x = toDNF $ Var "x" :&& Var "y"

testDNF3 :: String
testDNF3 | x == (Var "x" :|| Var "y") = "Passed testDNF3"
         | otherwise = "Failed testDNF3" where
             x = toDNF $ Var "x" :|| Var "y"

testDNF4 :: String
testDNF4 | x == (((Var "x" :&& Var "z") :|| (Var "x" :&& Var "t")) :|| ((Var "y" :&& Var "z") :|| (Var "y" :&& Var "t"))) = "Passed testDNF4"
         | otherwise = "Failed testDNF4" where
             x = toDNF $ (Var "x" :|| Var "y") :&& (Var "z" :|| Var "t")

testCNF1 :: String
testCNF1 | x == Var "x" = "Passed testCNF1"
         | otherwise = "Failed testCNF1" where
             x = toCNF $ Var "x"

testCNF2 :: String
testCNF2 | x == (Var "x" :|| Var "y") = "Passed testCNF2"
         | otherwise = "Failed testCNF2" where
             x = toCNF $ Var "x" :|| Var "y"

testCNF3 :: String
testCNF3 | x == (Var "x" :&& Var "y") = "Passed testCNF3"
         | otherwise = "Failed testCNF3" where
             x = toCNF $ Var "x" :&& Var "y"

testCNF4 :: String
testCNF4 | x == (((Var "x" :|| Var "z") :&& (Var "x" :|| Var "t")) :&& ((Var "y" :|| Var "z") :&& (Var "y" :|| Var "t"))) = "Passed testCNF4"
         | otherwise = "Failed testCNF4" where
             x = toCNF $ (Var "x" :&& Var "y") :|| (Var "z" :&& Var "t")
