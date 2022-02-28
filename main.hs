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
          deriving Show

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
