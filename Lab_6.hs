newtype I a =
  I a
  deriving (Show)

instance Functor I where
  fmap f (I arg) = I $ f arg

data S a =
  S a (S a)
  deriving (Show)

instance Functor S where
  fmap f (S arg ss) = S (f arg) $ fmap f ss

data T a b
  = T a (T b a)
  | N
  deriving (Show)

instance Functor (T a) where
  fmap _ N = N
  fmap f (T x N) = T x N
  fmap f (T x xs) = T x $ ffmap f xs
    where
      ffmap _ N        = N
      ffmap f (T x N)  = T (f x) N
      ffmap f (T x xs) = T (f x) $ fmap f xs

newtype C a b =
  C a
  deriving (Show)

instance Functor (C a) where
  fmap _ (C arg) = C arg

data P a =
  P
  deriving (Show)

instance Functor P where
  fmap _ P = P

data U =
  U
  deriving (Show)

newtype F a b =
  F (a -> b)

instance Functor (F a) where
  fmap f (F fn) = F $ f . fn

newtype E a =
  E (a -> a)

instance Functor E where
  fmap f (E fn) = E id

data Iso a b =
  Iso (a -> b) (b -> a)

-- Not possible, because the only allowed function
-- that accepts b as an argument is `id`, which returns `b`.
-- In other words, there is no way to convert `b` to `a`.
--
-- instance Functor (Iso a) where
--  fmap a1ToB (Iso aToA1 a1ToA) = Iso (a1ToB . aToA1) ???

main :: IO ()
main = print $ T 1 $ T 2 $ T 3 N
