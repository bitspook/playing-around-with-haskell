module MyWriter where

newtype MyWriter m a = MyWriter (a, m)

instance Functor (MyWriter m) where
  fmap f (MyWriter (a, m)) = MyWriter (f a, m)

instance Monoid m => Applicative (MyWriter m) where
  pure a = MyWriter (a, mempty)
  (<*>) (MyWriter (f, m1))  (MyWriter (a, m2)) = MyWriter (f a, m1 <> m2)

instance Monoid m => Monad (MyWriter m) where
  (>>=) (MyWriter (a, m1)) g = MyWriter (b, m1 <> m2)
    where
      MyWriter (b, m2) = g a

tell :: Monoid m => m -> MyWriter m ()
tell m = MyWriter ((), m)
