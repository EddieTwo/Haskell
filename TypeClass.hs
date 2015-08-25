class YesNo a where
   yesno :: a -> Bool

instance YesNo Int where
   yesno 0 = False
   yesno _ = True


instance YesNo Bool where
   yesno = id


class Eq' a where
   (~=~) :: a -> a -> Bool
   (/=~) :: a -> a -> Bool

instance Eq' a => Eq' (Maybe a) where
   Just x /=~ Just y = x /=~ y
   Nothing /=~ Nothing = True
   _ /=~ _ = True
   Just x ~=~ Just y = True
   Nothing ~=~ Nothing = True
   _ ~=~ _ = True

class Functor' f where
   fmap' :: (a -> b) -> f a -> f b
