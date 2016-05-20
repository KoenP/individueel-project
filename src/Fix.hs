{-# LANGUAGE UndecidableInstances #-}
module Fix where

-- Generic fixed point and annotation datatype.
newtype Fix f = In {out :: f (Fix f)} deriving Show
data Ann x f a = Ann {getAnn :: x, dumpAnn :: f a }

--instance Show (f (Fix f)) => Show (Fix f) where
    --show (In x) = "In " ++ show x

instance Functor f => Functor (Ann ann f) where
    -- fmap :: (a -> b) -> Ann z a -> Ann z b
    fmap f (Ann a e) = Ann a (fmap f e)

stripAnnotations :: Functor f => Fix (Ann ann f) -> Fix f
stripAnnotations (In annotated) =
    let (Ann _ free) = fmap stripAnnotations annotated
    in In free

annotate :: ann -> f (Fix (Ann ann f)) -> Fix (Ann ann f)
annotate ann f = In $ Ann ann f

mapAnnF :: (f x -> g y) -> Ann ann f x -> Ann ann g y
mapAnnF t (Ann a e) = Ann a (t e)

mapAnn :: (ann -> bnn) -> Ann ann f x -> Ann bnn f x
mapAnn t (Ann a e) = Ann (t a) e
