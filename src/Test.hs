import Data.Monoid
foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap g = mconcat . map g

foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
foldComposing f = foldMap (Endo . f)

foldr :: (a -> (b -> b)) -> b -> [a] -> b
foldr f y0 xs = appEndo (foldComposing f xs) y0
