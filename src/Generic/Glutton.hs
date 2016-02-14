module Generic.Glutton where
import Control.Monad
import Control.Applicative

data Glutton i o = Satiated o | Hungry (i -> Glutton i o)

-- Satiate a glutton with a sequence of input items.
-- Unchecked precondition: input list should be long enough to satiate glutton.
feed :: Glutton i g -> [i] -> g
feed (Hungry feedGlutton) (i:is) = feed (feedGlutton i) is
feed (Hungry _)           []     = error "food did not satiate glutton"
feed (Satiated g)         _      = g

-- Satiate a glutton by repeatedly performing an IO action to generate the inputs.
-- The IO action should be able to generate food until the glutton is satiated.
-- !! I never tested this function !!
feedIO :: Glutton i g -> IO i -> IO g
feedIO (Hungry feedGlutton) a = a >>= \i -> feedIO (feedGlutton i) a
feedIO (Satiated g)         _ = return g

-- Create a glutton that is satiated by a single food item.
nibbler :: (i -> o) -> Glutton i o
nibbler f = Hungry $ \i -> Satiated (f i)

instance Functor (Glutton i) where
    fmap t (Hungry feedGlutton) = Hungry $ \i -> let glutton = feedGlutton i
                                                 in fmap t glutton
    fmap t (Satiated a)         = Satiated (t a)

instance Applicative (Glutton i) where
    pure a = Satiated a
    (Satiated f) <*> g = f <$> g
    (Hungry feedGlutton) <*> f = Hungry $ \i -> let g = feedGlutton i in g <*> f

instance Monad (Glutton i) where
    return = pure
    (Satiated a) >>= f = f a
    (Hungry feedGlutton) >>= f = Hungry $ \i -> let g = feedGlutton i in g >>= f
