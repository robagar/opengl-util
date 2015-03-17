module Misc where

(.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:) = (.) . (.)
infixl 8 .:

maybeDo_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo_ = flip maybeM_

maybeDo :: Monad m => Maybe a -> b -> (a -> m b) -> m b
maybeDo m b f = maybeM b f m

maybeM_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM_ f = maybe (return ()) f

maybeM :: Monad m => b -> (a -> m b) -> Maybe a -> m b
maybeM b f = maybe (return b) f

