module Common (head_, tail_) where

-- | Like 'head', but with a more specific error message in case the argument is
-- empty. This is primarily defined to avoid incurring @-Wx-partial@ warnings
-- whenever 'head' is used.
head_ :: [a] -> a
head_ (x:_) = x
head_ []    = error "head_: Empty list"

-- | Like 'tail', but with a more specific error message in case the argument is
-- empty. This is primarily defined to avoid incurring @-Wx-partial@ warnings
-- whenever 'tail' is used.
tail_ :: [a] -> [a]
tail_ (_:xs) = xs
tail_ []     = error "tail_: Empty list"
