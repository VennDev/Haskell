data LinkedList a = Empty | Node a (LinkedList a)

isEmpty :: LinkedList a -> Bool
isEmpty Empty = True
isEmpty _     = False

prepend :: a -> LinkedList a -> LinkedList a
prepend x list = Node x list

head' :: LinkedList a -> Maybe a
head' Empty      = Nothing
head' (Node x _) = Just x

tail' :: LinkedList a -> LinkedList a
tail' Empty        = Empty
tail' (Node _ xs)  = xs
