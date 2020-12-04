let inline flip f a b = f b a
let inline (++) a = flip (+) a
let inline (>>>) f0 f1 a b = f1 (f0 a b)
