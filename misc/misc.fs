let flip f a b = f b a
let (++) a = flip (+) a
