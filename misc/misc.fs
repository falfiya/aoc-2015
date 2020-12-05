let inline flip f a b = f b a
let inline (++) a = flip (+) a
let inline (>>>) f0 f1 a b = f1 (f0 a b)
let inline (<-->) fn0 fn1 arg = ((fn0 arg), (fn1 arg))
let fork: ('T -> 'U)[] -> 'T -> 'U[] = flip Array.map
