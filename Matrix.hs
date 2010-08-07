module Matrix where


type Length = Double
type Vector2 = (Length, Length)
(.*:) :: Length -> Vector2 -> Vector2
s .*: (x, y) = (s*x, s*y)
(+:) :: Vector2 -> Vector2 -> Vector2
(a, b) +: (a', b') = (a+a', b+b')

type Matrix2 = (Vector2, Vector2)
(.*#) :: Length -> Matrix2 -> Matrix2
s .*# (u, v) = (s .*: u, s .*: v)
(#*:) :: Matrix2 -> Vector2 -> Vector2
(v, u) #*: (x, y) = (x .*: v) +: (y .*: u)
(#*#) :: Matrix2 -> Matrix2 -> Matrix2
m #*# (c, d) = (m #*: c, m #*: d)
type Transformation = (Matrix2, Vector2)
(#:*#:) :: Transformation -> Transformation -> Transformation
(m, v) #:*#: (m', v') = (m #*# m', v +: v')
(#:*:) :: Transformation -> Vector2 -> Vector2
(m, u) #:*: v = (m #*: v) +: u
zeroV :: Vector2
zeroV = (0, 0)
idM :: Matrix2
idM = ((1, 0), (0, 1))

scale :: Length -> Transformation
scale k = (k .*# idM, zeroV)
translate :: Vector2 -> Transformation
translate v = (idM, v)
