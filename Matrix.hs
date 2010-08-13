module Matrix where
import qualified Graphics.Rendering.OpenGL as GL

-- Key for operator names:
-- 
-- - . is scalar
-- 
-- - : is vector
-- 
-- - # is matrix
-- 
-- - #: is transformation
--
-- so, e.g., #:*: is application of a transformation to a vector.
--
-- :+: was renamed +: because only data constructors can be called /:.*/
type Scalar = GL.GLdouble
type Vector2 = (Scalar, Scalar)
(.*:) :: Scalar -> Vector2 -> Vector2
s .*: (x, y) = (s*x, s*y)
(+:) :: Vector2 -> Vector2 -> Vector2
(a, b) +: (a', b') = (a+a', b+b')
(-:) :: Vector2 -> Vector2 -> Vector2
(a, b) -: (a', b') = (a-a', b-b')
toNormal :: Vector2 -> (Vector2, Scalar)
toNormal v@(x, y) = (a, r)
  where r = sqrt(x*x + y*y)
        a = (1/r) .*: v
absV :: Vector2 -> Scalar
absV v = r where (_a, r) = toNormal v

type Matrix2 = (Vector2, Vector2)
(.*#) :: Scalar -> Matrix2 -> Matrix2
s .*# (u, v) = (s .*: u, s .*: v)
(#*:) :: Matrix2 -> Vector2 -> Vector2
(v, u) #*: (x, y) = (x .*: v) +: (y .*: u)
(#*#) :: Matrix2 -> Matrix2 -> Matrix2
m #*# (c, d) = (m #*: c, m #*: d)
type Transformation = (Matrix2, Vector2)
(#:*#:) :: Transformation -> Transformation -> Transformation
(m@(m0, m1), v) #:*#: (m', v'@(x', y')) =
  (m #*# m', v +: v' +: (x' .*: m0) +: (y' .*: m1))
(#:*:) :: Transformation -> Vector2 -> Vector2
(m, u) #:*: v = (m #*: v) +: u
zeroV :: Vector2
zeroV = (0, 0)
idM :: Matrix2
idM = ((1, 0), (0, 1))

scale :: Scalar -> Transformation
scale k = (k .*# idM, zeroV)
translate :: Vector2 -> Transformation
translate v = (idM, v)
rotate :: Scalar -> Transformation
rotate a = (((c, -s), (s, c)), zeroV)
  where s = sin a
        c = cos a