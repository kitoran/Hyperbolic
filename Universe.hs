module Universe (module Hyperbolic, points, camera, module Projection) where
import Projection
import Hyperbolic

tau::Floating a => a
tau = 2*pi 
camera :: Projector Double
camera = Projector  { projectorPosition = Point 0 0 (sinh 1) $ cosh 1,
                      projectorDirection = Point 0 0 0 1,
                      projectorHorizontal = Point (sinh 1) 0 0 $ cosh 1,
                      projectorVertical = Point 0 (sinh 1) 0 $ cosh 1 }
            where sq2 = sqrt 2

points :: [Point Double]
points = [Point (sqrt ((t+1)*(t+1))) 0 0 t| t <- [-10, -9.6 .. 10]]
            ++[Point (sin t * sinh r) (cos t * sinh r) 0 (cosh r) |
                                            t <- [0, 0.1.. tau], r <- [0, 1, 2, 3, 4]]

    {-[Point t 0 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 t 0 (sqrt (t+1)) | t <- [-10, -9.9..10]] ++
         [Point 0 0 t (sqrt (t+1)) | t <- [-1, -0.99..1]] 
-}

{-[Point 0 0 0 1, Point 1 0 0 (sqrt 2), Point 0 1 0 (sqrt 2), 
                                  Point (-1) 0 0 (sqrt 2), Point 0 (-1) 0 (sqrt 2)]-}

