{-# Language NoMonomorphismRestriction, OverloadedStrings,
             MultiParamTypeClasses, DeriveFunctor, DeriveGeneric #-}
module Camera where
import Hyperbolic
import Linear
import Control.Lens
toV4 (Point a b c d) = V4 a b c d

data Projector a = Projector {
                    projectorPosition::(Point a),
                    projectorDirection::(Point a),
                    projectorHorizontal::(Point a),
                    projectorVertical::(Point a)
                     } deriving (Show, Eq, Functor)

data Camera a = Camera { 
                  cameraPosition::Point a,
                  cameraDirection::Point a,
                  cameraVertical::Point a
                } deriving (Show, Eq, Functor)
{-
fromCamera::Camera a -> Projector a
fromCamera (Camera cp0 cd cv) = Projector pp0 pd ph pv where
  pp0 = cp0
  pd = cd
  pv = cv ^-^ formV cv cd *^ cd -- просто скопипастил, х з что тут должно быть
  ph =  cp0 + (cd - cp0) `cross` (pv - cp0) -}


-- |4x4 matrix inverse.
detinv44 :: Fractional a => M44 a -> M44 a
detinv44 (V4 (V4 i00 i01 i02 i03)
          (V4 i10 i11 i12 i13)
          (V4 i20 i21 i22 i23)
          (V4 i30 i31 i32 i33)) =
  let s0 = i00 * i11 - i10 * i01
      s1 = i00 * i12 - i10 * i02
      s2 = i00 * i13 - i10 * i03
      s3 = i01 * i12 - i11 * i02
      s4 = i01 * i13 - i11 * i03
      s5 = i02 * i13 - i12 * i03
      c5 = i22 * i33 - i32 * i23
      c4 = i21 * i33 - i31 * i23
      c3 = i21 * i32 - i31 * i22
      c2 = i20 * i33 - i30 * i23
      c1 = i20 * i32 - i30 * i22
      c0 = i20 * i31 - i30 * i21
  in            V4 (V4 (i11 * c5 - i12 * c4 + i13 * c3)
                       (-i01 * c5 + i02 * c4 - i03 * c3)
                       (i31 * s5 - i32 * s4 + i33 * s3)
                       (-i21 * s5 + i22 * s4 - i23 * s3))
                   (V4 (-i10 * c5 + i12 * c2 - i13 * c1)
                       (i00 * c5 - i02 * c2 + i03 * c1)
                       (-i30 * s5 + i32 * s2 - i33 * s1)
                       (i20 * s5 - i22 * s2 + i23 * s1))
                   (V4 (i10 * c4 - i11 * c2 + i13 * c0)
                       (-i00 * c4 + i01 * c2 - i03 * c0)
                       (i30 * s4 - i31 * s2 + i33 * s0)
                       (-i20 * s4 + i21 * s2 - i23 * s0))
                   (V4 (-i10 * c3 + i11 * c1 - i12 * c0)
                       (i00 * c3 - i01 * c1 + i02 * c0)
                       (-i30 * s3 + i31 * s1 - i32 * s0)
                       (i20 * s3 - i21 * s1 + i22 * s0))
{-# INLINE detinv44 #-}
{-
В принципе, надо доказать, что можно спроецировать все точки на квазиполярную гиперплоскость и
вызвать функции сишной библиотеки, чтобы смотреть уже из этой плоскости



-}


{-
delta :: Point a -> Point a -> Point a -> Plane a 
--плоскость, проходящая через две первые точки и перпендикулярная плоскости, проходящей через все три
--(эта функция нужна только чтобы думать, я её удалю)
delta p0 d v = polar v + deltad * (polar d) + deltap0 *(polar p0)
  where det = form p0 d * form d p0 - form p0 p0 * form d d
        deltad = (form p0 p0 *form v p0 - form p0 d * form v p0)/det
        deltap0 = (form v p0 * form d d - form d p0 * form v d)/det-}


data CameraEuclid a = CameraEuclid (V3 a) (V3 a) (V3 a) deriving (Show, Eq, Functor)
-- сделать из v точку, которая будет проецироваться на (0, +inf)
_t = _w

makeMatrix :: Floating a => Camera a -> M44 a
makeMatrix (Camera p0' d' v') = V4 eh ev ed p0 where
  s = formV p0 p0 -- отрицательно
  ed = (formV p0 p0 *^ d ^-^ formV p0 d *^ p0) 
          ^/ sqrt (- formV d d * formV p0 p0 + formV p0 d * formV p0 d)
--в предыдущей строке я поменял знак ^+^ на ^-^, чтобы посмотреть, что будет
--потому что с ^+^ оно не лежало в плоскости, полярной p0
  ev' = (v ^-^ formV v ed / formV ed ed *^ ed ^-^ formV v p0 / s *^ p0)
  ev = ev' ^/ sqrt (formV ev' ev') ^* sqrt (-s)

        {-^/ sqrt ( formV v v + 1 * formV v ed * formV v ed * formV ed ed 
                   + 1*formV v p0 * formV v p0/s)-}
--в предыдущей строке я поменял знак у подкоренного выражения, чтобы посмотреть, что будет
  eh' = (detinv44 (V4 p0 ed ev (V4 0 0 0 1)) ^.column _t) & _t %~ (* (-1))
  [p0, d, v] = map toV4 [p0', d', v']
  eh = eh' ^/ sqrt (-formV eh' eh'/formV p0 p0)

view :: Floating a => Camera a -> Point a -> (a, a)
view c@(Camera p0' d' v') p@(Point x' y' z t) = (x/z, y/z)
  --  where V4 x y z t = p & toV4 & (^/ form p p) & (^* form p0' p0'){-where 
    where V4 x y z t =   (( makeMatrix c )) !* (toV4 p)



viewP :: Floating a => Camera a -> Point a -> V4 a
viewP c@(Camera p0' d' v') p@(Point x' y' z t) = V4 x y z t
  --  where V4 x y z t = p & toV4 & (^/ form p p) & (^* form p0' p0'){-where 
    where V4 x y z t =  (inv44 ( makeMatrix c ))    !* (toV4 p) 
{-
toPolarHyperplaneAndOrthogonalProjectOnOxyzHyperplanePoint :: Fractional a => Point a -> (a, a, a)
toPolarHyperplaneAndOrthogonalProjectOnOxyzHyperplanePoint p0 p = V3 a
  where V4 x y z _ = toV4 p ^* form p0 p0 ^/ form p p
tPHAOPOOHP = toPolarHyperplaneAndOrthogonalProjectOnOxyzHyperplanePoint
toPolarHyperplaneAndOrthogonalProjectOnOxyzHyperplaneCamera :: Camera a -> CameraEuclid a
toPolarHyperplaneAndOrthogonalProjectOnOxyzHyperplaneCamera c = CameraEuclid p0' d' v'
  where Camera p0 d v = c
        [p0', d', v'] = map (tPHAOPOOHP p0) [p0, d, v]

viewEuclid :: CameraEuclid a -> V3 a -> (a, a)
viewEuclid (CameraEuclid p0 v d) p = 
-}
{-
camera :: Point a -> Point a -> Point a -> Projector a 
-- eye, point projected to (0,0), point projected to (0, y)
camera p0 d v = Projector p0 d h' v'
        where h' = 

              ez' = form p0 p0 *! d + form p0 d *! p0 
                     !/ sqrt (form d d * form p0 p0 + 3 * form p0 d * form p0 d)
              evo' = form p0 p0 *! vo + form p0 vo *! p0 
                     !/ sqrt (form vo vo * form p0 p0 + 3 * form p0 vo * form p0 vo)
              eho' 

et = p0


       -}