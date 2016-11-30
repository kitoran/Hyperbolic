module Physics where

import Hyperbolic
import Universe
import Debug.Trace
import Linear(M44, (!*!))

--текущее положение  матрица куда надо пойти   результат (?) 
-- correct :: M44 Double -> M44 Double -> M44 Double
-- correct 

--   старое положение    новое положение
pushOut :: Obstacles Double -> M44 Double -> M44 Double
pushOut (Obs a) currentPos = foldr (\(center, radius) -> (pushOutOne center radius (transposeMink currentPos !$ origin) !*!)) currentPos a
-- Тут мы много раз умножаем на identity, это можно оптимизировать разными 
-- способами, самый безболезненный, мне кажется - это добавить конструктор 
-- Identity в тип преобразований (M44)

-- \operatorname{ch}(x \pm y)=\operatorname{ch}x\,\operatorname{ch}y \pm \operatorname{sh}y\,\operatorname{sh}x.
--            центр сферы     радиус новое положение
-- pushOutOneOrigin :: Point Double -> Double -> M44 Double
-- pushOutOneOrigin m r = let 
--                         diff = r - distance origin m
--                        in  if (trace ("diff:" ++ show diff) diff) > 0 then moveTo m (-diff) else identityIm

-- pushOutOne :: Point Double -> Double -> Point Double -> M44 Double
-- pushOutOne center radius pos = {-commute (transposeMink $ moveRightTo pos) $-} pushOutOneOrigin (moveRightTo pos !$ center) radius

pushOutOne :: Point Double -> Double -> Point Double -> M44 Double
pushOutOne m r pos = let 
                        diff = r - distance pos  m
                       in  if (trace ("diff:" ++ show diff) diff) > 0 then moveFromTo pos m (-diff) else identityIm