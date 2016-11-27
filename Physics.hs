module Physics where

import Hyperbolic


--текущее положение  матрица куда надо пойти   результат (?) 
correct :: M44 Double -> M44 Double -> M44 Double
correct

--   старое положение    новое положение
pushOut :: M44 Double -> M44 Double
pushOut
--\operatorname{ch}(x \pm y)=\operatorname{ch}x\,\operatorname{ch}y \pm \operatorname{sh}y\,\operatorname{sh}x.
--           центр сферы чосинус радиуса новое положение
pushOutOne :: Point Double -> Double -> M44 Double
pushOutOne m r = let 
                        diff = r - distance origin m
                        if diff > 0 then moveTo m (-diff) else identity
