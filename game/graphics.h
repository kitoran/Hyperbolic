#ifndef GRAPHICS
#define GRAPHICS
#include "util/physics.h"
#include "util/commongraphics.h"
namespace G {
extern bool frame;
extern bool wheCons;
//displayGame :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)
//                                         => -}
//                 Console -> Bool -> Mesh -> Bool -> M44 Double -> AvatarPosition -> IO ()
void renderConsole() {}

void displayGame(const Mesh &st, const std::vector<Mesh> &its, const Mesh &ray, const Mesh &re, const Mesh &inv, const Matrix44 &tran) {
    auto transform = [&](Point p) {
        Point a = G::persViewMatrix * ( tran * p);
        glVertex4dv(G::saneVertex4(a).data);
    };
    auto toRaw = [&transform](const ColoredEntity& ce) {
        if(ce.e.type == Polygon) {
            renderPrimitive(GL_POLYGON, [&transform, &ce](){

                glColor4fv((float*)(&(ce.color)));
//                applyNormal(ce.e.p);

                for(auto x:ce.e.p) {
                    transform(x);
                }
            });
    //            glDisable(GL_DEPTH_TEST);
    //            renderPrimitive(GL_POINTS, [&transform, &applyNormal, bb, qwq, &ce](){
    //                glColor4f(1, 1, 1, 1);
    //                for(auto x:ce.e.p) {
    //                    transform(x);
    //                }
    //            });
    //            glEnable(GL_DEPTH_TEST);

        } else if(ce.e.type == Segment) {
            renderPrimitive(GL_LINES, [&](){
                glColor4fv((float*)(&(ce.color)));
                transform(ce.e.a);
                transform(ce.e.b);
            });
        } else {
            renderPrimitive(GL_POINTS, [&](){
                glColor4dv((double*)(&(ce.color)));
                transform(ce.e.a);
            });
        }
    };
//  matrixMode $= Projection
//  -- print cons
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
//        -- putStrLn $ "What displayGame sees:" <> show tranw
//    glEnabke(GL_LIGHTING);//lighting           $= Enabled
//  preservingMatrix $ do
//    -- matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
//    -- multMatrix matrixx
//    position (Light 0) $= lpos; //-- saneVertex4 0.3 0.1 0.15 (1::GLfloat)
    for(auto& e : st) {
        toRaw(e);
    }
    for(auto& e : its) {
        for(auto& y : e) {
            toRaw(y);
        }
    }
    for(auto& e : ray) {
        toRaw(e);
    }
    for(auto& e : re) {
        toRaw(e);
    }
    for(auto& e : inv) {
        toRaw(e);
    }
    glColor4f(1, 1, 1, 0.1);
    renderPrimitive( GL_TRIANGLES, []() {
        double x = 0.5, y = 0, z = 0, t = 1;
        glVertex4dv(saneVertex4({x-0.01, y, z, t}).data);

        glVertex4dv(saneVertex4({x,(y-0.01), z, t}).data);
        glVertex4dv(saneVertex4({x, y, (z-0.01), t}).data);
    });
    if(frame) {
      glColor3f(0, 0, (0));
      for(auto&r:st) {
          if(r.e.type == Polygon) {
              renderPrimitive(GL_LINE_LOOP, [&](){
                for(auto &q : r.e.p){
                    transform(q);
                }
              });
          }
      }
    }
    glColor3f(0, 0, (1));
//    -- clear [ColorBuffer, DepthBuffer]
    glWindowPos2f ( 0, 0);
//    lighting           $= Disabled
//    -- texture $= Disabled
//    -- renderS1tring Helvetica18 $
//    -- windowPos $ Vertex3 0 (h) (0::GLfloat)
//    auto transform2//:: Point Double -> V4 GLdouble
//        = [&](const Point &p) /*{- (H.Point x y z t)-}*/ {
//            Point p2 = persViewMatrix * ( tran *((p & _t %~ negate) ^. _v4 ) )  in --transform p = let (V4 x y z t) = transposeMink tran !* toV4 p  in
////                    {- when ((x/t)>0) -}
//            return Point{p2.x/p2.t, p2.y/p2.t, p2.z/p2.t, 1};
//    };
//        P.Polygon tri = snd $ head env
//        [aaa, bbb, ccc] = map transform2 $ tri
    if(wheCons) {
          renderConsole();
    };
//    -- (when  (not whecons)
//    --        (renderText $ T.intercalate "\n" [showt (_pos state !* origin3),
//    --                                          (showt $ _height state),
//    --                                          showt aaa,
//    --                                          "dist " <> showt (distance origin ((inv44 tran) !$ origin)),
//    --                                          "chDist " <> showt (chDistance origin ((inv44 tran) !$ origin)),
//    --                                          "vect " <> showt ((inv44 tran) !$ origin),
//    --                                          "insanity " <> showt (H.insanity tran),
//    --                                          mareix tran]))

    SDL_GL_SwapWindow(window);
//  -- return ()
//          applyNormal (a:b:c:_) = normal $ Normal3 (coerce x :: GLdouble) (coerce y) (coerce z)
//            where
//              V3 x1 y1 z1 = signorm $ cross ( klein a-klein b ) (klein a - klein c)
//              V3 x y z = if z1 < 0 then V3 x1 y1 z1 else negate (V3 x1 y1 z1)

}
Matrix44 viewPort(const AvatarPosition& ap) {
    return  H::rotateAroundY (-ap.nod) * H::moveAlongZ (-ap.height) * (H::m33_to_m44M ( H::transposeMink3 (ap.pos)));
}
bool containsZero (const std::vector<Vector2>& v) {
        bool res = false;
        int s = v.size();
    for(int i = 0; i< s; i++) {
        if(v[i].x*v[(i+1)%s].y-v[(i+1)%s].x*v[i].y) {
           res = true;
           break;
        }
    }
    return res;
}
struct MutableMesh {
    Mesh rays;
    std::vector<Mesh> items;
    Mesh recvs;
};
enum RayEnd {infinity, someReceiver};
struct UnfoldRayResult {
   std::vector<Point> line;
   struct {
       RayEnd end;
       union {
           Absolute abs;
           int i;
       };
   } eit;
};
UnfoldRayResult unfoldRay(std::vector<Deviator>, std::vector<Receiver>, Point p, Absolute a) {
   return {{}, {infinity, {a}}};
}
//unfoldRay listd listr pos dir = first (pos:) $ go pos dir -- (pos:map fst (go pos dir ), last $ dir : map snd (go pos dir ))
//  where go :: H.Point Double -> H.Absolute Double ->  ([(H.Point Double)], Either (H.Absolute Double)  Int)
//        go pos dir  = case foldMaybes listd listr pos dir of
//                           Infinity -> ([], Left dir)
//                           Receiver (p, i) -> ([p], Right i)
//                           Deviator (p, newd) -> case go p newd of (ps, re) -> (p:ps, re)  -- (deleteNth i list)
Matrix44 thatTransformation(const Deviator&) {
   return identity;
}
//thatTransformation (P.Devi pos dir d) = let move = moveRightTo pos -- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
//                                            dirFromStart = (toNonPhysicalPoint $ transposeMink move !$ dir)
//                                            turn = (H.getPointToOxyAroundOy `andThen`  getPointToOxzAroundOz) dirFromStart
//                                         in (move !*!  transposeMink turn !*! (rotateAroundX d))

MutableMesh toMesh (const std::vector<Source> &s,  const std::vector<Receiver> &r, const LevelState& ls)
{

//toMesh s r (P.LS (P.AP pos height nod _) mi (P.WS de di) sel) =

    auto mapping = [&ls, &r](const Source & source) -> std::pair<std::vector<ColoredEntity>, std::vector<int> >{
        UnfoldRayResult urr = /*(line, eit)*/  unfoldRay( ls.worldState.devis, r, source.p, source.a);
        if(urr.eit.end == infinity) {
            std::vector<ColoredEntity> res;
            if(urr.line.size()>=2) {
                for(int i = 0; i < urr.line.size()-2; i++) {
                    res.push_back({{1,1,1,1}, {Segment,{},urr.line[i], urr.line[i+1]}});
                }
            }
            if(urr.line.size()>=1) {
            res.push_back({{1,1,1,1}, {Segment,{},urr.line[urr.line.size()-1],
                                                    toNonPhysicalPoint(urr.eit.abs)}});
            }
            return {res, {}};
        } else {
            std::vector<ColoredEntity> res;
            for(int i = 0; i < urr.line.size()-1; i++) {
                res.push_back({{1,1,1,1}, {Segment,{},urr.line[i], urr.line[i+1]}});
            }
            return {res, {urr.eit.i}};
        }
    };
    std::vector<ColoredEntity> lines;
    std::vector<int> indices;
    for(const auto& d: s) {
        auto pair = mapping(d);
        lines.insert(lines.end(), pair.first.begin(), pair.first.end());
        indices.insert(indices.end(), pair.second.begin(), pair.second.end());
    }
    std::vector<ColoredEntity> recvs;
    for(int ind = 0; ind < r.size(); ind++) {
        if(std::find(indices.begin(), indices.end(), ind) < indices.end()) {
            recvs.push_back({{1, 1, 0, 1}, {Polygon, r[ind]}});
        } else {
            recvs.push_back({{0, 1, 0, 1}, {Polygon, r[ind]}});
        }
    }
    std::vector<Mesh> items;
    for(const auto& dede: ls.worldState.devis) {
        items.push_back(thatTransformation(dede) * deviator());
    }
    return {Mesh{lines}, items, Mesh{recvs}};
}

}
//    -- inv = case mi of
//    --     Nothing -> mempty
//    --     Just P.De -> transposeMink (viewPort (P.AP pos height nod 0))!*! H.moveAlongX 0.011 !*! H.moveAlongZ (-0.012) !$ deviator

#endif // GRAPHICS

