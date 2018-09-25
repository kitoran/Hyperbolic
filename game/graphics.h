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

#endif // GRAPHICS

