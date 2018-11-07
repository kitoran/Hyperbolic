#ifndef GRAPHICS
#define GRAPHICS
#include <iostream>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_ttf.h>
//#include <list>

#include "util/physics.h"
#include "commongraphics.h"
namespace G {
extern bool frame;
extern bool wheCons;
//displayGame :: {- forall a c. (Floating a, Ord a, Real a, Coercible Double c, Coercible Double a, Show a)
//                                         => -}
//                 Console -> Bool -> Mesh -> Bool -> M44 Double -> AvatarPosition -> IO ()
TTF_Font* sans = 0;
std::vector<std::string> history = {"console1",
                                    "console2"};
std::string console;
void renderLine(const std::string & line, int lineNumber) {
if(!sans) sans = TTF_OpenFont("/usr/share/fonts/truetype/freefont/FreeSans.ttf", 20);
if(line.empty()) return;
SDL_Surface * sFont = TTF_RenderText_Blended(sans, line.data(), {255,255,255,255});
GLuint texture;
glGenTextures(1, &texture);
glBindTexture(GL_TEXTURE_2D, texture);


glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, sFont->w, sFont->h, 0, GL_BGRA, GL_UNSIGNED_BYTE, sFont->pixels);

glBegin(GL_QUADS);
{
    static const double inter = 35;
  glTexCoord2i(0,1); glVertex2f(-1, -1+(lineNumber*inter )/height);
  glTexCoord2f(1/*0 + sFont->w*/, 1);/*(1,0);*/ glVertex2f(-1 + ((double)(sFont->w))*2/width, -1+(lineNumber*inter )/height);
  auto d = ((double)(sFont->w))*2/width; (void)d;
  glTexCoord2f(0 + 1, 0 );/*(1,1);*/ glVertex2f(-1 + ((double)(sFont->w))*2/width, -1+(lineNumber*inter )/height + ((double)(sFont->h))*2/height);
  glTexCoord2f(0, 0 );/*(0,1);*/ glVertex2f(-1, -1+(lineNumber*inter )/height + ((double)(sFont->h))*2/height);
}
glEnd();


glDeleteTextures(1, &texture);
SDL_FreeSurface(sFont);
}

void renderConsole() {
    glDisable(GL_DEPTH_TEST);
      glEnable(GL_TEXTURE_2D);
        for(int i = 0; i < history.size(); i++) {
           renderLine(history[i], history.size() - i);
        }
        renderLine(console, 0);

      glDisable(GL_TEXTURE_2D);
      glEnable(GL_DEPTH_TEST);


//    if(!sans){
////        sans = TTF_OpenFont("Sans.ttf", 10);
//        if(!sans) sans = TTF_OpenFont("/usr/share/fonts/truetype/freefont/FreeSans.ttf", 10);
//    };
//    char const* s = "Console text";
//    int h;
//    TTF_SizeUTF8(sans, s, 0, &h);
//    SDL_Surface* sur = TTF_RenderText_Solid(sans, s, {0,0,255,255}); // (L.V4 0 0 255 255) s

//    std::cout << "sdl: " << SDL_GetError() << "ttf: " << TTF_GetError () << std::endl;
//    SDL_Rect r = {0,0,width,height - h};
//    int err = SDL_BlitSurface(sur, &r, SDL_GetWindowSurface(window), 0);
//    std::cout << "error: " << err << " " << SDL_GetError() << std::endl;
//    glDisable(GL_DEPTH_TEST);
//    renderPrimitive(GL_TRIANGLES, [](){
//        glVertex4dv(saneVertex4({0, 0, 0, 1}).data);

//        glVertex4dv(saneVertex4({1,0, 0, 1}).data);
//        glVertex4dv(saneVertex4({0, 1, 0, 1}).data);
//    });
}

void displayGame(const Mesh &st, const std::vector<Mesh> &its, const Mesh &ray, const Mesh &re, const Mesh &inv, const Matrix44 &tran) {
    auto transform = [&](Point p) {
        Point a = G::persViewMatrix * ( tran * p);
        glVertex4dv(G::saneVertex4(a).data);
    };
    auto toRaw = [&transform](const ColoredEntity& ce) {
//        if(ce.e.type == Polygon) {
            renderPrimitive(ce.e.type == Polygon ? GL_POLYGON :
                           ce.e.type == Segment ? GL_LINES: GL_POINTS, [&transform, &ce](){

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

//        } else if(ce.e.type == Segment) {
//            renderPrimitive(GL_LINES, [&](){
//                glColor4fv((float*)(&(ce.color)));
//                transform(ce.e.a);
//                transform(ce.e.b);
//            });
//        } else {
//            renderPrimitive(GL_POINTS, [&](){
//                glColor4dv((double*)(&(ce.color)));
//                transform(ce.e.a);
//            });
//        }
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
    int s = v.size();
    for(int i = 0; i< s; i++) {
        if(v[i].x*v[(i+1)%s].y-v[(i+1)%s].x*v[i].y<0) {
           return false;
        }
    }
    return true;
}
struct MutableMesh {
    Mesh rays;
    std::vector<Mesh> items;
    Mesh recvs;
};
enum RayEndType {infinity, someReceiver};
struct UnfoldRayResult {
   std::vector<Point> line;
   struct RayEnd {
       RayEndType end;
       union {
           Absolute abs;
           int i;
       };
   } eit;
};
enum FoldMaybesResultType {one_infinity, one_receiver, one_deviator};
struct FoldMaybesResult {
    FoldMaybesResultType type;
    Point p;
    union {
        int i;
        Absolute dir;
    };
};
struct FunctionDeResult {
  double dis;
    Absolute diir;
};
boost::optional<FunctionDeResult> functionDe(Point pos, Absolute dir, Deviator de) {
/*functionDe pos dir (P.Devi dpos ddir d) = */
    Matrix44 move = moveRightTo(pos);
    Point dirFromStart = toNonPhysicalPoint(inv44(move) * dir);
    Matrix44 turn = andThen(getPointToOxyAroundOy, getPointToOxzAroundOz)(dirFromStart);
    Matrix44 trans = turn * inv44(move);
    Point res = trans * de.pos;
    Absolute newDir = inv44( trans) * (rotateAroundX (-de.nod)) * (moveAlongX (distance (origin, res))) * (Absolute{ 0, 1, 0}) ;
    bool cond = fabs(res. y) < 0.001 && fabs(res.z) < 0.001 && fabs(res.x*res.t) > 0.00001; // и ещё условие что девиатор правильно повёрнут
    if (cond) {
        return {{res.x/res.t, newDir}};
    }else {
        return boost::none;
    }
}
boost::optional<Point> functionRcv(Point pos, Absolute dir, Receiver re) {
//functionRcv pos dir (P.Receiver ss@(p1:p2:p3:ps)) =
    Matrix44 trans = getTriangleToOxy (re[0], re[1], re[2]);
    Point p /*@(H.Point px py pz pt)*/ = trans * pos;
    Point a/*@(H.Point ax ay az at)*/ = trans * (toNonPhysicalPoint (dir));
    Point intersect;
    FOR4(i) {
        intersect.data[i] = p.data[i] + (-p.z/a.z) * a.data[i]; //    (\pc ac -> pc + (-pz/az) * ac) <$> p <*> a
    }
    Point i = normalizeKlein (intersect);
    const double& ix = i.x, &iy = i.y;
    std::vector<Vector2> projs(re.size());
    for(int i = 0; i < re.size(); i++) {
        Point p = normalizeKlein (trans * re[i]);
        projs[i] = {p.x-ix, p.y-iy};
    }
    if (containsZero (projs)) {
        return transposeMink (trans) * intersect;
    } else {
        return boost::none;
    }
}
template<typename T>
boost::optional<T> minimumByDist(const std::vector<T>& s) {
    auto it = std::min_element(s.begin(), s.end(), [](const T&t1, const T&t2) {
        return t1.dist < t2.dist;
    });
    if(it >= s.end()) {
        return boost::none;
    } else {
        return {*it};
    }
}

auto foldMaybes(const std::vector<Deviator>& listd, const std::vector<Receiver>& lists,
                const H::Point& pos, const H::Absolute& dir) -> FoldMaybesResult {
/*foldMaybes listd lists pos dir = */
    struct DDe {
        double dist;
        Point pos;
        Absolute dir;
    };
    struct DRe {
        double dist;
        Point pos;
        int re;
    };
    auto maxFst = [](const boost::optional<DDe>& de, const boost::optional<DRe>& re) -> FoldMaybesResult {
        if(de.is_initialized()) {
            if(re.is_initialized()) {
                if(boost::get(de).dist < boost::get(re).dist) {
                    FoldMaybesResult r; r.dir = boost::get(de).dir; r.p = boost::get(de).pos; r.type = one_deviator;
                    return r;
                } else {
                    return {one_receiver, boost::get(re).pos, boost::get(re).re};
                }
            } else {
                FoldMaybesResult r; r.dir = boost::get(de).dir; r.p = boost::get(de).pos; r.type = one_deviator;
                return r;
            }
        } else {
            if(re.is_initialized()) {
                return {one_receiver, boost::get(re).pos, boost::get(re).re};
            } else {
                return {one_infinity, origin, -1};
            }
        }
    };
    std::vector<DDe> listDe;
    for(const auto& dev : listd) {
        auto discriminee = functionDe(pos, dir, dev);
        if(discriminee.is_initialized()) {
            listDe.push_back({boost::get(discriminee).dis, dev.pos, boost::get(discriminee).diir});
        }
    }
    std::vector<DRe> listRcv;
    for(int i = 0; i < lists.size(); i++) {
        auto discriminee = functionRcv(pos, dir, lists[i]);
        if(discriminee.is_initialized()) {
            listRcv.push_back({distance(pos, boost::get(discriminee)), boost::get(discriminee), i});
        }
    }
    return maxFst (minimumByDist(listDe), minimumByDist(listRcv));
}

void push_front(const Point& p, std::vector<Point>*v) {
    std::vector<Point> res {p};
    std::copy(v->begin(), v->end(), std::back_inserter(res));
    v->swap(res);
}
UnfoldRayResult unfoldRay(const std::vector<Deviator>& listd, const std::vector<Receiver>& listr,
                           const Point& pos, const Absolute & dir) {
    std::function<UnfoldRayResult(const Point&, Absolute)> go =  [&listd, &listr, &go] (const Point& pos, Absolute dir) -> UnfoldRayResult {
        auto discriminee = foldMaybes(listd, listr, pos, dir);
        if(discriminee.type == one_infinity) {
            return {{}, {infinity, dir}};
        } else if(discriminee.type == one_receiver) {
            UnfoldRayResult::RayEnd e;
            e.end = someReceiver;
            e.i = discriminee.i;
            return {{discriminee.p}, e};
        } else if(discriminee.type == one_deviator) {
            UnfoldRayResult r = go(discriminee.p, discriminee.dir);
            push_front(discriminee.p, &r.line);
            return r;
        } else std::terminate();
    };
    UnfoldRayResult r = go(pos, dir);
    push_front(pos, &r.line);
    return r;
}
//unfoldRay listd listr pos dir = first (pos:) $ go pos dir -- (pos:map fst (go pos dir ), last $ dir : map snd (go pos dir ))
//  where go :: H.Point Double -> H.Absolute Double ->  ([(H.Point Double)], Either (H.Absolute Double)  Int)
//        go pos dir  = case foldMaybes listd listr pos dir of
//                           Infinity -> ([], Left dir)
//                           Receiver (p, i) -> ([p], Right i)
//                           Deviator (p, newd) -> case go p newd of (ps, re) -> (p:ps, re)  -- (deleteNth i list)
Matrix44 thatTransformation(const Deviator& de) {
    Matrix44 move = moveRightTo (de.pos);
    Point dirFromStart = (toNonPhysicalPoint (transposeMink (move) * de.dir));
    Matrix44 turn = andThen(getPointToOxyAroundOy, getPointToOxzAroundOz)( dirFromStart);
    return move * transposeMink(turn) * rotateAroundX(de.nod);
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
                for(int i = 0; i < urr.line.size()-1; i++) {
                    res.push_back({{1,1,1,1}, {Segment,{urr.line[i], urr.line[i+1]}}});
                }
            }
            if(urr.line.size()>=1) {
            res.push_back({{1,1,1,1}, {Segment,{urr.line[urr.line.size()-1],
                                                    toNonPhysicalPoint(urr.eit.abs)}}});
            }
            return {res, {}};
        } else {
            std::vector<ColoredEntity> res;
            for(int i = 0; i < urr.line.size()-1; i++) {
                res.push_back({{1,1,1,1}, {Segment,{urr.line[i], urr.line[i+1]}}});
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

