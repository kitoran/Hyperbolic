﻿#include "console.h"
#include "graphics.h"
#include "gameloop.h"
#include <fmt/format.h>

using namespace G;
void G::renderConsole() {
    glDisable(GL_DEPTH_TEST);
    int i = 0;
    for(std::list<std::string>::const_iterator it = history.cbegin();
        it != history.end();
        i++, it++) {
        renderLine(*it, history.size() - i);
    }
    renderLine(console, 0);

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
//char frame[
void renderStats() {
    glDisable(GL_DEPTH_TEST);
    H::Matrix33 pos = globals::state.avatarPosition.pos;
    renderLine(std::to_string(insanity3(pos)), 0, false);
    Vector3 p = pos * origin3;
    renderLine(fmt::format("x/t = {:.4}, y/t = {:.4}, t = {:.4}a", p.x/p.t, p.y/p.t, p.t), 1, false);
    renderLine(fmt::format("h = {:.4}", globals::state.avatarPosition.height), 2, false);
    renderLine(fmt::format("dir = {}", pos * Vector3{1,0,1}), 3, false);
    renderLine(fmt::format("pos = {:.4} {:.4} {:.4}", pos[0], pos[1], pos[2]), 4, false);
    renderLine(fmt::format("      {:.4} {:.4} {:.4}", pos[3], pos[4], pos[5]), 5, false);
    renderLine(fmt::format("      {:.4} {:.4} {:.4}", pos[6], pos[7], pos[8]), 6, false);
    glEnable(GL_DEPTH_TEST);
}
void G::renderLine(const std::string &line, int lineNumber, bool bottom) {
    if(!mono) mono= TTF_OpenFont("/usr/share/fonts/truetype/freefont/FreeMono.ttf", 20);
    if(line.empty()) return;
    SDL_Surface * sFont = TTF_RenderText_Blended(mono, line.data(), {255,255,255,255});
    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);


    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, sFont->w, sFont->h, 0, GL_BGRA, GL_UNSIGNED_BYTE, sFont->pixels);

    glBegin(GL_QUADS);
    {
        double lineHeight = ((double)(sFont->h))*2/height;
        static const double inter = 35;
        glTexCoord2i(0,1); glVertex2f(-1, bottom?-1+(lineNumber*inter )/height
                                               :1-(lineNumber*inter )/height-lineHeight);
        glTexCoord2f(1/*0 + sFont->w*/, 1);/*(1,0);*/ glVertex2f(-1 + ((double)(sFont->w))*2/width, bottom?
                                                                     -1+(lineNumber*inter )/height
                                                                   :1-(lineNumber*inter )/height-lineHeight);
        auto d = ((double)(sFont->w))*2/width; (void)d;
        glTexCoord2f(0 + 1, 0 );/*(1,1);*/ glVertex2f(-1 + ((double)(sFont->w))*2/width, bottom?
                                                          -1+(lineNumber*inter )/height + lineHeight
                                                        :1-(lineNumber*inter )/height);
        glTexCoord2f(0, 0 );/*(0,1);*/ glVertex2f(-1, bottom?-1+(lineNumber*inter )/height + lineHeight
                                                           :1-(lineNumber*inter )/height);
    }
    glEnd();


    glDeleteTextures(1, &texture);
    SDL_FreeSurface(sFont);
}

void G::predisplay() {
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
void G::display(const std::vector<Mesh> &mesh, const Matrix44 &tran, bool AlternativeBuffer) {
    if(AlternativeBuffer) {

        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, selectionFramebuffer);
//        glDisable(GL_TEXTURE_2D);

        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }
    int i = 0;
    auto transform = [&](Point p) {
        Point a = G::persViewMatrix * ( tran * p);
//        if(!AlternativeBuffer) {
            if(i%3 == 0) {
    //            glTexCoord2d(231.0/500, 39.0/375);
                glTexCoord2d(2281.0/2480, 1070.0/3508);
            } else if(i%3 == 1) {
    //            glTexCoord2d(402.0/500, 137.0/375);
                glTexCoord2d(1678.0/2480, 25.0/3508);
            } else if(i%3 == 2) {
    //            glTexCoord2d(358.0/500, 331.0/375);
                            glTexCoord2d(1076.0/2480, 1070.0/3508);
            } else if(i%5 == 3) {
                glTexCoord2d(135.0/500, 349.0/375);
            } else if(i%5 == 4) {
                glTexCoord2d(64.0/500, 164.0/375);
            }
            i++;
//        }
        glVertex4dv(G::saneVertex4(a).data);
    };
    auto toRaw = [&](const ColoredEntity& ce) {

        glBegin(ce.e.type == Polygon ? GL_POLYGON :
                                       ce.e.type == Segment ? GL_LINES: GL_POINTS);
        glColor4fv((float*)(&(ce.color)));
        bool front = true;
        bool back = true;
        Point last = G::persViewMatrix * ( tran * ce.e.p.back());
        for(auto x:ce.e.p) {
            Point a1 = G::persViewMatrix * ( tran * x);
                if(i%3 == 0) {
                    glTexCoord2d(2281.0/2480, 1070.0/3508);
                } else if(i%3 == 1) {
                    glTexCoord2d(1678.0/2480, 25.0/3508);
                } else if(i%3 == 2) {
                    glTexCoord2d(1076.0/2480, 1070.0/3508);
                } else if(i%5 == 3) {
                    glTexCoord2d(135.0/500, 349.0/375);
                } else if(i%5 == 4) {
                    glTexCoord2d(64.0/500, 164.0/375);
                }
                i++;
    //        }
            glVertex4dv(G::saneVertex4(a1).data);
            Point a = normalizeKlein(a1);
            if(a.x*last.y-last.x*a.y<0) {
                front = false;
            }
            if(a.x*last.y-last.x*a.y>0) {
                back = false;
            }
            last = a;
        }
        Point v1 = normalizeKlein(G::persViewMatrix * ( tran *ce.e.p[0]));
        Point v2 = normalizeKlein(G::persViewMatrix * ( tran *ce.e.p[1]));
        Point v3 = normalizeKlein(G::persViewMatrix * ( tran *ce.e.p[2]));
        double denom = (v2.y-v3.y)*(v1.x-v3.x)+(v3.x-v2.x)*(v1.y-v3.y);
        double w1 = ((v1.y-v3.y)*(-v3.x)+(v3.x-v2.x)*(-v3.y))/denom;
        double w2 = ((v3.y-v1.y)*(-v3.x)+(v1.x-v2.x)*(-v3.y))/denom;
        double w3 = 1-w1-w2;
        double depth = v1.z*w1+v2.z*w2+v3.z*w3;
        glEnd();
        if((front || back) && (depth >= -1 && depth <= 1)) {
            fprintf(stderr, "intersecting face %p"
                            " front %s back %s at depth %lf"
                            "\n", &ce, front?"true":"false",
                    back?"true":"false", depth);
        }
    };
    //  matrixMode $= Projection
    //  -- print cons
    //        -- putStrLn $ "What displayGame sees:" <> show tranw
    //    glEnabke(GL_LIGHTING);//lighting           $= Enabled
    //  preservingMatrix $ do
    //    -- matrixx <- (newMatrix RowMajor $ m44toList tran :: IO (GLmatrix GLdouble))
    //    -- multMatrix matrixx
    //    position (Light 0) $= lpos; //-- saneVertex4 0.3 0.1 0.15 (1::GLfloat)

    for(auto& e : mesh) {
//        if(AlternativeBuffer) {

//            const void* add = (&e);
//            GLbyte* theColor = (GLbyte*)&add;
//            theColor[3]=theColor[2]=theColor[1]=theColor[0]=0;
//            glColor4bv(theColor);
//        }
        for(auto& y : e) {
            toRaw(y);
        }
    }
    if(frame) {
        glColor3f(0, 0, (0));
        for(auto&y:mesh) {
            for(auto& r : y) {
                if(r.e.type == Polygon) {
                    glBegin(GL_LINE_LOOP);
                    for(auto &q : r.e.p){
                        transform(q);
                    }
                    glEnd();
                }
            }
        }
    }

    //    glDisable(GL_DEPTH_TEST);
        glDisable(GL_TEXTURE);
        glEnable(GL_TEXTURE_2D);
    glBegin(GL_TRIANGLES);
    glColor3ub(255,127,0);
    for(Vector2* pp : {&A, &B, &C}) {
        Vector2& p = *pp;
        constexpr int n = 10;
        constexpr double w = 10;
        double windx = ((p.x+1)*width)/2;
        double windy = (p.y+1)*height/2;
        double lx = p.x, ly = p.y;
        for(int i = 0; i < n+1; i++) {
            double wxc = windx+w*cos(tau/n*i);
            double wyc = windy+w*sin(tau/n*i);
            double x = wxc*2/width-1;
            double y = wyc*2/height-1;


            glColor3ub(255-255.0/(n+1)*i,127,255.0/(n+1)*i);
            glVertex3d(p.x, p.y, 0);
            glVertex3d(x, y, 0);
            glVertex3d(lx, ly, 0);
            lx = x; ly = y;
            if(pp == &B) fprintf(stderr, "lx = %lf, ly = %lf", lx, ly);
        }
    }
        glColor3b(0,127,0);
        glVertex3d(A.x, A.y, 0);
        glVertex3d(B.x, B.y, 0);
        glVertex3d(C.x, C.y, 0);
    glEnd();
//    glEnable(GL_DEPTH_TEST);


    if(AlternativeBuffer) {

        glEnable(GL_TEXTURE_2D);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }
}
void G::postdisplay() {
    glColor4f(1, 1, 1, 0.1);
    glBegin(GL_TRIANGLES);
        double x = 0.5, y = 0, z = 0, t = 1;
        glVertex4dv(saneVertex4({x-0.01, y, z, t}).data);

        glVertex4dv(saneVertex4({x,(y-0.01), z, t}).data);


        glVertex4dv(saneVertex4({x, y, (z-0.01), t}).data);
    glEnd();

    glColor3f(1, 1, (1));
    glDisable(GL_DEPTH_TEST);
    glBegin(GL_LINES);
        glVertex2d(-5.0/(width/2.0), 0);
        glVertex2d(5.0/(width/2.0), 0);
        glVertex2d(0, -5.0/(height/2.0));
        glVertex2d(0, 5.0/(height/2.0));
    glEnd();
    glEnable(GL_DEPTH_TEST);

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
    renderStats();
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

Matrix44 G::viewPort(const AvatarPosition &ap) {
    return  H::rotateAroundY (-ap.nod) * H::moveAlongZ (-ap.height) * (H::m33_to_m44M ( H::transposeMink3 (ap.pos)));
}

bool G::containsZero(const std::vector<Vector2> &v) {
    int s = v.size();
    bool front = true;
    for(int i = 0; i< s; i++) {
        if(v[i].x*v[(i+1)%s].y-v[(i+1)%s].x*v[i].y<0) {
            front = false;
        }
    }
    bool back = true;
    for(int i = 0; i< s; i++) {
        if(v[i].x*v[(i+1)%s].y-v[(i+1)%s].x*v[i].y>0) {
            back = false;
        }
    }
    return front || back;
}

boost::optional<G::FunctionDeResult> G::functionDe(Point pos, Absolute dir, Deviator de) {
    /*functionDe pos dir (P.Devi dpos ddir d) = */
    Matrix44 move = moveRightTo(pos);
    Point dirFromStart = toNonPhysicalPoint(inv44(move) * dir);
    Matrix44 turn = andThen(getPointToOxyAroundOy, getPointToOxzAroundOz)(dirFromStart);
    Matrix44 trans = turn * inv44(move);
    Point res = trans * de.pos;
    Absolute newDir = inv44( trans) * (rotateAroundX (-de.nod)) * (moveAlongX (distance (origin, res))) * (Absolute{ 0, -1, 0}) ;
    bool cond = fabs(res. y) < 0.001 && fabs(res.z) < 0.001 && fabs(res.x*res.t) > 0.00001; // и ещё условие что девиатор правильно повёрнут
    if (cond) {
        return {{res.x/res.t, newDir}};
    }else {
        return boost::none;
    }
}

boost::optional<Point> G::functionRcv(Point pos, Absolute dir, Receiver re) {
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
G::FoldMaybesResult G::foldMaybes(const std::vector<Deviator> &listd, const std::vector<Receiver> &lists, const Point &pos, const Absolute &dir) {
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

void G::push_front(const Point &p, std::vector<Point> *v) {
    std::vector<Point> res {p};
    std::copy(v->begin(), v->end(), std::back_inserter(res));
    v->swap(res);
}

G::UnfoldRayResult G::unfoldRay(const std::vector<Deviator> &listd, const std::vector<Receiver> &listr, const Point &pos, const Absolute &dir) {
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

Matrix44 G::transformationForDeviator(const Deviator &de) {
    fmt::print(stderr, "making deviator: {}", toNonPhysicalPoint(de.dir));
    Matrix44 move = moveRightTo (de.pos);
    Point dirFromStart = ( (transposeMink (move) * toNonPhysicalPoint(de.dir)));
    Matrix44 toOxy = getPointToOxyAroundOy(dirFromStart);
    Point onOxy = toOxy * dirFromStart;
    Matrix44 toOx =  getPointToOxzAroundOz(onOxy);
    Point onOx = toOx * onOxy;
    Matrix44 res = move * transposeMink(toOxy) *
            transposeMink(toOx) /** rotateAroundX(de.nod)*/;
    Point pos = res*origin;
    Point dir = res*Point{1,0,0,1};
    return res;
}

G::MutableMesh G::toMesh(const std::vector<Source> &s, const std::vector<Receiver> &r, const LevelState &ls)
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
            recvs.push_back({{0.2, 0.2, 0, 1}, {Polygon, r[ind]}});
        }
    }
    std::vector<Mesh> items;
    for(const auto& dede: ls.worldState.devis) {
        items.push_back(transformationForDeviator(dede) * deviator(dede.size));
    }
    std::vector<Mesh> cubes;
    for(const auto& cubebe: ls.worldState.cubes) {
        cubes.push_back(cubebe.pos * cube(cubebe.faceCenterDist));
    }
    return {Mesh{lines}, items, Mesh{recvs}, cubes};
}
