#include "gameloop.h"
#include "console.h"
#include "clip.h"
#include "fmt/format.h"
OptionalDouble intersectRay( const Deviator & d, const Matrix44 &transs) {
    Matrix44 move = moveRightTo(d.pos);
    Point dirFromStart = toNonPhysicalPoint (transposeMink (move) * d.dir);
    Matrix44 turn = andThen(getPointToOxyAroundOx,  getPointToOxzAroundOz)( dirFromStart);
    Matrix44 trans = (move * turn) ;
    Mesh list = /*{-transposeMink-}*/ trans * G::deviator(d.size);
    bool any = false;
    for(int i = 0; i < list.size(); i++) {
        std::vector<Vector2> r;
        std::transform(list[i].e.p.begin(), list[i].e.p.end(), std::back_inserter(r), [&transs](const Point& p){
            Point point = transs * p;
            return Vector2{point.y/point.t, point.z/point.t};
        });
        if(G::containsZero(r)) {
           any = true;
           break;
        }
    }
    if(any) {
        return {distance(d.pos, origin)};
    } else {
        return  boost::none;
    }
}
OptionalInt foldMaybesP(const std::vector<Deviator> &list, const Matrix44 &ttt) {
    struct taggedDist {
        int i;
        double dist;
    };
    std::vector<taggedDist> listt;
    for(int i = 0; i < list.size(); i++) {
        auto ma = intersectRay (list[i], ttt );
        if( ma.is_initialized()) {
            listt.push_back ({i, boost::get(ma)}/*{-, ((P._devPos) dev, diir)-}*/); //--]filter (map (\e@(P.Devi poos _ _) -> ) list)
        }
    }
    if(listt.empty() ) {
        return boost::none;
    }
    int r = listt[0].i;
    double m = listt[0].dist;
    for(int i = 1; i < listt.size(); i++) {
        if(listt[i].dist < m) {
            r = listt[i].i;
            m =  listt[i].dist;
        }
    }
    return r;
}

OptionalInt findSelected(WorldState ws, AvatarPosition ap) {
    return foldMaybesP( ws.devis, (/*{-transposeMink- $-}*/ G::viewPort(ap)));
    // -- тут мы много раз считаем преобразование, а надо один если вообще считать
}

LevelState processInventory(const Matrix44 &trans, LevelState ap) {
    if(ap.inventory.type == Item::De) {
        //        Matrix44 nmat = m33_to_m44M(ap.avatarPosition.pos);
        ap.inventory.type = Item::Empty;
        ap.worldState.devis.push_back({trans*Point{0, 0, 0, 1}, trans*Absolute{1,0,0}, 0, ap.inventory.size});
        ap.selected = boost::none;;
        return ap;
    } else if(ap.inventory.type == Item::Empty) {
        if(!ap.selected.is_initialized()) {
            return ap;
        } else {
            ap.inventory.type = Item::De;
            ap.inventory.size = ap.worldState.devis[boost::get(ap.selected)].size;
            ap.worldState.devis.erase(ap.worldState.devis.begin()+boost::get(ap.selected));
            ap.selected = boost::none;
            return ap;
        }
    }
    std::terminate();
}

SqDistanceFromProjRes sqDistanceFromProj(const HyperEntity &he, const Matrix44 &trans) {
    Point h1 = trans * he.p[0];
    Point h2 = trans * he.p[1];
    Vector2 v1 = {((h1.y)/h1.x), (h1.z/h1.x)};
    Vector2 v2 = {((h2.y)/h2.x), (h2.z/h2.x)};
    auto numeratorRoot = (v2.x)*v1.y - v2.y*(v1.x);
    auto denominator = (v2.y-v1.y)*(v2.y-v1.y) + (( v2.x)-( v1.x))*(v2.x-v1.x);
    auto dis = numeratorRoot*numeratorRoot / denominator;
    auto ratio =  (v1.x*(v1.x-v2.x) + v1.y*(v1.y-v2.y) )/denominator;
    auto nx = v1.x + ratio*(v2.x-v1.x);
    auto rr = (nx*h1.x - h1.y)/((-h1.y)+h2.y*(h1.t/h2.t)+nx*h1.x-nx*h2.x*(h1.t/h2.t));
    return {dis, rr};
    // castedRay = transpose trans !$ Ray (0 0 0 1) neare
    // castedRay = transpose trans !$ Point (1 nx ny t)
    // (t nx*t ny*t 1) = (1-a)*(x1 y1 z1 t1) + a*(x2 y2 z2 t2) where everthing is normalizedKlein
    // t = (1-a)*x1+a*x2
    // nx*t = (1-a)*y1+a*y2
    // (1-a)*y1+a*y2 = nx*((1-a)*x1+a*x2)
    // y1 + a*(-y1+y2) = nx*x1-nx*a*x1+nx*a*x2
    // a*(-y1+y2+nx*x1-nx*x2) = nx*x1 - y1
    // a = (nx*x1 - y1)/(-y1+y2+nx*x1-nx*x2)
    // если восстановить t:
    // a = (nx*x1 - y1)/(-y1+y2(t1/t2)+nx*x1-nx*x2(t1/t2))
}

Matrix44 preShow(const Mesh &rays, const Matrix44 &vp) {
    //preShow rays vp = case rays of
    //    auto rrr = moveAlongX(-0.01) * G::viewPort(state.avatarPosition);
    //    return transposeMink(rrr);
    if (rays.size()<1) return moveRightTo({NAN, 0, 0, 1});
    auto res = sqDistanceFromProj(rays[0].e, vp);
    double ratio = res.rr;
    double dis = res.dis;
    int index = 0;
    for(int i = 1; i < rays.size(); i++) {
        auto res = sqDistanceFromProj(rays[i].e, vp);
        if(res.dis < dis) {
            index = i;
            dis = res.dis;
            ratio = res.rr;
        }
        //        ((_, ratio), index) = minimumBy (compare `on` (fst.fst)) $ zip (map (\(_::(Double, Double, Double, Double), s) -> ) $ coerce rays) [0..]
    }

//    fmt::print(stderr, "\nsqdis: {} {}\n", dis, ratio);
    //    (_::(Double, Double, Double, Double), P.Segment pos dir) = (coerce rays :: [((Double, Double, Double, Double), HyperEntity)]) !! index
    Matrix44 rayToOx = identity;// = let move = //-- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
    const Point& pos = rays[index].e.p[0];
    const Point& dir= rays[index].e.p[1];
    {
        auto move = moveRightTo (pos);
        auto dirFromStart = (transposeMink (move) * dir);
        auto toOxy = getPointToOxyAroundOy(dirFromStart);
        fmt::print(stderr, "toOxy*Oz = {}\n", toOxy * Point{0,0,1,1});
        Point onOxy = toOxy*dirFromStart;
        auto toOx = getPointToOxzAroundOz(onOxy);
        fmt::print(stderr, "toOx*Oz = {}\n", toOx * Point{0,0,1,1});
        auto turn = toOx * toOxy;

        rayToOx = ( move * transposeMink( turn) );
    }
//    fmt::print(stderr, "{}, {}, {}, {}\n", rayToOx*pos,
//               rayToOx*dir, transposeMink(rayToOx)*pos,
//               transposeMink(rayToOx)*dir);
    //    Point dx dy dz dt = dir
    //    Point px py pz pt = pos
    Point p = (1-ratio)*normalizeKlein (vp * pos) + ratio*normalizeKlein (vp * dir);
    auto trans = /*Debug.Trace.trace ("qqqq "++show x1) $*/
            ( rayToOx) * (moveAlongX (   (H::distance ( vp * pos, p))));
    //    Point h1 = vp * p;
    //    double x1 = (-h1.y)/h1.x;
    if(rays.empty()) {
        return transposeMink (vp) * H::moveAlongX(0.011) * H::moveAlongZ (-0.012);
    }
    else {
        return trans ;
    }
    //    _ -> trans

}

AvatarPosition tick(double gravity, const std::vector<RuntimeObstacle> &level, AvatarPosition s) {
    s = applySpeed(s);
    s = applyGravity(gravity, s);
    s = pushOut (level, s);
    if( s.height > 8) {
        s.height = 7.99;
        s.speed.z = -s.speed.z;
    }
    return s;
}
bool continueCycle = true;
void keyboardProcess() {
    const Uint8 *state = SDL_GetKeyboardState(nullptr);
    if(state[SDL_SCANCODE_ESCAPE] || (state[SDL_SCANCODE_F4] && (state[SDL_SCANCODE_LALT] || state[SDL_SCANCODE_RALT]))) {
        continueCycle = false;
    }
    //    SDL_Scancode c = a.keysym.scancode;
    static int comInhibited = 0;
    if(!comInhibited && state[SDL_SCANCODE_LCTRL]) {
        comInhibited = 10;
        if(state[SDL_SCANCODE_TAB]) {
            G::wheCons = !G::wheCons;
        }
        if(state[SDL_SCANCODE_C]) {
            Vector3 ph = globals::state.avatarPosition.pos * origin3;
            Point p4h{ph.x, ph.y, 0, ph.t};
            Point p = moveRightTo(p4h)*Point{0, 0, sinh(globals::state.avatarPosition.height),
                                                   cosh(globals::state.avatarPosition.height)};
            clip::set_text(fmt::format("{{{}, {}, {}, {}}}", p.x, p.y, p.z, p.t));
        }
        if(state[SDL_SCANCODE_D]) {
            Vector3 ph = globals::state.avatarPosition.pos * Vector3{1,0,1};
            Point p{ph.x, ph.y, 0, ph.t};
//            Point p = moveRightTo(p4h)*Point{0, 0, sinh(globals::state.avatarPosition.height),
//                                                   cosh(globals::state.avatarPosition.height)};
            clip::set_text(fmt::format("{{{}, {}, {}, {}}}", p.x, p.y, p.z, p.t));
        }
        if(state[SDL_SCANCODE_E]) {
            noclip = !noclip;
        }
    }
    if (comInhibited>0) comInhibited--;
    if(G::wheCons) {
        if(!comInhibited ) {
            bool keystroke = false;
            if(state[SDL_SCANCODE_UP]) {
                int d = history.size();
                //                               when (keysymKeycode a == KeycodeUp) (modifyIORef consoleRef consoleUp)
                if(positionInHistory == history.rend()) {
                    savedConsole = console;
                }
                positionInHistory--;
                console = *positionInHistory;
                keystroke = true;
            } else if(state[SDL_SCANCODE_DOWN]) {
                //                               when (keysymKeycode a == KeycodeUp) (modifyIORef consoleRef consoleUp)
                positionInHistory++;
                if(positionInHistory == history.rend()) {
                    console = savedConsole;
                } else {
                    console = *positionInHistory;
                }
                keystroke = true;
            } else if(state[SDL_SCANCODE_LCTRL] && (state[SDL_SCANCODE_D] || state[SDL_SCANCODE_C])) {
                continueCycle = false;
                keystroke = true;
            } else if(state[SDL_SCANCODE_RETURN]) {
                positionInHistory = history.rend();
                addToHistory(console);
                processLine();
                console = "";
                keystroke = true;
            } else if(state[SDL_SCANCODE_BACKSPACE]) {
                if(console.size() > 0) {
                    console.resize(console.size() - 1);
                }
                keystroke = true;
            } else {
                SDL_Scancode s;
                for(SDL_Scancode code = SDL_SCANCODE_A; code < SDL_SCANCODE_0; code=SDL_Scancode(code+1)) {
                    if(state[code]) {
                        keystroke = true;
                        s = code;
                    }
                }
                if(keystroke) {
                    console += char(SDL_GetKeyFromScancode(s));
                }
            }
            if(keystroke) comInhibited = 10;
        }
    } else {
        processKeyboard(state);
    }
    //                               if (keysymKeycode a == KeycodeTab && ctrl a) then (modifyIORef wheConsoleRef not) else do
    //                                wheCon <- readIORef wheConsoleRef
    //                                if wheCon then do
    //                                 -- let act = if (a == '\r') then '\n' else if (a == '\b') then '\b' else a
    //                                 -- hPutChar inp  act
    //                                 -- hFlush inp
    //                                 -- modifyIORef consoleRef (echo a)
    //                                 when (ctrl a && (keysymKeycode a == KeycodeC || keysymKeycode a == KeycodeD)) $ exitSuccess
    //                                  -- do
    //                                 (cons::Console) <- readIORef consoleRef
    //                                 return ()-- newConsole <- MTL.execStateT (interactive commands a) cons FIMXE
    //                                 -- writeIORef consoleRef newConsole FIMXE

    //                                          else do
    //                               -- when (not wheCon) $ do

    //                               -- \a _ -> case a of
    //                               -- 'q' -> leaveMainLoop
    //                               -- 'c' -> do
    //                               --         displayCallback $= do
    //                               --                                                                                  state' <- readIORef state
    //                               --                                                                                  mesh <- readIORef meshRef
    //                               -- --                                                                                  displayGame (mesh) (viewPort state')
    //                               --         modifyIORef consoleShown not
    //                               -- _   -> modifyIORef state $ processKeyboard a)
    //-- gameSpecialCallback a _ = do
    //                               -- when (a == GL.KeyUp) (modifyIORef consoleRef consoleUp)
}

void processKeyboard(const Uint8 *c) {
    if(c[SDL_SCANCODE_W]) {
        if(noclip) {
            double nod = state.avatarPosition.nod;
            Vector3 e = state.avatarPosition.pos * origin3;
            Matrix44 cu = fromAvatarPosition(state.avatarPosition);
            Vector3 e2 = projectToOxy(cu * origin);
            FOR3(i) {
                fprintf(stderr, "%lf ", e[i]);
            }
            fprintf(stderr, "\n");
            FOR3(i) {
                fprintf(stderr, "%lf ", e2[i]);
            }
            fprintf(stderr, "\n");
            fprintf(stderr, "%lf %lf\n", distance3(e, e2), step);
            assert(distance3(e, e2) < 0.01);
            cu = cu * moveAlongX(step);
            state.avatarPosition = toAvatarPosition(cu);

            state.avatarPosition.nod = nod;
        } else {
            state.avatarPosition.pos = state.avatarPosition.pos * moveAlongX3(step);
        }
    }
    if(c[SDL_SCANCODE_S]) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongX3(-step);
    }
    if(c[SDL_SCANCODE_A]) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongY3(step);
    }
    if(c[SDL_SCANCODE_D]) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongY3(-step);
    }
    if(c[SDL_SCANCODE_Z]) {
        state.avatarPosition.height += step;
    }
    if(c[SDL_SCANCODE_C]) {
        state.avatarPosition.height -= step;
    }
    if(c[SDL_SCANCODE_R]) {
        state = startState();
        //                    KeycodeR -> reset
    }
    if(c[SDL_SCANCODE_SPACE]) {
        std::cerr << "Jump!" << std::endl;
        state.avatarPosition.speed.z += jump;
    }
}

AvatarPosition processTurnLeft(double angle, const AvatarPosition &ap) {
    return {(ap.pos * (rotate3 (- angle))), ap.height, ap.nod, ap.speed};// -- Sudya po vsemu, skorost' nuzhno menyat' zdes' ili v processMove
}

void gameDisplay() {
    //                            state' <- readIORef stateRef
    //                            levelMesh <- readIORef levelMeshRef
    //                            (rays, itemss, recvs) <- readIORef mutableMeshRef
    //                            cons <- readIORef consoleRef
    //                            frame <- readIORef frameRef
    //                            sources <- readIORef sourceRef
    //                            wheCons <- readIORef wheConsoleRef
    auto ap = state.avatarPosition;
    Matrix44 ps = preShow(mutableMesh.rays, G::viewPort(ap));
    auto inv = state.inventory.type == Item::Empty ? Mesh{} :
                                          state.inventory.type == Item::De ? ps*G::transparentDeviator() : (abort(), Mesh{});
    fmt::print(stderr, "preshow for show {}", ps*Point{1,0,0,1});
    auto items =  mutableMesh.items;
    if(state.selected.is_initialized()) {
        G::lightenABit(&items[boost::get(state.selected)]);
    }
    G::displayGame(levelMesh, items, mutableMesh.rays, mutableMesh.recvs, inv, G::viewPort( ap));
}

AvatarPosition processTurnUp(double angle, const AvatarPosition &ap) {
    return {ap.pos, ap.height, bound((ap.nod + angle), -tau/4, tau/4 ), ap.speed};
}

AvatarPosition processMouse(int x, int y, const AvatarPosition &ap) {
    std::cerr << x << " " << y << std::endl;
    auto  fromGradi = [](auto x) {
        //        auto q =
        return (x / 360.0*tau*7.0/30.0);
    };
    //    auto fdf = fromGradi(-y);
    return processTurnUp ( fromGradi (y),  processTurnLeft ( fromGradi ( x), ap ));
}

void mouseCCase() {
    Matrix44 trans = preShow(mutableMesh.rays, (G::viewPort(state.avatarPosition)));
    //    auto oldState = state;

    fmt::print(stderr, "preshow for place {}", trans*Point{1,0,0,1});
    state = processInventory( trans, state);
    mutableMesh = toMesh(source, receivers, state);
}

void processTimer() {
    if(!noclip) {
        state.avatarPosition = tick(gravity, obs, state.avatarPosition);
    }
}

AvatarPosition applySpeed(const AvatarPosition &ap){
    return {(ap.pos * ( moveToTangentVector3 ({ap.speed.x, ap.speed.y})) ), (ap.height+ap.speed.z), ap.nod, ap.speed};
    // это очень очень неправильно
}

AvatarPosition applyGravity(double gravity, AvatarPosition state) {
    state.speed = Vector3{ state.speed.x, state.speed.y, (state.speed.z - gravity/(cosh (state.height))/(cosh (state.height)))};
    return state;
}

int filter(void *, SDL_Event *event) {
    return event->type == SDL_QUIT || event->type == SDL_WINDOWEVENT;
}

void gameLoop() {
    TTF_Init();
    SDL_SetHintWithPriority(SDL_HINT_MOUSE_RELATIVE_MODE_WARP, "1", SDL_HINT_OVERRIDE);
    //    SDL_SetRelativeMouseMode(SDL_TRUE);
    SDL_SetEventFilter(filter,
                       nullptr);
    //    auto tty = Console_Create(
    //                window,
    //                "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    //                10,
    //                SDLK_TAB,
    //                f,
    //                NULL);
    //(void)tty;
    SDL_WarpMouseInWindow(window, width/2, height/2);
    //    SDL_GL_SetSwapInterval(0);
    //    SDL_SetWindowGrab(window, SDL_TRUE);
    //    SDL
    //                           SDL_FALSE);
    glEnable(GL_DEPTH);
    glEnable(GL_TEXTURE_2D);

    levelMesh = level().mesh;
    for(const Source& s : level().sources) {

        Mesh sm = G::sourceMesh();

        sm = moveRightTo(s.p) * sm;


        levelMesh.insert(levelMesh.end(), sm.begin(), sm.end());
    }

    uint a = SDL_GetTicks();
    int cycles = 0;
    bool focus = true;
    int mouseInhibited = 0;
    while(continueCycle) {
        SDL_Event event;
        if (SDL_PollEvent(&event)) {
            if(event.type == SDL_WINDOWEVENT) {
                if(event.window.event == SDL_WINDOWEVENT_FOCUS_GAINED) focus = true;
                else if(event.window.event == SDL_WINDOWEVENT_FOCUS_LOST) focus = false;
            } else if(event.type == SDL_QUIT) {
                continueCycle = false;
            }
        }
        //        uint32_t flags = SDL_GetWindowFlags(window);
        //        std::cout << focus << std::endl<< std::endl;
        if(focus) {

            int x ,y;
            uint res = SDL_GetMouseState(&x, &y);
            if(x != width/2 || y != height/2) {
                state.avatarPosition = processMouse( x-width/2, y-height/2, state.avatarPosition);
                state.selected = state.inventory.type == Item::Empty ? findSelected(state.worldState, state.avatarPosition) : boost::none;
                SDL_WarpMouseInWindow(window, width/2, height/2);
            }
            if(res &  SDL_BUTTON(SDL_BUTTON_LEFT) && !mouseInhibited) {
                mouseCCase();
                mouseInhibited = 6;
            }
            if(mouseInhibited>0) mouseInhibited--;

            //
            keyboardProcess();
            processTimer();
            //Console_Draw(tty);       SDL_GL_SwapWindow(window);
            gameDisplay();
            //            SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
            cycles++;
            uint newa = SDL_GetTicks();
            if(newa - a >= 1000) {
                std::cerr << "кадров за прошедшую секунду" << cycles << std::endl;
                a = newa;
                cycles = 0;
            }
        } else {
            SDL_GL_SwapWindow(window);
        }
    }

}
