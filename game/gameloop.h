#ifndef GAMELOOP
#define GAMELOOP
#include "util/physics.h"
#include "graphics.h"
#include <time.h>
#include <string>
#include <boost/numeric/ublas/vector.hpp>
#include <iostream>
OptionalDouble intersectRay( const Deviator & d, const Matrix44 &transs) {
    Matrix44 move = moveRightTo(d.pos);
    Point dirFromStart = toNonPhysicalPoint (transposeMink (move) * d.dir);
    Matrix44 turn = andThen(getPointToOxyAroundOx,  getPointToOxzAroundOz)( dirFromStart);
    Matrix44 trans = (move * turn) ;
    Mesh list = /*{-transposeMink-}*/ trans * G::deviator();
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
//             -- ((moveRightTo pos `andConsideringThat`
//                                         -- (getPointToOxyAroundOx `andThen`  getPointToOxzAroundOz))
//                                         -- (toNonPhysicalPoint dir))-}
//    if any (\(_, Polygon a) ->  G.containsZero (map mapping a)) $
//                                                      /*{-  traceShow (map (\(_, Polygon a) -> (map mapping a) ) list)-}*/ list
//                          then Just $ H.distance (pos) origin
//                          else Nothing
//--if trace ("newdir = "++show newDir) cond then Just (x/t, newDir) else Nothing
//    -- res@(H.Point x y z t) = trans !$ dpos
//    -- newDir = {-H.moveAlongY (0.1) !$ H.rotateAroundZ (H.tau/4) !$ ddir -} transposeMink trans !*! rotateAroundX (-d) !*! H.moveAlongX (H.distance H.origin res) !$ (H.Abs 0 1 0) -- } trans !$ (H.Abs 0 1 0)
//    -- cond = abs y < 0.01 && abs z < 0.01 && x*t > 0 -- и ещё условие что девиатор правильно повёрнут
//-- вызывать гиперболический косинус от гиперболического арккосинуса очень весело

//foldMaybesP :: [P.Deviator] -> M44 Double -> Maybe Int -- , H.Absolute Double)
OptionalInt foldMaybesP(const std::vector<Deviator>& list, const Matrix44& ttt) {
    std::vector<double> listt;
    for(auto dev : list) {
        auto ma = intersectRay (dev, ttt );
        if( ma.is_initialized()) {
            listt.push_back (boost::get(ma)/*{-, ((P._devPos) dev, diir)-}*/); //--]filter (map (\e@(P.Devi poos _ _) -> ) list)
        }
    }
    if(listt.empty() ) {
        return boost::none;
    }
    int r = 0;
    double m = listt[0];
    for(int i = 1; i < listt.size(); i++) {
        if(listt[i] < m) {
            r = i;
            m =  listt[i];
        }
    }
    return r;
}
OptionalInt findSelected(WorldState ws, AvatarPosition ap) {
    return foldMaybesP( ws.devis, (/*{-transposeMink- $-}*/ G::viewPort(ap)));
}// -- тут мы много раз считаем преобразование, а надо один если вообще считать


LevelState startState() {
    LevelState r;
    AvatarPosition ap;
    ap.pos = identity33;
    ap.height = 0.1;
    ap.nod = 0;
    ap.speed = Vector3{ 0.0, 0, 0};
    r.avatarPosition = ap;
    r.inventory = Empty;
    r.worldState = { {  Deviator{ {0,
                                                           0,
                                                           0,
                                                           1},
                                                          {1,
                                                           0,
                                                           0},
                                                          0},
                                                               /* moveAlongY (-0.1::Double) !$ Devi (Point 0 0 0 1) (Abs 0 1 0) (0) */
                            },
                            {}
                          };
    r.selected  = boost::none;
    return r;
}
LevelState state = startState();
//-- { _avatarPosition :: AvatarPosition,
//--                        _avatarInventory :: Item,
//--                        _worldState :: WorldState
//--                      }
LevelState thisFunc(const Matrix44& trans, LevelState ap) {
    if(ap.inventory == De) {
//        Matrix44 nmat = m33_to_m44M(ap.avatarPosition.pos);
        ap.inventory = Empty;
        ap.worldState.devis.push_back({trans*Point{0, 0, 0, 1}, trans*Absolute{1,0,0}, 0});
        ap.selected = boost::none;;
        return ap;
    } else if(ap.inventory == Empty) {
        if(!ap.selected.is_initialized()) {
            return ap;
        } else {
            ap.inventory = De;
            ap.worldState.devis.erase(ap.worldState.devis.begin()+boost::get(ap.selected));
            return ap;
        }
    }
    std::terminate();
}
//thisFunc :: M44 Double -> (P.LevelState -> P.LevelState)
//thisFunc trans (LS (ap@(AP mat hei nod _)) (Just De) (WS des dis) _) = let nmat = m33_to_m44M mat in LS ap Nothing (WS (Devi ({-nmat !*! moveAlongZ hei-}trans !$ (Point 0 0 0 1)) ({-nmat-} trans !$ Abs (1) 0 0 ) 0: des) (dis)) Nothing
//thisFunc _ s@(LS (ap@(AP mat hei nod _)) (Nothing) (WS des dis) e) = case e of
//                                                                    Nothing -> s
//                                                                    Just i -> (LS (ap) (Just De) (WS [ x | (ix, x) <- zip [0..] des, ix /= i ] dis) e) -- = LS (ap@(mat hei nod)) (Nothing) (WS des dis) = LS ap Nothing (WS (Devi (mat !*! moveAlongZ hei !$ (Point 0 0 0 1)): des) (dis))
struct SqDistanceFromProjRes {
    double ratio;
    double rr;
};
SqDistanceFromProjRes sqDistanceFromProj(const HyperEntity & he, const Matrix44 &trans) {
    Point h1 = trans * he.p[0];
    Point h2 = trans * he.p[1];
    Vector2 v1 = {((-h1.y)/h1.x), (h1.z/h1.x)};
    Vector2 v2 = {((-h2.y)/h2.x), (h2.z/h2.x)};
    auto numeratorRoot = (v2.x)*v1.y - v2.y*(v1.x);
    auto denominator = (v2.y-v1.y)*(v2.y-v1.y) + (( v2.x)-( v1.x))*(v2.x-v1.x);
    auto dis = numeratorRoot*numeratorRoot / denominator;
    auto ratio =  (v1.x*(v1.x-v2.x) + v1.y*(v1.y-v2.y) )/denominator;
    auto nx = v1.x + ratio*(v2.x-v1.x);
    auto rr = (nx*h1.x - h1.y)/((-h1.y)+h2.y*(h1.t/h2.t)+nx*h1.x-nx*h2.x*(h1.t/h2.t));
    return {dis, rr};
}
//-- castedRay = transpose trans !$ Ray (0 0 0 1) neare
//-- castedRay = transpose trans !$ Point (1 nx ny t)
//-- (t nx*t ny*t 1) = (1-a)*(x1 y1 z1 t1) + a*(x2 y2 z2 t2) where everthing is normalizedKlein
//-- t = (1-a)*x1+a*x2
//-- nx*t = (1-a)*y1+a*y2
//-- (1-a)*y1+a*y2 = nx*((1-a)*x1+a*x2)
//-- y1 + a*(-y1+y2) = nx*x1-nx*a*x1+nx*a*x2
//-- a*(-y1+y2+nx*x1-nx*x2) = nx*x1 - y1
//-- a = (nx*x1 - y1)/(-y1+y2+nx*x1-nx*x2)
//-- если восстановить t:
//-- a = (nx*x1 - y1)/(-y1+y2(t1/t2)+nx*x1-nx*x2(t1/t2))
Matrix44 preShow(const Mesh& rays, const Matrix44& vp) {
//preShow rays vp = case rays of
    if (rays.size()<1) return H::identity;
    auto res = sqDistanceFromProj(rays[0].e, vp);

    double ratio = res.ratio;
    int index = 0;
    for(int i = 1; i < rays.size(); i++) {
        auto res = sqDistanceFromProj(rays[i].e, vp);
        if(res.ratio < ratio) {
            index = i;
            ratio = res.ratio;
        }
//        ((_, ratio), index) = minimumBy (compare `on` (fst.fst)) $ zip (map (\(_::(Double, Double, Double, Double), s) -> ) $ coerce rays) [0..]
    }
//    (_::(Double, Double, Double, Double), P.Segment pos dir) = (coerce rays :: [((Double, Double, Double, Double), HyperEntity)]) !! index
    Matrix44 rayToOx;// = let move = //-- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
    {
        auto move =moveRightTo (rays[index].e.p[0]);
        auto dirFromStart = (transposeMink (move) * rays[index].e.p[1]);
        auto turn = andThen(getPointToOxyAroundOy,  getPointToOxzAroundOz)( dirFromStart);
        rayToOx = (move * transposeMink( turn));
    }
    const Point& pos = rays[index].e.p[0];
    const Point& dir= rays[index].e.p[1];
//    Point dx dy dz dt = dir
//    Point px py pz pt = pos
    Point p = (1-ratio)*normalizeKlein (vp * pos) + ratio*normalizeKlein (vp * dir);
    auto trans = /*Debug.Trace.trace ("qqqq "++show x1) $*/ ( rayToOx) * (moveAlongX (   (H::distance ( vp * pos, p))));
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
//        --
//traceComm s a = Debug.Trace.trace (s ++ " " ++ show a) a
//tick :: Double -> [RuntimeObstacle ] -> AvatarPosition -> AvatarPosition
//tick gravity level = (\s@(AP pos height nod (V3 x y z)) -> if height > 8 then s {_height = 7.99, _speed = V3 x y (-z)} else s). pushOut (level) . applyGravity gravity . applySpeed
AvatarPosition applySpeed (const AvatarPosition& ap);
AvatarPosition applyGravity (double gravity, AvatarPosition state);
AvatarPosition tick (double gravity, const std::vector<RuntimeObstacle>& level, AvatarPosition s) {
    s = applySpeed(s);
    s = applyGravity(gravity, s);
    s = pushOut (level, s);
    if( s.height > 8) {
        s.height = 7.99;
        s.speed.z = -s.speed.z;
    }
    return s;
}

//matricesMoveInPlane :: Floating a => [(Keycode, a -> M33 a)]

//matricesMoveInPlane = {-fmap (\(a, b) -> (a, b (1/cosh a))) -}[(KeycodeW, moveAlongX3 ), (KeycodeS, moveAlongX3 . negate),
//                           (KeycodeA, moveAlongY3 ), (KeycodeD, moveAlongY3 . negate)]

//runtimeObstacles :: [RuntimeObstacle]
//runtimeObstacles = computeObs (_obstacles level)
//level :: Environment
Environment level() {
    Point p0p = Point{ (sinh (1)), 0.0, 0.0, (cosh (1))};
    Point p1p = rotateAroundZ (tau/3) * p0p;
    Point p2p = rotateAroundZ (-tau/3) * p0p;
    Point p0 = moveAlongZ(-0.1)*p0p;
    Point p1 = moveAlongZ(-0.1)*p1p;
    Point p2 = moveAlongZ(-0.1)*p2p;
    Point r0p = {0, 0.1, 0.1, 2};
    Point r1p = rotateAroundX (tau/4) * r0p;
    Point r2p = rotateAroundX (tau/4) * r1p;
    Point r3p = rotateAroundX (tau/4) * r2p;
    std::vector<Point> l1 = {moveAlongX (0.1) * r0p,
            moveAlongX (0.1) * r1p,
            moveAlongX (0.1) * r2p,
            moveAlongX (0.1) * r3p};
    std::vector<Point> l2 = {moveAlongX (0.2) * r0p,
                             moveAlongX (0.2) * r1p,
                             moveAlongX (0.2) * r2p,
                             moveAlongX (0.2) * r3p};
    Environment res;
    res.mesh = Mesh {/*((1.0, 0.0, 0.0, 1.0),
        //                                              (P.Polygon ) $
        //                                              map ((\a -> rotateAroundZ a !$ (Point 1 0 0 1)) .
        //                                                   (/360.0) .
        //                                                   (*(tau::Double)) .
        //                                                   fromIntegral::Integer->Point Double) $ [0..359]),*/
                          {{1, 0, 0, 1}, {Polygon, {p0, p1, p2}}}};
    Obstacle o;
    o.type = Triangle;
    o.a = p0;
    o.b = p1;
    o.c = p2;
    o.thickness = 0.01;
    res.obstacles = Obstacles{o};
    res.sources = std::vector<Source>{{{0, (-0.0001), 0.0001, 2}, {1.0001, 0.00009999, (-0.0001)}}};
    res.receivers = std::vector<Receiver>{l1, l2};
    return res;
//        red = (1.0, 0.0, 0.0, 1)
}
Mesh levelMesh = level().mesh;
std::vector<RuntimeObstacle> obs = computeObs(level().obstacles);
std::vector<Source> source =  level().sources;
double step = 0.01;
double jump = 0.001;
double gravity = .0002;
//mutableMeshRef = unsafePerformIO $ newIORef (G.toMesh (_sources level) (_receivers level) startState)
struct {
    std::string cur;
    std::vector<std::string> history;
    int i;
} console;// = unsafePerformIO $ newIORef (Console "" [] 0)
std::vector<Receiver> receivers = level().receivers;
//-- ctrl ::
//ctrl a = keyModifierLeftCtrl (keysymModifier a) || keyModifierRightCtrl (keysymModifier a)
//commands = (T.Node ( Command "" "" (io (return ())) True) [T.Node (setGravity gravityRef) [],
//                                                                           T.Node (loadLevel obsRef levelMeshRef sourceRef receiversRef) [],
//                                                                           T.Node (loadObj obsRef levelMeshRef) [],
//                                                                           T.Node (setStep stepRef) [],
//                                                                           T.Node (setJump jumpRef) [],
//                                                                           T.Node (quit) [],
//                                                                           T.Node (toggleFrame frameRef) [],
//                                                                           T.Node (help commands) [],
//                                                                           T.Node (Console.state stateRef) []
//                                                                          ])
void processKeyboard (const Uint8* c) {
    if(c[SDL_SCANCODE_W]) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongX3(step);
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

bool continueCycle = true;
void keyboardProcess () {
    const Uint8 *state = SDL_GetKeyboardState(NULL);
    if(state[SDL_SCANCODE_ESCAPE] || (state[SDL_SCANCODE_F4] && (state[SDL_SCANCODE_LALT] || state[SDL_SCANCODE_RALT]))) {
        continueCycle = false;
    }
//    SDL_Scancode c = a.keysym.scancode;
    if(state[SDL_SCANCODE_UP]) {
//                               when (keysymKeycode a == KeycodeUp) (modifyIORef consoleRef consoleUp)

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

    processKeyboard(state);
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
G::MutableMesh mutableMesh = G::toMesh(level().sources, level().receivers, state);
void gameDisplay() {
//                            state' <- readIORef stateRef
//                            levelMesh <- readIORef levelMeshRef
//                            (rays, itemss, recvs) <- readIORef mutableMeshRef
//                            cons <- readIORef consoleRef
//                            frame <- readIORef frameRef
//                            sources <- readIORef sourceRef
//                            wheCons <- readIORef wheConsoleRef
    auto ap = state.avatarPosition;
    auto inv = state.inventory == Empty ? Mesh{} :
               state.inventory == De ? preShow(mutableMesh.rays, G::viewPort(ap))*G::transparentDeviator() : (abort(), Mesh{});
    auto items =  mutableMesh.items;
    if(state.selected.is_initialized()) {
        G::lightenABit(&items[boost::get(state.selected)]);
    }
    G::displayGame(levelMesh, items, mutableMesh.rays, mutableMesh.recvs, inv, G::viewPort( ap));
}
AvatarPosition processTurnLeft (double angle, const AvatarPosition& ap) {
    return {(ap.pos * (rotate3 (- angle))), ap.height, ap.nod, ap.speed};// -- Sudya po vsemu, skorost' nuzhno menyat' zdes' ili v processMove
}
template <typename T>
T bound(const T& n, const T& lower, const T& upper) {
  return std::max(lower, std::min(n, upper));
}
AvatarPosition processTurnUp (double angle, const AvatarPosition& ap) {
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
using namespace G;
void mouseCCase() {
    Matrix44 trans = preShow(mutableMesh.rays, (G::viewPort(state.avatarPosition)));
    state = thisFunc( trans, state);
    mutableMesh = toMesh(source, receivers, state);
}
//                -- pr = do
//                --  b <- hReady out
//                --  when(b) $ do
//                --    esc <- readIORef escape
//                --    c <- hGetChar out

//                --    putStrLn $ "pr: " ++ show esc ++ ", c: " ++ show c -- writeIORef escape ""
//                --    let news = esc ++ [c]
//                --    if isSequencePrefix (news)
//                --      then
//                --       do
//                --        putStrLn $ "in then, news = " ++ show news
//                --        case Console.sequence (news) of
//                --          Nothing -> do
//                --            putStrLn $ "in Nothing"

//                --            writeIORef escape (news)
//                --          Just seqq-> do
//                --            putStrLn $ "in Just" ++ show seqq
//                --            modifyIORef consoleRef (applyEscapeSequence seqq) >> writeIORef escape ""
//                --      else
//                --       -- error ("unknown seq: "++show news)
//                --       do
//                --         when (esc /= "") $ error "unknown esc seq"
//                --         putStrLn $ "in else"
//                --         -- forM esc $ (\e -> do
//                --           -- cc <- readIORef consoleRef
//                --           -- newCC <- (MTL.execStateT (interactive commands e) cc)
//                --           -- writeIORef consoleRef newCC) -- (runStateTinteractive commands e))
//                --         --
//                --         writeIORef escape ""
//                --         cc <- readIORef consoleRef
//                --         newCC <- (MTL.execStateT (interactive commands c) cc)
//                --         writeIORef consoleRef newCC -- modifyIORefIO consoleRef (interactive commands c)
//                --    pr     -- e <- readIORef consoleRef
//                       -- ((), newConsole) <- MTL.runStateT (interactive commands c) cons
//                       -- writeIORef consoleRef newConsole
//                     -- str ->
//gameTimerCallback = do
//                                    gravity <- readIORef gravityRef
//                                    obs <- readIORef obsRef
//                                    modifyIORef stateRef (avatarPosition %~ tick gravity obs)

void processTimer() {
    state.avatarPosition = tick(gravity, obs, state.avatarPosition);
}
//        --                            readIORef state >>= (putStrLn . show)
//        -- console obsRef meshRef stepRef jumpRef gravityRef frameRef stateRef =
//          -- (interactive commands)


//processMove move = {-modifyIORef state-} (\(AP pos height nod speed) -> AP
//                                                                                                  (pos !*! move (0.1/cosh height) )
//                                                                                                  height
//                                                                                                  nod
//                                                                                                  speed
//                                                                                            )
//matricesMoveZ = [(KeycodeZ, {-moveAlongZ-} (0.01)), (KeycodeC, {-moveAlongZ-} (-0.01))]




//--   let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
//--   let rotateDelta = fmap (\(p) -> rotate3 p) (fmap (fst) $ toGradi <$> mouseDelta)
AvatarPosition applySpeed (const AvatarPosition& ap){
    return {(ap.pos * ( moveToTangentVector3 ({ap.speed.x, ap.speed.y})) ), (ap.height+ap.speed.z), ap.nod, ap.speed};
    // это очень очень неправильно
}
AvatarPosition applyGravity (double gravity, AvatarPosition state) {
    state.speed = Vector3{ state.speed.x, state.speed.y, (state.speed.z - gravity/(cosh (state.height))/(cosh (state.height)))};
    return state;
}

//-- processEvent :: Event a -> IO ()
//-- processEvent (Move a) = modifyIORef currentMatrix $ move a
//-- processEvent ToOrigin = writeIORef currentMatrix identityIm

//--data Input = Reset | Move (Double -> M33 Double) | Down Double | Left Double | Tick






//--processEvent :: Input -> (AvatarPosition -> AvatarPosition)
//reset state = startState

//--   -- $(prettyR "upMatrix")
//--   -- $(prettyV "upAngle")
//--   -- let moveDeltaEvent = moveDelta <@ ekeyboard
//--   -- $(prettyR "upMoveDelta")
//--   -- $(prettyR "upMove")

//--   let viewPort = liftA3 (\x y z -> {-moveAlongZ (-1/4) !*!-} z !*! y !*! x) (fmap m33_to_m44M move) upMove upMatrix  -- <@ viewPortChangeStream
//--   idle <- viewPort <@ edisplay
//--   -- let dist = fmap (\x -> distance origin (x !$ origin)) (idle)-- ::Event (M44 Double)) -- <@ ekeyboard
//--   -- $(prettyV "dist")
//--   -- let currp :: Event (Point a, Point a)
//--       -- currp = fmap (\x -> ((x !$ origin), (over _v4  (*! x) origin))) (idle)-- ::Event (M44 Double)) -- <@ ekeyboard
//--   -- $(prettyV "currp")
//--   reactimate (fmap (\x -> display (mesh environment) (x)) idle)
//--   -- reactimate $ fmap (\x -> putStrLn $ "insanity:"++ show (insanity x) ++ "\n")  idle
int filter(void*      /*userdata*/,
                    SDL_Event* event) {
    return event->type == SDL_QUIT || event->type == SDL_WINDOWEVENT;
}


void gameLoop() {
//    SDL_SetHintWithPriority(SDL_HINT_MOUSE_RELATIVE_MODE_WARP, "1", SDL_HINT_OVERRIDE);
//    SDL_SetRelativeMouseMode(SDL_TRUE);
//    SDL_SetEventFilter(filter,
//                            nullptr);
    SDL_WarpMouseInWindow(window, width/2, height/2);
//    SDL_GL_SetSwapInterval(0);
//    SDL_SetWindowGrab(window, SDL_TRUE);
//    SDL
//                           SDL_FALSE);
    glEnable(GL_DEPTH);
    uint a = SDL_GetTicks();
    int cycles = 0;
    bool focus = true;
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
                state.selected = state.inventory == Empty ? findSelected(state.worldState, state.avatarPosition) : boost::none;
                SDL_WarpMouseInWindow(window, width/2, height/2);
            }
            if(res &  SDL_BUTTON(SDL_BUTTON_LEFT)) {
                    mouseCCase();
            }

    //
            keyboardProcess();
            processTimer();
            gameDisplay();
            SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
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
#endif // GAMELOOP

