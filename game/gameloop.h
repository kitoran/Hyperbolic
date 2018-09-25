#ifndef GAMELOOP
#define GAMELOOP
#include "util/physics.h"
#include "graphics.h"
#include <time.h>
OptionalDouble intersectRay( const Deviator & d, const Matrix44 &transs) {
    Matrix44 move = moveRightTo(d.pos);
    Point dirFromStart = toNonPhysicalPoint (transposeMink (move) * d.dir);
    Matrix44 turn = andThen(getPointToOxyAroundOx,  getPointToOxzAroundOz, dirFromStart);
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
        return {true, distance(d.pos, origin)};
    } else {
        return  {false, 0};
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
        auto ma =intersectRay (dev, ttt );
             if( ma.there) {
                 listt.push_back (ma.i/*{-, ((P._devPos) dev, diir)-}*/); //--]filter (map (\e@(P.Devi poos _ _) -> ) list)
             }
    }
    if(listt.empty() ) {
       return {false, 0};
    }
    int r = 0;
    double m = listt[0];
    for(int i = 1; i < listt.size(); i++) {
        if(listt[i] < m) {
            r = i;
           m =  listt[i];
        }
    }
    return {true, r};
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
    r.worldState = { {moveAlongY( 0.1) * Deviator{ {0,
                                                           0,
                                                           0,
                                                           1},
                                                          {0,
                                                           1,
                                                           0},
                                                          0},
                                                               /* moveAlongY (-0.1::Double) !$ Devi (Point 0 0 0 1) (Abs 0 1 0) (0) */
                            },
                            {}
                          };
    r.selected  = OptionalInt{true,
                           0
                          };
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
        ap.selected = {false, 0};
        return ap;
    } else if(ap.inventory == Empty) {
        if(ap.selected.there == false) {
            return ap;
        } else {
            ap.inventory = De;
            ap.worldState.devis.erase(ap.worldState.devis.begin()+ap.selected.i);
            return ap;
        }
    }
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
    Point h1 = trans * he.a;
    Point h2 = trans * he.b;
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
    if (rays.size()<1) abort();
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
        auto move =moveRightTo (rays[index].e.a);
        auto dirFromStart = (transposeMink (move) * rays[index].e.b);
        auto turn = andThen(getPointToOxyAroundOy,  getPointToOxzAroundOz, dirFromStart);
        rayToOx = (move * transposeMink( turn));
    }
    const Point& pos = rays[index].e.a;
    const Point& dir= rays[index].e.b;
//    Point dx dy dz dt = dir
//    Point px py pz pt = pos
    Point p = (1-ratio)*normalizeKlein (vp * pos) + ratio*normalizeKlein (vp * dir);
    auto trans = /*Debug.Trace.trace ("qqqq "++show x1) $*/ ( rayToOx) * (moveAlongX (   (H::distance ( vp * pos, p))));
    Point h1 = vp * p;
    double x1 = (-h1.y)/h1.x;
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
//obsRef = unsafePerformIO $ newIORef runtimeObstacles
std::vector<Source> source =  level().sources;
double step = 0.01;
double jump = 0.01;
//gravityRef = unsafePerformIO $ newIORef 0 -- .0002
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
void processKeyboard (SDL_Scancode c) {
    if(c == SDL_SCANCODE_W) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongX3(step);
    } else if(c == SDL_SCANCODE_S) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongX3(-step);
    } else if(c == SDL_SCANCODE_A) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongY3(step);
    } else if(c == SDL_SCANCODE_D) {
        state.avatarPosition.pos = state.avatarPosition.pos * moveAlongY3(-step);
    } else if(c == SDL_SCANCODE_Z) {
        state.avatarPosition.height += step;
    } else if(c == SDL_SCANCODE_C) {
            state.avatarPosition.height -= step;
    } else if(c == SDL_SCANCODE_R) {
            state = startState();
//                    KeycodeR -> reset
    } else if(c == SDL_SCANCODE_SPACE) {
            state.avatarPosition.speed.z += jump;
    }
}


void keyboardCase (SDL_KeyboardEvent a) {
    SDL_Scancode c = a.keysym.scancode;
    if(c == SDL_SCANCODE_UP) {
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

    processKeyboard(c);
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
    if(state.selected.there) {
        G::lightenABit(&items[state.selected.i]);
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
    return {ap.pos, ap.height, bound(-tau/4, tau/4, (ap.nod + angle)), ap.speed};
                                                                                            }
AvatarPosition processMouse(int width, int height, int x, int y, const AvatarPosition &ap) {
    auto  fromGradi = [](auto x) {

        return (x / 360*tau*7.0/30.0);
    };
    return processTurnUp ( fromGradi (y - height/2),  processTurnLeft ( fromGradi ( x - width/2), ap ));
}
long last = 0;
void mouseMCase(const H::Vector2 &v) {//gamePassiveMotionCallback (V2 x y) = do

    long            now;
    timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);

    now = round(spec.tv_nsec / 1.0e6);
    last = now;
if(now - last > 300) {
        LevelState savedState = state;
        state.avatarPosition = processMouse( (G::width), (G::height), v.x, v.y, savedState.avatarPosition);
        state.selected = state.inventory == Empty ? findSelected(savedState.worldState, state.avatarPosition) : OptionalInt{false, 0};
    }
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
//applySpeed :: AvatarPosition -> AvatarPosition
//applySpeed (AP pos height nod speed@(V3 x y z)) = AP (pos !*! ( moveToTangentVector3 (V2 x y)) ) (height+z) nod speed
//applyGravity :: Double -> AvatarPosition -> AvatarPosition
//applyGravity gravity state@(AP pos height nod cspeed@(V3 x y z)) = state { _speed = V3 x y (z - gravity/(cosh height)/(cosh height))}


//-- processEvent :: Event a -> IO ()
//-- processEvent (Move a) = modifyIORef currentMatrix $ move a
//-- processEvent ToOrigin = writeIORef currentMatrix identityIm

//--data Input = Reset | Move (Double -> M33 Double) | Down Double | Left Double | Tick






//--processEvent :: Input -> (AvatarPosition -> AvatarPosition)
//reset state = startState
//-- networkDescription :: forall a. (RealFloat a, Ord a, Show a, Real a) => Environment a ->
//--                                                                         (Int, Int) ->
//--                                                                         AddHandler Char ->
//--                                                                         AddHandler (Int, Int) ->
//--                                                                         AddHandler () ->
//--                                                                         AddHandler () ->
//--                                                                         MomentIO ()
//-- networkDescription environment (width, height) addKeyboard addMouse addDisplay addTimer = do
//--   ekeyboard <- fromAddHandler $ addKeyboard
//--   emouse' <- (fromAddHandler $ addMouse  )
//--   edisplay <- (fromAddHandler $ addDisplay)
//--   etimer <- fromAddHandler addTimer
//--   let mouseDelta = fmap (\(x,y) -> ( x - (width`div`2), y - (height`div`2))) emouse'
//--   let toGradi (x,y) = (ff x, ff y) where ff i = (fromIntegral i / 360*tau)
//--   let rotateDelta = fmap (\(p) -> rotate3 p) (fmap (fst) $ toGradi <$> mouseDelta)
//--       ids = (identityIm)
//--   --let startPosMatrixM = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)--(1))
//--       resetRequest = filterE (== 'r') ekeyboard
//--       reset = fmap (const $ const identityIm) $ resetRequest
//--   -- let moveFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
//--   --     moveFunc = fmap (\x (_, b, c) -> (x, b, c)) move
//--   -- let rotateFunc::Event ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
//--   --     rotateFunc =  fmap (\x (a, _, c) -> (a, x, c)) $ rotate
//--   upAngle <- accumB 0 $ unions [(fmap (\n delta -> bound (-tau/4) (tau/4) (n+delta)) (fmap (negate.snd) $ toGradi <$> mouseDelta)), fmap (const $ const 0) $ resetRequest]
//--   let upMatrix = fmap rotateAroundY upAngle
//--   let --upMoveDelta :: Event (M44 Double)
//--       upMoveDelta = (filterJust $ fmap (flip lookup matricesMoveZ) ekeyboard)
//--   upMove <- accumB identityIm $ fmap (\x y -> x !*! y) upMoveDelta
//--   curz <- fmap (\x -> distance origin (x !$ origin)) upMove <@ ekeyboard
//--   let curzB :: Behaviour ((a -> M33 a) -> M33 a)
//--       curzB = stepper ($ 0.1) (fmap (\a -> ($ 0.1/cosh a)) curz)
//--   --let moveDelta :: Event (M44 a)
//--   -- speed :: V33 a
//--   speedE <- accumE (V3 0 0 0) $ unions [fmap (\_ (V3 x y z) -> (V3 0 0 0)) resetRequest, fmap (\_ (V3 x y z) -> V3 x y (z-0.0001)) etimer]
//--   moveDeltaInteractive <- curzB <@> (filterJust $ fmap (\k -> lookup k matricesMoveInPlane ) ekeyboard)
//--   (moveDelta::Event (M33 a)) <- unionWithP (!*!) identityIm identityIm moveDeltaInteractive never--(fmap (\(V3 _ _ z) -> (moveAlongZ (-z))) speedE)
//--   (move::Behaviour (M33 a)) <- accumB identityIm $ unions [reset, (fmap (\x y -> x !*! y) moveDelta), fmap (\x y -> x !*! y) rotateDelta]--(move::Behaviour (M44 a)) <- accumB startPosMatrix $ unions [(fmap (\x y -> (y !*! x !*! transposeMink y)) $  unionWith (!*!) moveDeltaRotated rotateDelta ), reset]
//--       -- upFunc::Behavior ((M44 a, M44 a, M44 a) -> (M44 a, M44 a, M44 a))
//--       -- upFunc =  fmap (\x (a, b, _) -> (a, b, x)) upMatrix
//--       -- viewPortChangeStream = unionWith const (fmap (const ()) ekeyboard) (fmap (const ()) emouse')


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

void gameLoop() {
    while(true) {
        SDL_Event event;
        if(SDL_PollEvent(&event)) {
            switch(event.type) {
            case SDL_KEYDOWN:{
                keyboardCase(event.key);
            }break;
            case SDL_MOUSEMOTION:{
                mouseMCase({event.motion.xrel, event.motion.yrel});
            }break;
            case SDL_MOUSEBUTTONDOWN:{
                mouseCCase();
            }break;
            case SDL_QUIT: return;
            default: {
            }
            }
        } else {
            gameDisplay();
        }
        SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
    }

}
#endif // GAMELOOP
