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
inline TTF_Font* mono = 0;

void renderLine(const std::string & line, int lineNumber, bool bottom = true);

void renderConsole();
void predisplay( );
void postdisplay( );

void display(const std::vector<Mesh> &mesh, const Matrix44 &tran, bool AlternativeBuffer = false);
Matrix44 viewPort(const AvatarPosition& ap);
bool containsZero (const std::vector<Vector2>& v);
struct MutableMesh {
    Mesh rays;
    std::vector<Mesh> items;
    Mesh recvs;
    std::vector<Mesh> cubes;
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
boost::optional<FunctionDeResult> functionDe(Point pos, Absolute dir, Deviator de);
boost::optional<Point> functionRcv(Point pos, Absolute dir, Receiver re);
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
                const H::Point& pos, const H::Absolute& dir) -> FoldMaybesResult;

void push_front(const Point& p, std::vector<Point>*v);
UnfoldRayResult unfoldRay(const std::vector<Deviator>& listd, const std::vector<Receiver>& listr,
                          const Point& pos, const Absolute & dir);
//unfoldRay listd listr pos dir = first (pos:) $ go pos dir -- (pos:map fst (go pos dir ), last $ dir : map snd (go pos dir ))
//  where go :: H.Point Double -> H.Absolute Double ->  ([(H.Point Double)], Either (H.Absolute Double)  Int)
//        go pos dir  = case foldMaybes listd listr pos dir of
//                           Infinity -> ([], Left dir)
//                           Receiver (p, i) -> ([p], Right i)
//                           Deviator (p, newd) -> case go p newd of (ps, re) -> (p:ps, re)  -- (deleteNth i list)
Matrix44 transformationForDeviator(const Deviator& de);
//thatTransformation (P.Devi pos dir d) = let move = moveRightTo pos -- если сделать, чтобы одна функция возвращала moveRightTo и moveRightFrom, то меньше вычислений
//                                            dirFromStart = (toNonPhysicalPoint $ transposeMink move !$ dir)
//                                            turn = (H.getPointToOxyAroundOy `andThen`  getPointToOxzAroundOz) dirFromStart
//                                         in (move !*!  transposeMink turn !*! (rotateAroundX d))

MutableMesh toMesh (const std::vector<Source> &s,  const std::vector<Receiver> &r, const LevelState& ls);

}
//    -- inv = case mi of
//    --     Nothing -> mempty
//    --     Just P.De -> transposeMink (viewPort (P.AP pos height nod 0))!*! H.moveAlongX 0.011 !*! H.moveAlongZ (-0.012) !$ deviator

#endif // GRAPHICS

