#include "level.h"

Environment levelTriangle() {
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
                          {{1, 0.5, 0.5, 1}, {Polygon, {p0, p1, p2}}}};
    Obstacle o;
    o.type = Triangle;
    o.a = p0;
    o.b = p1;
    o.c = p2;
    o.thickness = 0.01;
    res.obstacles = Obstacles{o};
    res.sources = std::vector<Source>{{{-0.01, (-0.1), -0.0001, 2}, {1.0001, 0.7999, (-0.0001)}}};
    res.receivers = std::vector<Receiver>{l1, l2};
    return res;
//        red = (1.0, 0.0, 0.0, 1)
}
Environment levelPentagon() {
    auto X = moveAlongX (acosh((sqrt(5)+1)/2));
    auto Y = moveAlongY (acosh((sqrt(5)+1)/2));
    auto Z = moveAlongZ (acosh((sqrt(5)+1)/2));
    Point p0 = H::origin;
    Point p1 = X * p0;
    Point p2 = X * Y * p0;
    Point p3 = Y * X * p0;
    Point p4 = Y * p0;
    Environment res;
    res.mesh = Mesh {/*((1.0, 0.0, 0.0, 1.0),
        //                                              (P.Polygon ) $
        //                                              map ((\a -> rotateAroundZ a !$ (Point 1 0 0 1)) .
        //                                                   (/360.0) .
        //                                                   (*(tau::Double)) .
        //                                                   fromIntegral::Integer->Point Double) $ [0..359]),*/
                          {{1, 0, 0, 1}, {Polygon, {p0, p1, p2, p3, p4}}}};
    Obstacle o;
    o.type = Triangle;
    o.a = p0;
    o.b = p1;
    o.c = p2;
    Obstacle o1;
    o1.type = Triangle;
    o1.a = p0;
    o1.b = p2;
    o1.c = p3;
    Obstacle o2;
    o2.type = Triangle;
    o2.a = p0;
    o2.b = p3;
    o2.c = p4;
    o.thickness = o1.thickness = o2.thickness = 0.01;
    res.obstacles = Obstacles{o, o1, o2};


    Environment dod;
    Environment floor = res;
    floor.mesh.front().color = Color{1,1,1,1};
    Environment xrotater = rotateAroundX(M_PI_2) * res;
    xrotater.mesh.front().color = Color{0,0,1,1};

    dod += floor;
    dod += xrotater;
    dod += rotateAroundY(-M_PI_2) * res;

    dod += Y * rotateAroundX(M_PI_2) * res;
    dod += X * rotateAroundY(-M_PI_2) * res;
    dod += Z * res;
    dod += moveFromTo(p1, p2, acosh((sqrt(5)+1)/2)) * rotateAroundX(M_PI_2) * res;
    dod += moveFromTo(p4, p3, acosh((sqrt(5)+1)/2)) * rotateAroundY(-M_PI_2) * res;
    dod += X * Z * rotateAroundY(M_PI) * res;
    dod += Y * Z * rotateAroundX(M_PI) *res;
    Environment one =  X * Y * Z * rotateAroundX(M_PI) *res;
    Environment other = X * Y * Z * res;
    one.mesh.front().color = Color{0,1,0,1};
    other.mesh.front().color = Color{1,1,0,1};
    dod += one;
    dod += other;
    return moveAlongX(-0.2)*moveAlongY(-0.2)*dod;
//        red = (1.0, 0.0, 0.0, 1)
}

Obstacle meshTriangleToObstacle(HyperEntity h) {
    Obstacle o;
    o.type = Triangle;
    o.a = h.p[0];
    o.b = h.p[1];
    o.c = h.p[2];
    o.thickness = 0.01;
    return o;
}

Environment levelCorridor() {
    Environment res;
    Point lastFar = movePerpendicularlyToOxy(-0.11, Point{sinh(0.842482+0.9), 0,0,cosh(0.842482+0.9)});
    Point lastNear = movePerpendicularlyToOxy(-0.11, Point{sinh(0.842482-0.20), 0,0,cosh(0.842482-0.2)});
    auto addTriangle = [&res](HyperEntity h) {
        res.mesh.push_back({white, h});
        res.obstacles.push_back(meshTriangleToObstacle(h));
    };
    auto up = [&res](Point p) {
        Matrix44 rt = moveRightTo(p);
        return rt * Point{0, 0, sinh(0.5), cosh(0.5)};
    };
    for(int i = 1; i <= 5; i++) {
        Point far = rotateAroundZ((tau/5)) * lastFar;
        Point near = rotateAroundZ((tau/5)) * lastNear;
        addTriangle({Polygon, {far, near, lastFar}});
        addTriangle({Polygon, {lastNear, near, lastFar}});
        fprintf(stderr, "%lf %lf %lf %lf\n", far.x, far.y, far.z, far.t);
        addTriangle({Polygon, {near, lastNear, up(near)}});
        addTriangle({Polygon, {up(lastNear), lastNear, up(near)}});

        addTriangle({Polygon, {far, lastFar, up(far)}});
        addTriangle({Polygon, {up(lastFar), lastFar, up(far)}});

        lastFar = far;
        lastNear = near;
    }
    fprintf(stderr, "%ld", res.mesh.size());
    res = inv44(moveRightTo(Point{0.1417,
                             0.627,
                             0,
                             1}))*res;
    res.deviators.push_back(
    Deviator{{0.20772526696369414, 0.2974917510234292, 0.0, 1.0694858627394699},
             {1,0,0},
                0});
    res.sources.push_back(
    Source{{0.3502363157176379, 0.07359641546437636, 0.0, 1.0678158976874874},
            {0.6736414716207426, -0.9418777769235139, 0.0}});


    return res;
    //        red = (1.0, 0.0, 0.0, 1)
}

Environment level() {
    return levelCorridor();
}

LevelState startState() {
    LevelState r;
    AvatarPosition ap;
    ap.pos = identity33;
    fmt::print(stderr, "startStateDir: {}", ap.pos * Vector3{1,0,1});
    ap.height = 0.1;
    ap.nod = 0;
    ap.speed = Vector3{ 0.0, 0, 0};
    r.avatarPosition = ap;
    r.inventory = Empty;
    r.worldState = { level().deviators,
                     {}
                   };
    r.selected  = boost::none;
    return r;
}
