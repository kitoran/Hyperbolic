#ifndef LEVEL_H
#define LEVEL_H

#include "util/physics.h"
#include "util/serialize.h"

struct Level
{
public:
    Mesh mesh;
    std::vector<Source> sources;
    std::vector<Receiver> receivers;
    Level();

};
inline void serialize(FILE* stream, const Level& level) {
    serialize(stream, level.mesh);
    serialize(stream, level.sources);
    serialize(stream, level.receivers);
}
inline  void deserialize(FILE* stream, Level* level) {
    deserialize(stream, &level->mesh);
    deserialize(stream, &level->sources);
    deserialize(stream, &level->receivers);
}
inline Level level;



#endif // LEVEL_H
