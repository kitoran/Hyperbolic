#ifndef LEVEL_H
#define LEVEL_H

// include headers that implement a archive in simple text format
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/vector.hpp>
#include "util/physics.h"

struct Level
{
private:
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /*version*/)
    {
//        std::vector<int> fewfwe{10,2,3,4};
//        ar & fewfwe;
        ar & mesh;
        ar & sources;
        ar & receivers;
    }
public:
    Mesh mesh;
    std::vector<Source> sources;
    std::vector<Receiver> receivers;
    Level();
};
inline Level level;
#endif // LEVEL_H
