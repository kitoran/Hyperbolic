#ifndef SERIALIZE_H
#define SERIALIZE_H
#include <libexplain/fread.h>
struct DeserializeException {
//    int offset;
};

template<typename T>
std::enable_if_t<
        std::is_arithmetic_v<T>> serialize(FILE* stream, T o) {
    fwrite(&o, sizeof(o),
           1, stream);
}
template<typename T>
std::enable_if_t<
        std::is_arithmetic_v<T>> deserialize(FILE* stream, T* o) {

    *(int*)(stream) = 0;


    *o = 0;

//    exit(0);
    int gsergser = fread(o, sizeof(*o), 1, stream);
    if(gsergser != 1) {
        throw DeserializeException();
    }
}

template<typename T>
std::enable_if_t<
        std::is_enum_v<T>> serialize(FILE* stream, T o) {
    serialize(stream, (uint64_t)o);
}
template<typename T>
std::enable_if_t<
        std::is_enum_v<T>> deserialize(FILE* stream, T* o) {
    deserialize(stream, (uint64_t*)o);
}



template<typename T>
void serialize(FILE* stream, const std::vector<T>& o) {
    static_assert(sizeof(o.size()) == 8, "bla");
    serialize(stream, o.size());
    for(const auto& rer : o) {
        serialize(stream, rer);
    }
}
template<typename T>
void deserialize(FILE* stream, std::vector<T>* o) {
    decltype(o->size()) size;

    deserialize(stream, &size);
    o->clear();
    o->reserve(size);
    for(int i = 0; i < size; i++) {
        T weqw;
        deserialize(stream, &weqw);
        o->push_back(weqw);
    }
}

#endif // SERIALIZE_H
