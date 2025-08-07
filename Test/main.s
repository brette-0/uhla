void MyFunc(int foo, int bar, int ash) {
    ash, bar, foo
}

#define v2t3(xy, z) xy.x, xy.y, z

struct vector2 {
    int x, y
}

vector2 coords
int health

MyFunc(v2t3(coords, health))
MyFunc(coords.x, coords.y, health)