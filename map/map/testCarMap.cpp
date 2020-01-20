// testCarMap.cpp

#include "Map.h";
#include "CarMap.h";
#include <iostream>
#include <cassert>
using namespace std;

int main()
{
    CarMap map;
    assert(map.fleetSize() == 0);
    map.addCar("AB12CD3");
    assert(map.fleetSize() == 1 && map.miles("AB12CD3") == 0);
    assert(!map.addCar("AB12CD3"));
    assert(!map.drive("GF34LC7", 18));
    assert(map.drive("AB12CD3", 14) && map.miles("AB12CD3") == 14);
    assert(map.drive("AB12CD3", 10) && map.miles("AB12CD3") == 24);
    assert(!map.drive("AB12CD3", -5) && map.miles("AB12CD3") == 24);
    map.addCar("GF34LC7");
    map.addCar("LS78JD0");
    map.print();
    
    cout << "Passed all tests." << endl;
}
