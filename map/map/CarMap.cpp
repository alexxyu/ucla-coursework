// CarMap.cpp

#include <iostream>
#include "CarMap.h"

CarMap::CarMap()
{
    
}

bool CarMap::addCar(std::string license)
{
    return m_map.insert(license, 0);
}

double CarMap::miles(std::string license) const
{
    ValueType miles;
    bool hasLicense = m_map.get(license, miles);
    
    if(!hasLicense)
        return -1;
    
    return miles;
}

bool CarMap::drive(std::string license, double distance)
{
    if(distance < 0) return false;
    return m_map.update(license, distance);
}

int CarMap::fleetSize() const
{
    return m_map.size();
}

void CarMap::print() const
{
    int fleetSize = m_map.size();
    for(int i=0; i<fleetSize; i++) {
        std::string license;
        double distance;
        m_map.get(i, license, distance);
        
        std::cout << license << " " << distance << std::endl;
    }
}
