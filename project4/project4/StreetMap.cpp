// StreetMap.cpp

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <list>
#include "ExpandableHashMap.h"

struct GeoCoord;
struct StreetSegment;

class StreetMap
{
public:
    StreetMap();
    ~StreetMap();
    bool load(std::string mapFile);
    bool getSegmentsThatStartWith(const GeoCoord& gc,
                                  std::vector<StreetSegment>& segs) const;
    
private:
    ExpandableHashMap<GeoCoord, std::list<StreetSegment*>> m_streetMap;
};

StreetMap::StreetMap()
{
    
}

StreetMap::~StreetMap()
{
    
}

bool StreetMap::load(std::string mapFile)
{
    std::ifstream infile(mapFile);
    if(!infile) {
        std::cerr << "Data file does not exist!" << std::endl;
        return false;
    }
    
    std::string line;
    while(std::getline(infile, line)) {
        std::string streetName = line;
        streetName.pop_back();
        
        // process number of segments
        int numSegments;
        infile >> numSegments;
        infile.ignore(10000, '\n');
        std::cout << "Processing data for " << streetName << "." << std::endl;
        
        for(int i=1; i<=numSegments; i++) {
            
            getline(infile, line);
            std::istringstream iss(line);
            double startLatitude, startLongitude, endLatitude, endLongitude;
            if( !(iss >> startLatitude >> startLongitude >> endLatitude >> endLongitude) ) {
                std::cerr  << "Data file not formatted appropriately." << std::endl;
                return false;
            }
            
            // add street segment to map
            
        }
    }
    
    return true;
}

bool StreetMap::getSegmentsThatStartWith(const GeoCoord& gc, std::vector<StreetSegment>& segs) const
{
    const std::list<StreetSegment*>* segments = m_streetMap.find(gc);
    if(segments != nullptr && segments->size() > 0) {
        segs.clear();
        for(StreetSegment* seg: *segments)
            segs.push_back(*seg);
        
        return true;
    }
    
    return false;
}

int main()
{
    StreetMap map;
    assert(map.load("/Users/alexyu/Desktop/Projects/cs32/project4/project4/mapdata.txt"));
}
