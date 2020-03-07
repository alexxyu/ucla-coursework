// StreetMap.cpp

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <list>
#include <functional>
#include "ExpandableHashMap.h"
#include "provided.h"
using namespace std;

unsigned int hash(const GeoCoord& g)
{
    return hash<string>()(g.latitudeText + g.longitudeText);
}

class StreetMapImpl
{
public:
    StreetMapImpl();
    ~StreetMapImpl();
    bool load(string mapFile);
    bool getSegmentsThatStartWith(const GeoCoord& gc,
                                  vector<StreetSegment>& segs) const;
    
private:
    ExpandableHashMap<GeoCoord, list<StreetSegment*>> m_StreetMapImpl;
    
    void loadSegment(string streetName, GeoCoord start, GeoCoord end);
    
};

StreetMapImpl::StreetMapImpl()
{
    
}

StreetMapImpl::~StreetMapImpl()
{
    
}

bool StreetMapImpl::load(string mapFile)
{
    ifstream infile(mapFile);
    if(!infile) {
        cerr << "Data file does not exist!" << endl;
        return false;
    }
    
    string line;
    while(getline(infile, line)) {
        string streetName = line;
        streetName.pop_back();
        
        // process number of segments
        int numSegments;
        infile >> numSegments;
        infile.ignore(10000, '\n');
        cout << "Processing data for " << streetName << "." << endl;
        
        for(int i=1; i<=numSegments; i++) {
            
            // parse street segment information from line
            getline(infile, line);
            istringstream iss(line);
            string startLatitude, startLongitude, endLatitude, endLongitude;
            if( !(iss >> startLatitude >> startLongitude >> endLatitude >> endLongitude) ) {
                cerr  << "Data file not formatted appropriately." << endl;
                return false;
            }
            
            GeoCoord start(startLatitude, startLongitude);
            GeoCoord end(endLatitude, endLongitude);
            
            loadSegment(streetName, start, end);
            loadSegment(streetName, end, start);
        }
    }
    
    return true;
}

void StreetMapImpl::loadSegment(string streetName, GeoCoord start, GeoCoord end)
{
    StreetSegment* seg = new StreetSegment(start, end, streetName);
    
    list<StreetSegment*>* segmentList = m_StreetMapImpl.find(start);
    if(segmentList != nullptr) {
        segmentList->push_back(seg);
    } else {
        list<StreetSegment*> newSegmentList;
        newSegmentList.push_back(seg);
        m_StreetMapImpl.associate(start, newSegmentList);
    }
}

bool StreetMapImpl::getSegmentsThatStartWith(const GeoCoord& gc, vector<StreetSegment>& segs) const
{
    const list<StreetSegment*>* segments = m_StreetMapImpl.find(gc);
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
    StreetMapImpl map;
    assert(map.load("/Users/alexyu/Desktop/Projects/cs32/project4/project4/mapdata.txt"));
}

//******************** StreetMap functions ************************************

// These functions simply delegate to StreetMapImpl's functions.
// You probably don't want to change any of this code.

StreetMap::StreetMap()
{
    m_impl = new StreetMapImpl;
}

StreetMap::~StreetMap()
{
    delete m_impl;
}

bool StreetMap::load(string mapFile)
{
    return m_impl->load(mapFile);
}

bool StreetMap::getSegmentsThatStartWith(const GeoCoord& gc, vector<StreetSegment>& segs) const
{
   return m_impl->getSegmentsThatStartWith(gc, segs);
}
