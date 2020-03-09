// StreetMap.cpp

#include <vector>
#include <list>
#include <functional>
#include <iostream>
#include <fstream>
#include <sstream>
#include "provided.h"
#include "ExpandableHashMap.h"
using namespace std;

unsigned int hasher(const GeoCoord& g)
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
    list<StreetSegment*> allSegments;
    
    void loadSegment(string streetName, GeoCoord start, GeoCoord end);
};

StreetMapImpl::StreetMapImpl()
{
    
}

StreetMapImpl::~StreetMapImpl()
{
    for(StreetSegment* seg: allSegments)
        delete seg;
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
        // process street name
        string streetName = line;
        streetName.pop_back();
        
        // process number of street segments
        int numSegments;
        infile >> numSegments;
        infile.ignore(10000, '\n');
        
        for(int i=1; i<=numSegments; i++) {
            
            // parse street segment information from line
            getline(infile, line);
            istringstream iss(line);
            string startLatitude, startLongitude, endLatitude, endLongitude;
            if( !(iss >> startLatitude >> startLongitude >> endLatitude >> endLongitude) ) {
                cerr  << "Data file not formatted appropriately." << endl;
                return false;
            }
            
            // load forward and reverse street segments
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
    allSegments.push_front(seg);
    
    list<StreetSegment*>* segmentList = m_StreetMapImpl.find(start);
    if(segmentList != nullptr) {
        // add street segment to existing entry
        segmentList->push_back(seg);
    } else {
        // add street segment to new entry
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
