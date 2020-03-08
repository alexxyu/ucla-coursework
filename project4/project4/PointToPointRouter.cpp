//  PointToPointRouter.cpp

#include <list>
#include <queue>
#include <unordered_set>
#include "provided.h"
using namespace std;

class PointToPointRouterImpl
{
public:
    PointToPointRouterImpl(const StreetMap* sm);
    ~PointToPointRouterImpl();
    DeliveryResult generatePointToPointRoute(
        const GeoCoord& start,
        const GeoCoord& end,
        list<StreetSegment>& route,
        double& totalDistanceTravelled) const;
    
private:
    const StreetMap* m_streetMap;
    
    struct PathNode
    {
        double totalCost;       // f
        double distCost;        // g
        double heuristicCost;   // h
        GeoCoord gc;
        GeoCoord parent;
    };
    
    struct PathNodeComparator
    {
        bool operator()(const PathNode& n1, const PathNode& n2) const
        {
            return n1.totalCost > n2.totalCost;
        }
    };
    
    struct PathNodeHasher
    {
        unsigned int operator()(const PathNode& n) const
        {
            unsigned int hasher(const GeoCoord& g);
            return hasher(n.gc);
        }
    };
    
    struct PathNodeEqual
    {
        bool operator()(const PathNode& n1, const PathNode& n2) const
        {
            return n1.gc == n2.gc;
        }
    };
    
};

PointToPointRouterImpl::PointToPointRouterImpl(const StreetMap* sm)
{
    m_streetMap = sm;
}

PointToPointRouterImpl::~PointToPointRouterImpl()
{
    
}

DeliveryResult PointToPointRouterImpl::generatePointToPointRoute(const GeoCoord& start,
                                                                 const GeoCoord& end,
                                                                 list<StreetSegment>& route,
                                                                 double& totalDistanceTravelled) const
{
    // check for validity of start and end coordinates
    vector<StreetSegment> temp;
    m_streetMap->getSegmentsThatStartWith(start, temp);
    if(temp.size() == 0)
        return BAD_COORD;
    m_streetMap->getSegmentsThatStartWith(end, temp);
    if(temp.size() == 0)
        return BAD_COORD;

    if(start == end) {
        route.clear();
        totalDistanceTravelled = 0;
    }
    
    // beginning of A* pathfinding algorithm
    priority_queue<PathNode, vector<PathNode>, PathNodeComparator> open;
    unordered_set<PathNode, PathNodeHasher, PathNodeEqual> openList;
    unordered_set<PathNode, PathNodeHasher, PathNodeEqual> closedList;
    
    GeoCoord rootCoord("-1", "-1");
    PathNode startNode = {0, 0, 0, start, rootCoord};
    open.push(startNode);
    openList.insert(startNode);
    
    while(!open.empty()) {
        
        PathNode curr = open.top();
        open.pop();
        openList.erase(curr);
        closedList.insert(curr);
        
        // end of path found, backtrack and return
        if(curr.gc == end) {
        
            route.clear();
            totalDistanceTravelled = curr.distCost;
            
            return DELIVERY_SUCCESS;
        }
        
        // otherwise, process connecting nodes
        vector<StreetSegment> connectingPaths;
        m_streetMap->getSegmentsThatStartWith(curr.gc, connectingPaths);
        for(StreetSegment path: connectingPaths) {
            PathNode child = {0, 0, 0, path.end, curr.gc};
            
            // skip node if already in closed list
            if(closedList.find(child) != closedList.end())
                continue;
            
            child.distCost = curr.distCost + distanceEarthMiles(curr.gc, child.gc);
            child.heuristicCost = distanceEarthMiles(child.gc, end);
            child.totalCost = child.distCost + child.heuristicCost;
        
            // skip node if it has higher cost that another path
            auto openListChild = openList.find(child);
            if(openListChild != openList.end() && child.distCost > (*openListChild).distCost)
                continue;
        
            open.push(child);
            openList.insert(child);
        }
        
    }
    
    return NO_ROUTE;
}

//******************** PointToPointRouter functions ***************************

// These functions simply delegate to PointToPointRouterImpl's functions.
// You probably don't want to change any of this code.

PointToPointRouter::PointToPointRouter(const StreetMap* sm)
{
    m_impl = new PointToPointRouterImpl(sm);
}

PointToPointRouter::~PointToPointRouter()
{
    delete m_impl;
}

DeliveryResult PointToPointRouter::generatePointToPointRoute(
        const GeoCoord& start,
        const GeoCoord& end,
        list<StreetSegment>& route,
        double& totalDistanceTravelled) const
{
    return m_impl->generatePointToPointRoute(start, end, route, totalDistanceTravelled);
}

int main()
{
    StreetMap* map = new StreetMap();
    assert(map->load("/Users/alexyu/Desktop/Projects/cs32/project4/project4/mapdata.txt"));
    PointToPointRouterImpl ppr(map);
    
    GeoCoord start("34.0625329", "-118.4470263");
    GeoCoord end("34.0712323", "-118.4505969");
    
    list<StreetSegment> path;
    double distance;
    ppr.generatePointToPointRoute(start, end, path, distance);
    
    cout << "Distance traveled: " << distance << endl;
    
    delete map;
}
