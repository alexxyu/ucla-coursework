//  PointToPointRouter.cpp

#include <iostream>
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
        GeoCoord gc;
        PathNode* parent;
        double totalCost;       // f
        double distCost;        // g
        double heuristicCost;   // h
    };
    
    struct PathNodeComparator
    {
        bool operator()(const PathNode* n1, const PathNode* n2) const
        {
            return n1->totalCost > n2->totalCost;
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

DeliveryResult PointToPointRouterImpl::generatePointToPointRoute(
     const GeoCoord& start,
     const GeoCoord& end,
     list<StreetSegment>& route,
     double& totalDistanceTravelled) const
{
    // check for validity of start and end coordinates
    vector<StreetSegment> temp;
    if(!m_streetMap->getSegmentsThatStartWith(start, temp))
        return BAD_COORD;
    if(!m_streetMap->getSegmentsThatStartWith(end, temp))
        return BAD_COORD;

    if(start == end) {
        route.clear();
        totalDistanceTravelled = 0;
        return DELIVERY_SUCCESS;
    }
    
    // beginning of A* pathfinding algorithm
    priority_queue<PathNode*, vector<PathNode*>, PathNodeComparator> open;
    unordered_set<PathNode, PathNodeHasher, PathNodeEqual> openList;
    unordered_set<PathNode, PathNodeHasher, PathNodeEqual> closedList;
    
    list<PathNode*> toBeDeleted;
    
    PathNode* startNode = new PathNode;
    *startNode = {start, nullptr, 0, 0, 0};
    open.push(startNode);
    openList.insert(*startNode);
    toBeDeleted.push_front(startNode);
    
    // continue searching until no more paths to check
    while(!open.empty()) {
        PathNode* curr = open.top();
        open.pop();
        openList.erase(*curr);
        closedList.insert(*curr);
        
        // path found from start to end GeoCoord
        if(curr->gc == end) {
            route.clear();
            totalDistanceTravelled = curr->distCost;
            
            // backtrack path and add StreetSegments to route
            vector<StreetSegment> segs;
            while(curr->parent != nullptr) {
                m_streetMap->getSegmentsThatStartWith(curr->parent->gc, segs);
                for(StreetSegment s: segs) {
                    if(s.end == curr->gc) {
                        route.push_front(s);
                        break;
                    }
                }
                curr = curr->parent;
            }
            
            // cleanup dynamically allocated PathNodes
            for(PathNode* n: toBeDeleted)
                delete n;
            
            return DELIVERY_SUCCESS;
        }
        
        // otherwise, process connecting nodes
        vector<StreetSegment> connectingPaths;
        m_streetMap->getSegmentsThatStartWith(curr->gc, connectingPaths);
        for(StreetSegment path: connectingPaths) {
            PathNode* child = new PathNode;
            *child = {path.end, curr, 0, 0, 0};
            toBeDeleted.push_front(child);
            
            // skip node if already in closed list
            if(closedList.find(*child) != closedList.end())
                continue;
            
            // update costs
            child->distCost = curr->distCost + distanceEarthMiles(curr->gc, child->gc);
            child->heuristicCost = distanceEarthMiles(child->gc, end);
            child->totalCost = child->distCost + child->heuristicCost;
        
            // skip node if it has higher cost than another path
            auto openListChild = openList.find(*child);
            if(openListChild != openList.end() && child->distCost >= (*openListChild).distCost)
                continue;
        
            open.push(child);
            openList.insert(*child);
        }
    }
    
    // no possible route found
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
