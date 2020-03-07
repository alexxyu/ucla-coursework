//  PointToPointRouter.cpp

#include <list>
#include "provided.h"

class StreetMap;
struct GeoCoord;
struct StreetSegment;
enum DELIVERY_RESULT { success, no_route, bad_coord };

class PointToPointRouterImpl
{
public:
    PointToPointRouterImpl(const StreetMap* sm);
    ~PointToPointRouterImpl();
    DELIVERY_RESULT generatePointToPointRoute(const GeoCoord& start, const GeoCoord& end,
                                              std::list<StreetSegment>& route,
                                              double& totalDistanceTravelled) const;
    
private:
    const StreetMap* m_streetMap;
};

PointToPointRouterImpl::PointToPointRouterImpl(const StreetMap* sm)
{
    m_streetMap = sm;
}

PointToPointRouterImpl::~PointToPointRouterImpl()
{
    
}

DELIVERY_RESULT PointToPointRouterImpl::generatePointToPointRoute(const GeoCoord& start,
                                                                  const GeoCoord& end,
                                                                  std::list<StreetSegment>& route,
                                                                  double& totalDistanceTravelled) const
{
    DELIVERY_RESULT returnVal;
    if(start == end) {
        route.clear();
        totalDistanceTravelled = 0;
    }
        
    
    return returnVal;
}
