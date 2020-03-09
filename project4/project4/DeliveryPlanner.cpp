#include "provided.h"
#include <vector>
using namespace std;

class DeliveryPlannerImpl
{
public:
    DeliveryPlannerImpl(const StreetMap* sm);
    ~DeliveryPlannerImpl();
    DeliveryResult generateDeliveryPlan(
        const GeoCoord& depot,
        const vector<DeliveryRequest>& deliveries,
        vector<DeliveryCommand>& commands,
        double& totalDistanceTravelled) const;
        
private:
    PointToPointRouter router;
    DeliveryOptimizer optimizer;
    
    string getDirection(double angle) const;
    bool getTurn(double angle, string& turn) const;
};

DeliveryPlannerImpl::DeliveryPlannerImpl(const StreetMap* sm) : router(sm), optimizer(sm)
{
}

DeliveryPlannerImpl::~DeliveryPlannerImpl()
{
}

DeliveryResult DeliveryPlannerImpl::generateDeliveryPlan(
    const GeoCoord& depot,
    const vector<DeliveryRequest>& deliveries,
    vector<DeliveryCommand>& commands,
    double& totalDistanceTravelled) const
{
    if(deliveries.size() < 1)
        return DELIVERY_SUCCESS;
    
    // optimize delivery order
    double oldDist, newDist;
    vector<DeliveryRequest> optimizedDeliveries = deliveries;
    optimizer.optimizeDeliveryOrder(depot, optimizedDeliveries, oldDist, newDist);

    // generate path for deliveries
    list<StreetSegment> route;
    totalDistanceTravelled = 0;
    
    for(int i=0; i<optimizedDeliveries.size()+1; i++) {
        double distanceTravelled = 0;
        DeliveryResult res;
        list<StreetSegment> currRoute;
        
        if(i == 0) {
            DeliveryRequest req = optimizedDeliveries[i];
            res = router.generatePointToPointRoute(depot, req.location, currRoute, distanceTravelled);
        } else if(i < optimizedDeliveries.size()) {
            DeliveryRequest req = optimizedDeliveries[i];
            res = router.generatePointToPointRoute(optimizedDeliveries[i-1].location, req.location, currRoute, distanceTravelled);
        } else {
            res = router.generatePointToPointRoute(optimizedDeliveries[i-1].location, depot, currRoute, distanceTravelled);
        }
        
        route.splice(route.end(), currRoute);
        totalDistanceTravelled += distanceTravelled;
        if(res != DELIVERY_SUCCESS)
            return res;
    }
    
    // create delivery commands based on path
    commands.clear();
    bool startProceed = true;
    int currRequestNo = 0;
    for(auto iter = route.begin(); iter != route.end(); iter++) {
        StreetSegment segment = *iter;
        
        // should begin new proceed command
        if(startProceed) {
            
            DeliveryCommand first;
            string dir = getDirection(angleOfLine(segment));
            first.initAsProceedCommand(dir, segment.name,
                                       distanceEarthMiles(segment.start, segment.end));
            commands.push_back(first);
            startProceed = false;
            
        } else {
            StreetSegment previousSegment = *(--iter);
            iter++;
            if(segment.name != previousSegment.name) {
                // turn onto new street
                string turn;
                double angle = angleBetween2Lines(previousSegment, segment);
                
                if(getTurn(angle, turn)) {
                    DeliveryCommand turnCommand;
                    turnCommand.initAsTurnCommand(turn, segment.name);
                    commands.push_back(turnCommand);
                }
                
                // proceed on new street
                DeliveryCommand proceedCommand;
                string dir = getDirection(angleOfLine(segment));
                proceedCommand.initAsProceedCommand(dir, segment.name, distanceEarthMiles(segment.start, segment.end));
                commands.push_back(proceedCommand);
                
            } else {
                // otherwise, proceed on current street
                DeliveryCommand* prev = &commands[commands.size()-1];
                prev->increaseDistance(distanceEarthMiles(segment.start, segment.end));
            }
        }
        
        if(currRequestNo < optimizedDeliveries.size() &&
           segment.end == optimizedDeliveries[currRequestNo].location) {
            // deliver the item
            DeliveryCommand command;
            command.initAsDeliverCommand(optimizedDeliveries[currRequestNo].item);
            commands.push_back(command);
            startProceed = true;
            currRequestNo++;
        }
    }
    
    return DELIVERY_SUCCESS;
}

string DeliveryPlannerImpl::getDirection(double angle) const
{
    if(angle >= 22.5 && angle < 67.5)
        return "northeast";
    if(angle >= 67.5 && angle < 112.5)
        return "north";
    if(angle >= 122.5 && angle < 157.5)
        return "northwest";
    if(angle >= 157.5 && angle < 202.5)
        return "west";
    if(angle >= 202.5 && angle < 247.5)
        return "southwest";
    if(angle >= 247.5 && angle < 292.5)
        return "south";
    if(angle >= 292.5 && angle < 337.5)
        return "southeast";
    return "east";
}

bool DeliveryPlannerImpl::getTurn(double angle, string& turn) const
{
    // should not turn because there is no real turn from first street
    if(angle < 1 || angle > 359)
        return false;
    
    if(angle >= 1 && angle < 180)
        turn = "left";
    else
        turn = "right";
    return true;
}

//******************** DeliveryPlanner functions ******************************

// These functions simply delegate to DeliveryPlannerImpl's functions.
// You probably don't want to change any of this code.

DeliveryPlanner::DeliveryPlanner(const StreetMap* sm)
{
    m_impl = new DeliveryPlannerImpl(sm);
}

DeliveryPlanner::~DeliveryPlanner()
{
    delete m_impl;
}

DeliveryResult DeliveryPlanner::generateDeliveryPlan(
    const GeoCoord& depot,
    const vector<DeliveryRequest>& deliveries,
    vector<DeliveryCommand>& commands,
    double& totalDistanceTravelled) const
{
    return m_impl->generateDeliveryPlan(depot, deliveries, commands, totalDistanceTravelled);
}
