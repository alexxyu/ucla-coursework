#include "provided.h"
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
using namespace std;

class DeliveryOptimizerImpl
{
public:
    DeliveryOptimizerImpl(const StreetMap* sm);
    ~DeliveryOptimizerImpl();
    void optimizeDeliveryOrder(
        const GeoCoord& depot,
        vector<DeliveryRequest>& deliveries,
        double& oldCrowDistance,
        double& newCrowDistance) const;
    
private:
    const int MAX_ITERS = 10000;
    const StreetMap* m_streetMap;
    
    double calculateDistanceForDeliveries(
        const GeoCoord& depot,
        vector<DeliveryRequest>& deliveries) const;
    void perturb(vector<DeliveryRequest>& deliveries) const;
};

DeliveryOptimizerImpl::DeliveryOptimizerImpl(const StreetMap* sm)
{
    m_streetMap = sm;
}

DeliveryOptimizerImpl::~DeliveryOptimizerImpl()
{
}

void DeliveryOptimizerImpl::optimizeDeliveryOrder(
    const GeoCoord& depot,
    vector<DeliveryRequest>& deliveries,
    double& oldCrowDistance,
    double& newCrowDistance) const
{
    if(deliveries.size() <= 0)
        return;
    
    oldCrowDistance = calculateDistanceForDeliveries(depot, deliveries);
    newCrowDistance = oldCrowDistance;
    
    int iters = min(MAX_ITERS, static_cast<int>(pow(deliveries.size(), 3)));
    vector<DeliveryRequest> test = deliveries;
    auto rng = default_random_engine {};
    shuffle(begin(test), end(test), rng);
    
    // try different random combinations and look for best route
    for(int i=0; i<iters; i++) {
        double testDistance = calculateDistanceForDeliveries(depot, test);
        if(testDistance < newCrowDistance) {
            newCrowDistance = testDistance;
            deliveries = test;
        }
        
        perturb(test);
    }
}

double DeliveryOptimizerImpl::calculateDistanceForDeliveries(
    const GeoCoord& depot,
    vector<DeliveryRequest>& deliveries) const
{
    // sum up distances between waypoints
    double totalDistance = 0;
    for(int i=0; i<deliveries.size(); i++) {
        if(i==0)
            totalDistance += distanceEarthMiles(depot, deliveries[i].location);
        else
            totalDistance += distanceEarthMiles(deliveries[i-1].location, deliveries[i].location);
    }
    totalDistance += distanceEarthMiles(deliveries[deliveries.size()-1].location, depot);
    
    return totalDistance;
}

void DeliveryOptimizerImpl::perturb(vector<DeliveryRequest>& deliveries) const
{
    random_device rd;
    default_random_engine generator(rd());
    uniform_int_distribution<> distro(0, static_cast<int>(deliveries.size()-1));
    
    int randIndex1 = distro(generator);
    int randIndex2;
    do {
        randIndex2 = distro(generator);
    } while(randIndex1 != randIndex2);
    
    swap(deliveries[randIndex1], deliveries[randIndex2]);
}

//******************** DeliveryOptimizer functions ****************************

// These functions simply delegate to DeliveryOptimizerImpl's functions.
// You probably don't want to change any of this code.

DeliveryOptimizer::DeliveryOptimizer(const StreetMap* sm)
{
    m_impl = new DeliveryOptimizerImpl(sm);
}

DeliveryOptimizer::~DeliveryOptimizer()
{
    delete m_impl;
}

void DeliveryOptimizer::optimizeDeliveryOrder(
        const GeoCoord& depot,
        vector<DeliveryRequest>& deliveries,
        double& oldCrowDistance,
        double& newCrowDistance) const
{
    return m_impl->optimizeDeliveryOrder(depot, deliveries, oldCrowDistance, newCrowDistance);
}
