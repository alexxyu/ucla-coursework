#ifndef STUDENTWORLD_H_
#define STUDENTWORLD_H_

#include "GameWorld.h"
#include <string>
#include <list>

class Socrates;
class Actor;

class StudentWorld : public GameWorld
{
public:
    StudentWorld(std::string assetPath);
    virtual ~StudentWorld();
    
    // For creating and updating game world pieces
    virtual int init();
    virtual int move();
    virtual void cleanUp();
    
    // Interface for actors
    void addActor(Actor* actor);
    bool isOverlappingWithSocrates(double x, double y) const;
    bool isOverlappingWithDirt(double x, double y) const;
    
    void getRadialPosition(int dir, double& dx, double& dy) const;
    double distance(double x1, double y1, double x2, double y2) const;
    
    // Interface for bacteria behavior
    bool findAndEatOverlappingFood(double x, double y);
    bool directionToNearestFoodIfWithinDistance(double x, double y, double dist, int& dir);
    bool directionToSocratesIfWithinDistance(double x, double y, double dist, int& dir);
    
    // Interface for projectiles
    bool damageDamageable(double x, double y, int damage);
    
    // Interface for goodies
    void damageSocrates(int damage);
    void healSocrates(int amount);
    void refillSocratesFlames(int amount);
    
    // Handle number of enemies
    void addNumberOfBacteria(int amount) { m_numEnemies += amount; }
    void decrementNumberOfBacteria() { m_numEnemies--; }

private:
    static const int MAX_OJBECT_DIST_FROM_CENTER = 120;
    
    // Internal helper functions
    void removeDeadGameObjects();
    void addNewActors();
    void updateDisplayText();
    
    bool isOverlappingWithActors(double x, double y, int numToCheck) const;
    int getDirectionToActor(Actor *actor, double x, double y) const;
    bool isOverlapping(double x1, double y1, double x2, double y2) const;
    
    Socrates*         m_socrates;
    std::list<Actor*> m_actors;
    int               m_numEnemies;
};

#endif // STUDENTWORLD_H_
