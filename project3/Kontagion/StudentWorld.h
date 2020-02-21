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
    static const int MAX_OJBECT_DIST_FROM_CENTER = 120;
    
    StudentWorld(std::string assetPath);
    virtual ~StudentWorld();
    
    // For creating and updating game world pieces
    virtual int init();
    virtual int move();
    virtual void cleanUp();
    
    // Interface for actors
    void addActor(Actor* actor);
    Socrates* getSocrates() const { return socrates; }
    
    Actor* findNearestFood(double x, double y) const;
    
    bool damageDamageable(double x, double y, int damage);
    
    bool isOverlappingWithSocrates(double x, double y) const;
    bool isOverlappingWithDirt(double x, double y) const;
    bool isOverlapping(double x1, double y1, double x2, double y2) const;
    
    // Handle Socrates
    
    void getRadialPosition(int dir, double& dx, double& dy) const;
    double distance(double x1, double y1, double x2, double y2) const;

private:
    // Internel helper functions
    void removeDeadGameObjects();
    void addNewActors();
    void updateDisplayText();
    bool isOverlappingWithActors(double x, double y, int numToCheck) const;
    
    Socrates*         socrates;
    std::list<Actor*> actors;
    int               numEnemies;
};

#endif // STUDENTWORLD_H_
