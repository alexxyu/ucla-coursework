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
    
    virtual int init();
    virtual int move();
    virtual void cleanUp();
    
    void addActor(Actor* actor);
    Socrates* getSocrates() const { return socrates; }
    
    double distance(double x1, double y1, double x2, double y2) const;
    Actor* findOverlapWithDamageable(double x, double y) const;
    bool isOverlappingWithSocrates(double x, double y) const;
    void getRadialPosition(int dir, double& dx, double& dy) const;

private:
    void removeDeadGameObjects();
    void addNewActors();
    void updateDisplayText();
    bool isOverlapping(double x1, double y1, double x2, double y2) const;
    
    Socrates*         socrates;
    std::list<Actor*> actors;
};

#endif // STUDENTWORLD_H_
