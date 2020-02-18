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
    
    double distance(int x1, int y1, int x2, int y2);
    void addActor(Actor* actor);

private:
    void removeDeadGameObjects();
    void addNewActors();
    void updateDisplayText();
    
    Socrates* socrates;
    std::list<Actor*> actors;
};

#endif // STUDENTWORLD_H_
