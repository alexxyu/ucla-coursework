#ifndef STUDENTWORLD_H_
#define STUDENTWORLD_H_

#include "GameWorld.h"
#include <vector>
#include <string>

// Students:  Add code to this file, StudentWorld.cpp, Actor.h, and Actor.cpp

class StudentWorld : public GameWorld
{
public:
    static const int MAX_OJBECT_DIST_FROM_CENTER = 120;
    
    StudentWorld(std::string assetPath);
    ~StudentWorld();
    
    virtual int init();
    virtual int move();
    virtual void cleanUp();

private:
    void removeDeadGameObjects();
    void addNewActors();
    void updateDisplayText();
    
    Actor* socrates;
    std::vector<Actor*> actors;
};

#endif // STUDENTWORLD_H_
