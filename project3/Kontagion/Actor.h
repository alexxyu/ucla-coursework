#ifndef ACTOR_H_
#define ACTOR_H_

#include "GraphObject.h"
#include "GameWorld.h"
#include "GameConstants.h"

class Actor: public GraphObject
{
public:
    Actor(int imageID, double startX, double startY, GameWorld* world);
    
    virtual void doSomething() = 0;
    
    bool isDead();
    GameWorld* getWorld();

private:
    bool       m_dead;
    GameWorld* m_world;
    
};

class Socrates: public Actor
{
public:
    Socrates(double startX, double startY, GameWorld* world);
    
    virtual void doSomething();
    
    int getHealth();
    int getSprayCount();
    int getFlameCount();
    
private:
    int m_health;
    int m_spray_count;
    int m_flame_count;
};

#endif // ACTOR_H_
