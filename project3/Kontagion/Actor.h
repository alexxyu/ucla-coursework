#ifndef ACTOR_H_
#define ACTOR_H_

#include "GraphObject.h"
#include "GameWorld.h"
#include "GameConstants.h"

class Actor: public GraphObject
{
public:
    Actor(int imageID, double startX, double startY, int dir, int depth, GameWorld* world);
    
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
    static const int STARTING_SPRAY_CHARGES = 20;
    static const int STARTING_FLAME_CHARGES = 5;
    static const int STARTING_HEALTH = 100;
    static const int MOVE_DEGREES = 5;

    Socrates(double startX, double startY, GameWorld* world);
    
    virtual void doSomething();
    
    int getHealth();
    int getSprayCount();
    int getFlameCount();
    
private:
    int m_health;
    int m_spray_count;
    int m_flame_count;
    
    void adjustPosition(int degree);
};

class DirtPile: public Actor
{
public:
    DirtPile(double startX, double startY, GameWorld* world);
    
    virtual void doSomething();
    
private:
};

#endif // ACTOR_H_
