#ifndef ACTOR_H_
#define ACTOR_H_

#include "GraphObject.h"
#include "StudentWorld.h"
#include "GameConstants.h"
#include <iostream>

class Actor: public GraphObject
{
public:
    Actor(int imageID, double startX, double startY, int dir, int depth, StudentWorld* world)
     : GraphObject(imageID, startX, startY, dir, depth)
    {
        m_dead = false;
        m_world = world;
    }
    
    virtual ~Actor() { }
    
    virtual void doSomething() { };
    
    bool isDead() const { return m_dead; }
    StudentWorld* getWorld() const { return m_world; }

protected:
    void setDead() { m_dead = true; }
    
private:
    bool          m_dead;
    StudentWorld* m_world;
};

class Damageable: public Actor
{
public:
    Damageable(int imageID, double startX, double startY, int depth, StudentWorld* world, int startHealth)
     : Actor(imageID, startX, startY, 0, depth, world), m_health(startHealth) { }
    
    virtual ~Damageable() { }
    
    int getHealth() const { return m_health; }
    
private:
    int m_health;
};

class Projectile: public Actor
{
public:
    Projectile(int imageID, double startX, double startY, int dir, StudentWorld* world,
               double maxDistance, double damage)
     : Actor(imageID, startX, startY, dir, 1, world)
    {
        m_startX = startX;
        m_startY = startY;
        m_maxDistance = maxDistance;
        m_damage = damage;
    }
    
    virtual ~Projectile() { }
    
    virtual void doSomething();

private:
    double m_startX;
    double m_startY;
    double m_maxDistance;
    double m_damage;
};

class Socrates: public Damageable
{
public:
    
    static const int STARTING_SPRAY_CHARGES = 20;
    static const int STARTING_FLAME_CHARGES = 5;
    static const int STARTING_HEALTH = 100;
    static const int MOVE_DEGREES = 5;

    Socrates(double startX, double startY, StudentWorld* world);
    virtual ~Socrates() { }
    
    virtual void doSomething();
    
    int getSprayCount() const;
    int getFlameCount() const;
    
private:
    int m_spray_count;
    int m_flame_count;
    
    void adjustPosition(int degree);
};

class Spray: public Projectile
{
public:
    static const int MAX_DISTANCE = 112;
    static const int DAMAGE = 2;
    
    Spray(double startX, double startY, int dir, StudentWorld* world)
     : Projectile(IID_SPRAY, startX, startY, dir, world, MAX_DISTANCE, DAMAGE)
    {
        
    }
    
    virtual ~Spray() { }
};

class Flame: public Projectile
{
public:
    static const int MAX_DISTANCE = 32;
    static const int DAMAGE = 5;
    
    Flame(double startX, double startY, int dir, StudentWorld* world)
     : Projectile(IID_FLAME, startX, startY, dir, world, MAX_DISTANCE, DAMAGE)
    {
        
    }
    
    virtual ~Flame() { }
};

class Food: public Actor
{
public:
    Food(double startX, double startY, StudentWorld* world)
     : Actor(IID_FOOD, startX, startY, 90, 1, world)
    {
        
    }
    virtual ~Food() { }
};

class DirtPile: public Damageable
{
public:
    DirtPile(double startX, double startY, StudentWorld* world)
     : Damageable(IID_DIRT, startX, startY, 1, world, 1)
    {
        
    }
    virtual ~DirtPile() { }
    
private:
};

class Pit: public Actor
{
public:
    Pit(double startX, double startY, StudentWorld* world);
    virtual ~Pit() { }
    
    virtual void doSomething();
};

#endif // ACTOR_H_
