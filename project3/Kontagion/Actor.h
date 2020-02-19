#ifndef ACTOR_H_
#define ACTOR_H_

#include "GraphObject.h"
#include "StudentWorld.h"
#include "GameConstants.h"
#include <iostream>

///////////////////////////////////////////////////////////////////////////
//  ACTOR DECLARATION
///////////////////////////////////////////////////////////////////////////

class Actor: public GraphObject
{
public:
    Actor(int imageID, double startX, double startY, int dir, int depth,
          StudentWorld* world, bool damageable=false)
    : GraphObject(imageID, startX, startY, dir, depth)
    {
        m_dead = false;
        m_world = world;
        m_damageable = damageable;
    }
    
    virtual ~Actor() { }
    
    virtual void doSomething() { }
    virtual void takeDamage(int amount) { }
    
    bool isDead() const { return m_dead; }
    bool isDamageable() const { return m_damageable; }
    StudentWorld* getWorld() const { return m_world; }

protected:
    void setDead() { m_dead = true; }
    
private:
    bool          m_dead;
    bool          m_damageable;
    StudentWorld* m_world;
};

///////////////////////////////////////////////////////////////////////////
//  DAMAGEABLES DECLARATION
///////////////////////////////////////////////////////////////////////////

class Damageable: public Actor
{
public:
    Damageable(int imageID, double startX, double startY, int dir, int depth,
               StudentWorld* world, int startHealth)
    : Actor(imageID, startX, startY, dir, depth, world, true), m_health(startHealth)
    {
        
    }
    
    virtual ~Damageable() { }
    virtual void takeDamage(int damage)
    {
        m_health -= damage;
        if(m_health <= 0)
            setDead();
    }
    
    int getHealth() const { return m_health; }
    void healToAmount(int amount) { m_health = amount; }
    
private:
    int m_health;
};

///////////////////////////////////////////////////////////////////////////
//  EXPIRABLES DECLARATION
///////////////////////////////////////////////////////////////////////////

class Expirable: public Damageable
{
public:
    Expirable(int imageID, double startX, double startY,
              StudentWorld* world, int pointValue, bool playSoundOnTouch=true);
    virtual ~Expirable() { }
    
    virtual void doSomething();
    virtual void giveReward() = 0;
    virtual void takeDamage(int damage);
    
private:
    int  m_tickCount;
    int  m_lifespan;
    int  m_pointValue;
    bool m_playSound;

    void generateLifespan();
};

class RestoreHealthGoodie: public Expirable
{
public:
    static const int POINT_VALUE = 250;
    static const int HEAL_AMOUNT = 100;
    
    RestoreHealthGoodie(double startX, double startY, StudentWorld* world)
    : Expirable(IID_RESTORE_HEALTH_GOODIE, startX, startY, world, POINT_VALUE)
    {
        
    }
    virtual ~RestoreHealthGoodie() { }
    
    virtual void giveReward();
};

class FlameThrowerGoodie: public Expirable
{
public:
    static const int POINT_VALUE = 300;
    static const int REFILL_AMOUNT = 5;
    
    FlameThrowerGoodie(double startX, double startY, StudentWorld* world)
    : Expirable(IID_FLAME_THROWER_GOODIE, startX, startY, world, POINT_VALUE)
    {
        
    }
    virtual ~FlameThrowerGoodie() { }
    
    virtual void giveReward();
};

class ExtraLifeGoodie: public Expirable
{
public:
    static const int POINT_VALUE = 500;
    
    ExtraLifeGoodie(double startX, double startY, StudentWorld* world)
    : Expirable(IID_EXTRA_LIFE_GOODIE, startX, startY, world, POINT_VALUE)
    {
        
    }
    virtual ~ExtraLifeGoodie() { }
    
    virtual void giveReward();
};

class Fungus: public Expirable
{
public:
    static const int POINT_VALUE = -50;
    static const int DAMAGE = 20;
    
    Fungus(double startX, double startY, StudentWorld* world)
    : Expirable(IID_FUNGUS, startX, startY, world, POINT_VALUE, false)
    {
        
    }
    virtual ~Fungus() { }
    virtual void giveReward();
};

///////////////////////////////////////////////////////////////////////////
//  BACTERIA DECLARATION
///////////////////////////////////////////////////////////////////////////

class Bacterium: public Damageable
{
public:
    Bacterium(int imageID, double startX, double startY, StudentWorld* world, int health)
    : Damageable(imageID, startX, startY, 90, 0, world, health)
    {
        
    }
    virtual ~Bacterium() { }
    virtual void doSomething();
};

class RegularSalmonella: public Bacterium
{
public:
    static const int STARTING_HEALTH = 4;
    
    RegularSalmonella(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH)
    {
        
    }
};

class AggressiveSalmonella: public Bacterium
{
public:
    static const int STARTING_HEALTH = 10;
    
    AggressiveSalmonella(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH)
    {
        
    }
};

class EColi: public Bacterium
{
public:
    static const int STARTING_HEALTH = 5;
    
    EColi(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_ECOLI, startX, startY, world, STARTING_HEALTH)
    {
        
    }
};

///////////////////////////////////////////////////////////////////////////
//  SOCRATES DECLARATION
///////////////////////////////////////////////////////////////////////////

class Socrates: public Damageable
{
public:
    static const int MOVE_DEGREES = 5;
    static const int STARTING_HEALTH = 100;
    static const int STARTING_SPRAY_CHARGES = 20;
    static const int STARTING_FLAME_CHARGES = 5;

    Socrates(double startX, double startY, StudentWorld* world);
    virtual ~Socrates() { }
    
    virtual void doSomething();
    
    int getSprayCount() const;
    int getFlameCount() const;
    void refillFlames(int amount) { m_flame_count += amount; }
    
private:
    int m_spray_count;
    int m_flame_count;
    
    void adjustPosition(int degree);
};

class DirtPile: public Damageable
{
public:
    DirtPile(double startX, double startY, StudentWorld* world)
    : Damageable(IID_DIRT, startX, startY, 0, 1, world, 1)
    {
        
    }
    virtual ~DirtPile() { }
    
private:
};

///////////////////////////////////////////////////////////////////////////
//  PROJECTILES DECLARATION
///////////////////////////////////////////////////////////////////////////

class Projectile: public Actor
{
public:
    Projectile(int imageID, double startX, double startY, int dir, StudentWorld* world,
               double maxDistance, int damage)
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
    int m_damage;
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

///////////////////////////////////////////////////////////////////////////
//  OTHER DECLARATIONS
///////////////////////////////////////////////////////////////////////////

class Food: public Actor
{
public:
    Food(double startX, double startY, StudentWorld* world)
    : Actor(IID_FOOD, startX, startY, 90, 1, world)
    {
        
    }
    virtual ~Food() { }
};

class Pit: public Actor
{
public:
    static const int NUM_REGULAR_SALMONELLA = 5;
    static const int NUM_AGGRESSIVE_SALMONELLA = 3;
    static const int NUM_ECOLI = 2;
    
    static const int REGULAR_SALMONELLA_ID = 0;
    static const int AGGRESSIVE_SALMONELLA_ID = 1;
    static const int ECOLI_ID = 2;
    
    static const int NUM_OF_BACTERIA_TYPES = 3;
    
    Pit(double startX, double startY, StudentWorld* world);
    virtual ~Pit() { }
    
    virtual void doSomething();
    bool isEmpty();
    
private:
    int m_bacteriaCount[NUM_OF_BACTERIA_TYPES];
};

#endif // ACTOR_H_
