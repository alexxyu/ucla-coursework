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
    Actor(int imageID, double startX, double startY, int dir, int depth, StudentWorld* world)
    : GraphObject(imageID, startX, startY, dir, depth)
    {
        m_dead = false;
        m_world = world;
    }
    
    virtual ~Actor() { }
    
    virtual void doSomething() { }
    virtual void takeDamage(int amount) { }
    
    virtual bool isPit() const { return false; }
    virtual bool isFood() const { return false; }
    virtual bool isDamageable() const { return false; }
    virtual bool isDirtPile() const { return false; }
    
    bool isDead() const { return m_dead; }
    void setDead() { m_dead = true; }
    
    StudentWorld* getWorld() const { return m_world; }
    
private:
    bool          m_dead;
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
    : Actor(imageID, startX, startY, dir, depth, world), m_health(startHealth)
    {
        
    }
    
    virtual ~Damageable() { }
    virtual void takeDamage(int damage)
    {
        m_health -= damage;
        if(m_health <= 0)
            setDead();
    }
    virtual bool isDamageable() const { return true; }
    
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
    static const int POINT_VALUE = 100;
    static const int RESET_MOVEMENT_PLAN_DISTANCE = 10;
    static const int FOOD_NEEDED_TO_DIVIDE = 3;
    
    Bacterium(int imageID, double startX, double startY, StudentWorld* world,
              int health, int soundHurt, int soundDead, int movementPlanDistance)
    : Damageable(imageID, startX, startY, 90, 0, world, health)
    {
        m_soundHurt = soundHurt;
        m_soundDead = soundDead;
        m_foodEatenSinceLastDivide = 0;
        m_movementPlanDistance = movementPlanDistance;
    }
    virtual ~Bacterium() { }
    virtual void doSomething();
    virtual void takeDamage(int damage);
    
    int getFoodEatenSinceLastDivide() const { return m_foodEatenSinceLastDivide; }
    void resetFoodEaten() { m_foodEatenSinceLastDivide = 0; }
    void tryToEatFood();
    
    int getDirectionToActor(Actor* actor) const;
    void tryToMove();
    
    int getMovementPlanDistance() const { return m_movementPlanDistance; }
    void decreaseMovementPlanDistance() { m_movementPlanDistance--; }
    void resetMovementPlanDistance() { m_movementPlanDistance = RESET_MOVEMENT_PLAN_DISTANCE; }
    
    void calculateNewBacteriumDistance(double& newX, double& newY);
    
private:
    int m_soundHurt;
    int m_soundDead;
    int m_foodEatenSinceLastDivide;
    int m_movementPlanDistance;
};

class RegularSalmonella: public Bacterium
{
public:
    static const int STARTING_HEALTH = 4;
    static const int DAMAGE = 1;
    static const int MAX_DISTANCE_TO_FOOD = 128;
    static const int MOVEMENT = 3;
    
    RegularSalmonella(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH,
                SOUND_SALMONELLA_HURT, SOUND_SALMONELLA_DIE, 0)
    {
        
    }
    
    virtual ~RegularSalmonella() { }
    
    virtual void doSomething();
};

class AggressiveSalmonella: public Bacterium
{
public:
    static const int STARTING_HEALTH = 10;
    static const int DAMAGE = 2;
    static const int MAX_DISTANCE_TO_SOCRATES = 72;
    static const int MAX_DISTANCE_TO_FOOD = 128;
    static const int MOVEMENT = 3;
    
    AggressiveSalmonella(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH,
                SOUND_SALMONELLA_HURT, SOUND_SALMONELLA_DIE, 0)
    {
        
    }
    
    virtual ~AggressiveSalmonella() { }
    
    virtual void doSomething();
};

class EColi: public Bacterium
{
public:
    static const int STARTING_HEALTH = 5;
    static const int DAMAGE = 4;
    static const int MOVEMENT = 2;
    static const int MOVEMENT_TRIES = 10;
    static const int MAX_DISTANCE_TO_SOCRATES = 256;
    
    EColi(double startX, double startY, StudentWorld* world)
    : Bacterium(IID_ECOLI, startX, startY, world, STARTING_HEALTH,
                SOUND_ECOLI_HURT, SOUND_SALMONELLA_DIE, 0)
    {
        
    }
    
    virtual ~EColi() { }
    virtual void doSomething();
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
    
    int getSprayCount() const { return m_spray_count; }
    int getFlameCount() const { return m_flame_count; }
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
    virtual bool isDirtPile() const { return true; }
    
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
    static const int MAX_DISTANCE = 100;
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
    virtual bool isFood() const { return true; }
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
    
    virtual bool isPit() { return true; }
    bool isEmpty();
    
private:
    int m_bacteriaCount[NUM_OF_BACTERIA_TYPES];
};

#endif // ACTOR_H_
