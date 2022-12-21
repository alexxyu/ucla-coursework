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
    Actor(int imageID, double startX, double startY, int dir,
          int depth, StudentWorld* world);
    virtual ~Actor() { }
    
    virtual void doSomething() = 0;
    
    virtual bool isFood() const;
    virtual bool isDamageable() const;
    virtual bool isDirtPile() const;
    
    bool isDead() const;
    void setDead();
    
protected:
    StudentWorld* getWorld() const;
    
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
    Damageable(int imageID, double startX, double startY,
               int dir, int depth, StudentWorld* world, int startHealth);
    virtual ~Damageable() { }
    
    virtual void takeDamage(int damage);
    
    int getHealth() const;
    void healToAmount(int amount);
    
private:
    int m_health;
    
    virtual bool isDamageable() const;
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
    
    virtual void giveReward() const = 0;
    
private:
    int  m_tickCount;
    int  m_lifespan;
    int  m_pointValue;
    bool m_playSound;

    void generateLifespan();
    
    virtual void doSomething();
    virtual void takeDamage(int);
};

class RestoreHealthGoodie: public Expirable
{
public:
    RestoreHealthGoodie(double startX, double startY, StudentWorld* world);
    virtual ~RestoreHealthGoodie() { }
    
private:
    static const int POINT_VALUE = 250;
    static const int HEAL_AMOUNT = 100;
    
    virtual void giveReward() const;
};

class FlameThrowerGoodie: public Expirable
{
public:
    FlameThrowerGoodie(double startX, double startY, StudentWorld* world);
    virtual ~FlameThrowerGoodie() { }
    
private:
    static const int POINT_VALUE = 300;
    static const int REFILL_AMOUNT = 5;
    
    virtual void giveReward() const;
};

class ExtraLifeGoodie: public Expirable
{
public:
    ExtraLifeGoodie(double startX, double startY, StudentWorld* world);
    virtual ~ExtraLifeGoodie() { }
    
private:
    static const int POINT_VALUE = 500;
    
    virtual void giveReward() const;
};

class Fungus: public Expirable
{
public:
    Fungus(double startX, double startY, StudentWorld* world);
    virtual ~Fungus() { }
    
private:
    static const int POINT_VALUE = -50;
    static const int DAMAGE = 20;
    
    virtual void giveReward() const;
};

///////////////////////////////////////////////////////////////////////////
//  BACTERIA DECLARATIONS
///////////////////////////////////////////////////////////////////////////

class Bacterium: public Damageable
{
public:
    Bacterium(int imageID, double startX, double startY, StudentWorld* world,
              int health, int soundHurt, int soundDead, int damage, int movement);
    virtual ~Bacterium() { }
    
protected:
    void tryToEatFood();
    bool tryToMove();
    bool tryToDivide();
    void moveTowardFood();
    
    virtual void doSomething();
    virtual void divide(double newX, double newY) = 0;
    
private:
    static const int POINT_VALUE = 100;
    static const int RESET_MOVEMENT_PLAN_DISTANCE = 10;
    static const int FOOD_NEEDED_TO_DIVIDE = 3;
    static const int MAX_DISTANCE_TO_FOOD = 128;
    
    int m_soundHurt;
    int m_soundDead;
    int m_foodEatenSinceLastDivide;
    int m_movementPlanDistance;
    int m_damage;
    int m_movement;
    
    virtual void takeDamage(int damage);
    
    void calculateNewBacteriumDistance(double& newX, double& newY) const;
    void tryNewDirection();
};

class RegularSalmonella: public Bacterium
{
public:
    RegularSalmonella(double startX, double startY, StudentWorld* world);
    virtual ~RegularSalmonella() { }
    
private:
    static const int STARTING_HEALTH = 4;
    static const int DAMAGE = 1;
    static const int MOVEMENT = 3;
    
    virtual void doSomething();
    virtual void divide(double newX, double newY);
};

class AggressiveSalmonella: public Bacterium
{
public:
    AggressiveSalmonella(double startX, double startY, StudentWorld* world);
    virtual ~AggressiveSalmonella() { }
    
private:
    static const int STARTING_HEALTH = 10;
    static const int DAMAGE = 2;
    static const int MAX_DISTANCE_TO_SOCRATES = 72;
    static const int MOVEMENT = 3;
    
    virtual void doSomething();
    virtual void divide(double newX, double newY);
};

class EColi: public Bacterium
{
public:
    EColi(double startX, double startY, StudentWorld* world);
    virtual ~EColi() { }

private:
    static const int STARTING_HEALTH = 5;
    static const int DAMAGE = 4;
    static const int MOVEMENT = 2;
    static const int MOVEMENT_TRIES = 10;
    static const int MAX_DISTANCE_TO_SOCRATES = 256;
    
    virtual void doSomething();
    virtual void divide(double newX, double newY);
};

///////////////////////////////////////////////////////////////////////////
//  SOCRATES DECLARATION
///////////////////////////////////////////////////////////////////////////

class Socrates: public Damageable
{
public:
    Socrates(double startX, double startY, StudentWorld* world);
    virtual ~Socrates() { }
    
    virtual void doSomething();
    virtual void takeDamage(int damage);
    
    int getSprayCount() const;
    int getFlameCount() const;
    void refillFlames(int amount);
    
private:
    static const int MOVE_DEGREES = 5;
    static const int STARTING_HEALTH = 100;
    static const int STARTING_SPRAY_CHARGES = 20;
    static const int STARTING_FLAME_CHARGES = 5;
    static const int DEGREES_BETWEEN_FLAMES = 22;
    static const int NUMBER_OF_FLAMES_PER_CHARGE = 16;

    int m_spray_count;
    int m_flame_count;
    
    void shootSpray();
    void shootFlameCharge();
    void adjustPosition(int degree);
};

class DirtPile: public Damageable
{
public:
    DirtPile(double startX, double startY, StudentWorld* world);
    virtual ~DirtPile() { }
    
private:
    virtual bool isDirtPile() const;
    virtual void doSomething();
};

///////////////////////////////////////////////////////////////////////////
//  PROJECTILE DECLARATIONS
///////////////////////////////////////////////////////////////////////////

class Projectile: public Actor
{
public:
    Projectile(int imageID, double startX, double startY, int dir, StudentWorld* world,
               double maxDistance, int damage);
    virtual ~Projectile() { }

private:
    double m_startX;
    double m_startY;
    double m_maxDistance;
    int m_damage;
    
    virtual void doSomething();
};

class Spray: public Projectile
{
public:
    Spray(double startX, double startY, int dir, StudentWorld* world);
    virtual ~Spray() { }
    
private:
    static const int MAX_DISTANCE = 112;
    static const int DAMAGE = 2;
};

class Flame: public Projectile
{
public:
    Flame(double startX, double startY, int dir, StudentWorld* world);
    virtual ~Flame() { }
    
private:
    static const int MAX_DISTANCE = 32;
    static const int DAMAGE = 5;
};

///////////////////////////////////////////////////////////////////////////
//  OTHER DECLARATIONS
///////////////////////////////////////////////////////////////////////////

class Food: public Actor
{
public:
    Food(double startX, double startY, StudentWorld* world);
    virtual ~Food() { }
    
private:
    virtual void doSomething();
    virtual bool isFood() const;
};

class Pit: public Actor
{
public:
    Pit(double startX, double startY, StudentWorld* world);
    virtual ~Pit() { }
    
private:
    static const int NUM_REGULAR_SALMONELLA = 5;
    static const int NUM_AGGRESSIVE_SALMONELLA = 3;
    static const int NUM_ECOLI = 2;
    
    static const int REGULAR_SALMONELLA_ID = 0;
    static const int AGGRESSIVE_SALMONELLA_ID = 1;
    static const int ECOLI_ID = 2;
    
    static const int NUM_OF_BACTERIA_TYPES = 3;
    
    int m_bacteriaCount[NUM_OF_BACTERIA_TYPES];
    
    virtual void doSomething();
    
    bool isEmpty() const;
    void generateBacteria();
};

#endif // ACTOR_H_
