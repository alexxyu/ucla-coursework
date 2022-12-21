#include "Actor.h"
#include "StudentWorld.h"
#include <cmath>
using namespace std;

///////////////////////////////////////////////////////////////////////////
//  ACTOR IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Actor::Actor(int imageID, double startX, double startY, int dir,
             int depth, StudentWorld* world)
: GraphObject(imageID, startX, startY, dir, depth)
{
    m_dead = false;
    m_world = world;
}

bool Actor::isFood() const { return false; }
bool Actor::isDamageable() const { return false; }
bool Actor::isDirtPile() const { return false; }
bool Actor::isDead() const { return m_dead; }
void Actor::setDead() { m_dead = true; }
StudentWorld* Actor::getWorld() const { return m_world; }

///////////////////////////////////////////////////////////////////////////
//  DAMAGEABLES IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Damageable::Damageable(int imageID, double startX, double startY,
                       int dir, int depth, StudentWorld* world, int startHealth)
: Actor(imageID, startX, startY, dir, depth, world), m_health(startHealth)
{
    
}

void Damageable::takeDamage(int damage)
{
    m_health -= damage;
    if(m_health <= 0)
        setDead();
}

bool Damageable::isDamageable() const { return true; }
int Damageable::getHealth() const { return m_health; }
void Damageable::healToAmount(int amount) { m_health = amount; }

///////////////////////////////////////////////////////////////////////////
//  EXPIRABLES IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Expirable::Expirable(int imageID, double startX, double startY,
                     StudentWorld* world, int pointValue, bool playSoundOnTouch)
 : Damageable(imageID, startX, startY, 0, 1, world, 0)
{
    m_tickCount = 0;
    m_pointValue = pointValue;
    m_playSound = playSoundOnTouch;
    generateLifespan();
}

void Expirable::doSomething()
{
    if(isDead())
        return;
    
    StudentWorld* world = getWorld();
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        world->increaseScore(m_pointValue);
        setDead();
        if(m_playSound)
            getWorld()->playSound(SOUND_GOT_GOODIE);
        giveReward();
        return;
    }
    
    m_tickCount++;
    if(m_tickCount >= m_lifespan)
        setDead();
}

void Expirable::generateLifespan()
{
    m_lifespan = max(rand() % (300 - 10 * getWorld()->getLevel()), 50);
}

void Expirable::takeDamage(int)
{
    setDead();
}

RestoreHealthGoodie::RestoreHealthGoodie(double startX, double startY,
                                         StudentWorld* world)
: Expirable(IID_RESTORE_HEALTH_GOODIE, startX, startY, world, POINT_VALUE)
{
    
}

void RestoreHealthGoodie::giveReward() const
{
    getWorld()->healSocrates(HEAL_AMOUNT);
}

FlameThrowerGoodie::FlameThrowerGoodie(double startX, double startY,
                                       StudentWorld* world)
: Expirable(IID_FLAME_THROWER_GOODIE, startX, startY, world, POINT_VALUE)
{
    
}

void FlameThrowerGoodie::giveReward() const
{
    getWorld()->refillSocratesFlames(REFILL_AMOUNT);
}

ExtraLifeGoodie::ExtraLifeGoodie(double startX, double startY, StudentWorld* world)
: Expirable(IID_EXTRA_LIFE_GOODIE, startX, startY, world, POINT_VALUE)
{
    
}

void ExtraLifeGoodie::giveReward() const
{
    getWorld()->incLives();
}

Fungus::Fungus(double startX, double startY, StudentWorld* world)
: Expirable(IID_FUNGUS, startX, startY, world, POINT_VALUE, false)
{
    
}

void Fungus::giveReward() const
{
    // There is no reward for a fungus, mwahahaha!
    getWorld()->damageSocrates(DAMAGE);
}

///////////////////////////////////////////////////////////////////////////
//  BACTERIUM IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Bacterium::Bacterium(int imageID, double startX, double startY, StudentWorld* world,
                     int health, int soundHurt, int soundDead, int damage, int movement)
: Damageable(imageID, startX, startY, 90, 0, world, health)
{
    m_soundHurt = soundHurt;
    m_soundDead = soundDead;
    m_foodEatenSinceLastDivide = 0;
    m_movementPlanDistance = 0;
    m_damage = damage;
    m_movement = movement;
}

void Bacterium::doSomething()
{
    if(isDead())
        return;
    
    StudentWorld* world = getWorld();
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        world->damageSocrates(m_damage);
    }
    else if(!tryToDivide())
        tryToEatFood();
}

void Bacterium::takeDamage(int damage)
{
    if(isDead())
        return;

    Damageable::takeDamage(damage);
    if(!isDead()) {
        getWorld()->playSound(m_soundHurt);
    }
    else {
        StudentWorld* world = getWorld();
        world->playSound(m_soundDead);
        world->increaseScore(POINT_VALUE);
        
        if(randInt(0, 1) == 0)
            world->addActor(new Food(getX(), getY(), world));
        world->decrementNumberOfBacteria();
    }
}

bool Bacterium::tryToMove()
{
    if(m_movementPlanDistance > 0) {
        double newX, newY;
        getPositionInThisDirection(getDirection(), m_movement, newX, newY);
        double distToCenter = getWorld()->distance(newX, newY, VIEW_WIDTH/2, VIEW_HEIGHT/2);
        
        if(!getWorld()->isOverlappingWithDirt(newX, newY) && distToCenter < VIEW_RADIUS)
            moveTo(newX, newY);
        else
            tryNewDirection();
        
        m_movementPlanDistance--;
        return true;
    }
    
    return false;
}

void Bacterium::tryToEatFood()
{
    if(getWorld()->findAndEatOverlappingFood(getX(), getY()))
        m_foodEatenSinceLastDivide++;
}

bool Bacterium::tryToDivide()
{
    if(m_foodEatenSinceLastDivide == FOOD_NEEDED_TO_DIVIDE) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        divide(newX, newY);
        
        StudentWorld* world = getWorld();
        world->playSound(SOUND_BACTERIUM_BORN);
        world->addNumberOfBacteria(1);
        
        m_foodEatenSinceLastDivide = 0;
        return true;
    }
    
    return false;
}

void Bacterium::moveTowardFood()
{
    StudentWorld* world = getWorld();
    int dir;
    if(world->directionToNearestFoodIfWithinDistance(getX(), getY(), MAX_DISTANCE_TO_FOOD, dir)) {
        double newX, newY;
        getPositionInThisDirection(dir, m_movement, newX, newY);
        if(!world->isOverlappingWithDirt(newX, newY)) {
            setDirection(dir);
            moveTo(newX, newY);
        }
        else
            tryNewDirection();
    }
    else
        tryNewDirection();
}

void Bacterium::calculateNewBacteriumDistance(double &newX, double &newY) const
{
    newX = getX();
    if(newX < VIEW_WIDTH / 2)
        newX += SPRITE_WIDTH/2;
    else if(newX > VIEW_WIDTH/2)
        newX -= SPRITE_WIDTH/2;
    
    newY = getY();
    if(newY < VIEW_HEIGHT / 2)
        newY += SPRITE_HEIGHT/2;
    else if(newY > VIEW_HEIGHT/2)
        newY -= SPRITE_HEIGHT/2;
}

void Bacterium::tryNewDirection()
{
    setDirection(randInt(0, 359));
    m_movementPlanDistance = RESET_MOVEMENT_PLAN_DISTANCE;
}

///////////////////////////////////////////////////////////////////////////
//  SALMONELLA AND E. COLI IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

RegularSalmonella::RegularSalmonella(double startX, double startY, StudentWorld* world)
: Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH,
            SOUND_SALMONELLA_HURT, SOUND_SALMONELLA_DIE, DAMAGE, MOVEMENT)
{
    
}

void RegularSalmonella::doSomething()
{
    if(isDead())
        return;
    
    Bacterium::doSomething();
    if(!tryToMove())
        moveTowardFood();
}

void RegularSalmonella::divide(double newX, double newY)
{
    getWorld()->addActor(new RegularSalmonella(newX, newY, getWorld()));
}

AggressiveSalmonella::AggressiveSalmonella(double startX, double startY,
                                           StudentWorld* world)
: Bacterium(IID_SALMONELLA, startX, startY, world, STARTING_HEALTH,
            SOUND_SALMONELLA_HURT, SOUND_SALMONELLA_DIE, DAMAGE, MOVEMENT)
{
    
}

void AggressiveSalmonella::doSomething()
{
    if(isDead())
        return;
    
    StudentWorld* world = getWorld();
    bool shouldNotMoveForFood = false;
    bool shouldSkip = false;
    
    // try to move towards Socrates
    int dir;
    if(world->directionToSocratesIfWithinDistance(getX(), getY(), MAX_DISTANCE_TO_SOCRATES, dir)) {
        double newX, newY;
        getPositionInThisDirection(dir, MOVEMENT, newX, newY);
        if(!world->isOverlappingWithDirt(newX, newY))
            moveTo(newX, newY);
        shouldNotMoveForFood = true;
    }
    
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        world->damageSocrates(DAMAGE);
        shouldSkip = true;
    }
    if(!shouldSkip && tryToDivide())
        shouldSkip = true;
    if(!shouldSkip)
        tryToEatFood();
    
    if(!shouldNotMoveForFood && !tryToMove())
        moveTowardFood();
}

void AggressiveSalmonella::divide(double newX, double newY)
{
    getWorld()->addActor(new AggressiveSalmonella(newX, newY, getWorld()));
}

EColi::EColi(double startX, double startY, StudentWorld* world)
: Bacterium(IID_ECOLI, startX, startY, world, STARTING_HEALTH,
            SOUND_ECOLI_HURT, SOUND_SALMONELLA_DIE, DAMAGE, MOVEMENT)
{
    
}

void EColi::doSomething()
{
    if(isDead())
        return;
    
    Bacterium::doSomething();
    
    StudentWorld* world = getWorld();
    int dirToTry;
    if(world->directionToSocratesIfWithinDistance(getX(), getY(),
                                                  MAX_DISTANCE_TO_SOCRATES, dirToTry))
    {
        // try to move towards Socrates
        for(int i=0; i<MOVEMENT_TRIES; i++) {
            double newX, newY;
            getPositionInThisDirection(dirToTry, MOVEMENT, newX, newY);
            if(!world->isOverlappingWithDirt(newX, newY)) {
                moveTo(newX, newY);
                return;
            }
            dirToTry = (dirToTry + 10) % 360;
        }
    }
}

void EColi::divide(double newX, double newY)
{
    getWorld()->addActor(new EColi(newX, newY, getWorld()));
}

///////////////////////////////////////////////////////////////////////////
//  SOCRATES IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Socrates::Socrates(double startX, double startY, StudentWorld* world)
: Damageable(IID_PLAYER, startX, startY, 0, 0, world, STARTING_HEALTH)
{
    m_spray_count = STARTING_SPRAY_CHARGES;
    m_flame_count = STARTING_FLAME_CHARGES;
}

int Socrates::getSprayCount() const { return m_spray_count; }
int Socrates::getFlameCount() const { return m_flame_count; }
void Socrates::refillFlames(int amount) { m_flame_count += amount; }

void Socrates::doSomething()
{
    // handle user input
    int dir;
    if(getWorld()->getKey(dir)) {
        switch(dir) {
            case KEY_PRESS_SPACE:
                shootSpray();
                break;
            case KEY_PRESS_ENTER:
                shootFlameCharge();
                break;
            case KEY_PRESS_RIGHT:
                adjustPosition(-MOVE_DEGREES);
                break;
            case KEY_PRESS_LEFT:
                adjustPosition(MOVE_DEGREES);
                break;
            default:
                break;
        }
    }
    else if(m_spray_count < STARTING_SPRAY_CHARGES)
        m_spray_count++;
}

void Socrates::takeDamage(int damage)
{
    Damageable::takeDamage(damage);
    getWorld()->playSound(SOUND_PLAYER_HURT);
}

void Socrates::shootSpray()
{
    if(m_spray_count > 0) {
        StudentWorld* world = getWorld();
        double sprayStartX, sprayStartY;
        
        getPositionInThisDirection(getDirection(), 2*SPRITE_RADIUS, sprayStartX, sprayStartY);
        world->addActor(new Spray(sprayStartX, sprayStartY, getDirection(), world));
        m_spray_count--;
        world->playSound(SOUND_PLAYER_SPRAY);
    }
}

void Socrates::shootFlameCharge()
{
    if(m_flame_count > 0) {
        StudentWorld* world = getWorld();
        int dir = getDirection();
        double flameStartX, flameStartY;
        
        // generate flame charges in circular fashion around Socrates
        for(int i=0; i<NUMBER_OF_FLAMES_PER_CHARGE; i++) {
            getPositionInThisDirection(dir+DEGREES_BETWEEN_FLAMES*i, 2*SPRITE_RADIUS,
                                       flameStartX, flameStartY);
            world->addActor(new Flame(flameStartX, flameStartY, dir+DEGREES_BETWEEN_FLAMES*i, world));
        }
        m_flame_count--;
        world->playSound(SOUND_PLAYER_FIRE);
    }
}

void Socrates::adjustPosition(int degree)
{
    int newAngle = (getDirection() + 180 + degree) % 360;
    double dx, dy;
    getWorld()->getRadialPosition(newAngle, dx, dy);
    
    moveTo(dx, dy);
    setDirection((newAngle + 180) % 360);
}

///////////////////////////////////////////////////////////////////////////
//  PROJECTILE IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Projectile::Projectile(int imageID, double startX, double startY, int dir,
                       StudentWorld* world, double maxDistance, int damage)
: Actor(imageID, startX, startY, dir, 1, world)
{
    m_startX = startX;
    m_startY = startY;
    m_maxDistance = maxDistance;
    m_damage = damage;
}

void Projectile::doSomething()
{
    if(isDead())
        return;
    
    // check overlap with damageable object
    if(getWorld()->damageDamageable(getX(), getY(), m_damage)) {
        setDead();
        return;
    }
    
    moveForward(SPRITE_RADIUS*2);
    
    // check if dissipated
    if(getWorld()->distance(getX(), getY(), m_startX, m_startY) >= m_maxDistance)
        setDead();
}

Spray::Spray(double startX, double startY, int dir, StudentWorld* world)
 : Projectile(IID_SPRAY, startX, startY, dir, world, MAX_DISTANCE, DAMAGE)
{
    
}

Flame::Flame(double startX, double startY, int dir, StudentWorld* world)
: Projectile(IID_FLAME, startX, startY, dir, world, MAX_DISTANCE, DAMAGE)
{
    
}

///////////////////////////////////////////////////////////////////////////
//  PIT IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Pit::Pit(double startX, double startY, StudentWorld* world)
 : Actor(IID_PIT, startX, startY, 0, 1, world)
{
    m_bacteriaCount[REGULAR_SALMONELLA_ID] = NUM_REGULAR_SALMONELLA;
    m_bacteriaCount[AGGRESSIVE_SALMONELLA_ID] = NUM_AGGRESSIVE_SALMONELLA;
    m_bacteriaCount[ECOLI_ID] = NUM_ECOLI;
    
    world->addNumberOfBacteria(NUM_REGULAR_SALMONELLA + NUM_AGGRESSIVE_SALMONELLA + NUM_ECOLI);
}

void Pit::doSomething()
{
    if(isEmpty()) {
        setDead();
        return;
    }
    
    int chance = randInt(0, 49);
    if(chance == 0)
        generateBacteria();
}

bool Pit::isEmpty() const
{
    for(int i=0; i<NUM_OF_BACTERIA_TYPES; i++)
        if(m_bacteriaCount[i] != 0)
            return false;
    
    return true;
}

void Pit::generateBacteria()
{
    int bacteriumGenerated;
    do {
        bacteriumGenerated = randInt(0, NUM_OF_BACTERIA_TYPES-1);
    } while(m_bacteriaCount[bacteriumGenerated] < 1);
    
    StudentWorld* world = getWorld();
    
    switch(bacteriumGenerated) {
        case REGULAR_SALMONELLA_ID:
            world->addActor(new RegularSalmonella(getX(), getY(), world));
            break;
        case AGGRESSIVE_SALMONELLA_ID:
            world->addActor(new AggressiveSalmonella(getX(), getY(), world));
            break;
        case ECOLI_ID:
            world->addActor(new EColi(getX(), getY(), world));
            break;
        default:
            break;
    }
    
    m_bacteriaCount[bacteriumGenerated]--;
    world->playSound(SOUND_BACTERIUM_BORN);
}

///////////////////////////////////////////////////////////////////////////
//  OTHER IMPLEMENTATIONS
///////////////////////////////////////////////////////////////////////////

DirtPile::DirtPile(double startX, double startY, StudentWorld* world)
: Damageable(IID_DIRT, startX, startY, 0, 1, world, 1)
{
    
}

bool DirtPile::isDirtPile() const { return true; }
void DirtPile::doSomething() { };

Food::Food(double startX, double startY, StudentWorld* world)
: Actor(IID_FOOD, startX, startY, 90, 1, world)
{
    
}

void Food::doSomething() { }
bool Food::isFood() const { return true; }
