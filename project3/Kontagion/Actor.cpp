#include "Actor.h"
#include "StudentWorld.h"
#include <cmath>
using namespace std;

///////////////////////////////////////////////////////////////////////////
//  SOCRATES IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Socrates::Socrates(double startX, double startY, StudentWorld* world)
 : Damageable(IID_PLAYER, startX, startY, 0, 0, world, STARTING_HEALTH)
{
    m_spray_count = STARTING_SPRAY_CHARGES;
    m_flame_count = STARTING_FLAME_CHARGES;
}

void Socrates::doSomething()
{
    int dir;
    StudentWorld* world = getWorld();
    if(world->getKey(dir)) {
        
        switch(dir) {
            case KEY_PRESS_SPACE:
                if(m_spray_count > 0) {
                    // add spray object
                    double sprayStartX, sprayStartY;
                    getPositionInThisDirection(getDirection(), 2*SPRITE_RADIUS, sprayStartX, sprayStartY);
                    world->addActor(new Spray(sprayStartX, sprayStartY, getDirection(), world));
                    m_spray_count--;
                    world->playSound(SOUND_PLAYER_SPRAY);
                }
                break;
            case KEY_PRESS_ENTER:
                if(m_flame_count > 0) {
                    // add flame object
                    int dir = getDirection();
                    double flameStartX, flameStartY;
                    for(int i=0; i<16; i++) {
                        getPositionInThisDirection(dir+22*i, 2*SPRITE_RADIUS, flameStartX, flameStartY);
                        world->addActor(new Flame(flameStartX, flameStartY, dir+22*i, world));
                    }
                    m_flame_count--;
                    world->playSound(SOUND_PLAYER_FIRE);
                }
                break;
            case KEY_PRESS_RIGHT:
                adjustPosition(-MOVE_DEGREES);
                break;
            case KEY_PRESS_LEFT:
                adjustPosition(MOVE_DEGREES);
                break;
            case KEY_PRESS_TAB:
                if(m_flame_count > 0) {
                    // add flame object
                    int dir = getDirection();
                    double flameStartX, flameStartY;
                    for(int i=0; i<16; i++) {
                        getPositionInThisDirection(dir+22*i, 2*SPRITE_RADIUS, flameStartX, flameStartY);
                        world->addActor(new Flame(flameStartX, flameStartY, dir+22*i, world));
                    }
                    world->playSound(SOUND_PLAYER_FIRE);
                }
                break;
            default:
                break;
        }
        
    }
    
    else if(m_spray_count < STARTING_SPRAY_CHARGES)
        m_spray_count++;
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
    if(chance == 0) {
        
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
}

bool Pit::isEmpty()
{
    for(int i=0; i<NUM_OF_BACTERIA_TYPES; i++)
        if(m_bacteriaCount[i] != 0)
            return false;
    
    return true;
}

///////////////////////////////////////////////////////////////////////////
//  BACTERIA IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

void Bacterium::doSomething()
{
    return;
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

void Bacterium::calculateNewBacteriumDistance(double &newX, double &newY)
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

void Bacterium::tryToMove()
{
    double newX, newY;
    getPositionInThisDirection(getDirection(), 3, newX, newY);
    double distToCenter = getWorld()->distance(newX, newY, VIEW_WIDTH/2, VIEW_HEIGHT/2);
    if(!getWorld()->isOverlappingWithDirt(newX, newY) && distToCenter < VIEW_RADIUS) {
        moveTo(newX, newY);
    }
    else {
        setDirection(randInt(0, 359));
        resetMovementPlanDistance();
    }
}

void Bacterium::tryToEatFood()
{
    if(getWorld()->findAndEatOverlappingFood(getX(), getY()))
        m_foodEatenSinceLastDivide++;
}

void RegularSalmonella::doSomething()
{
    if(isDead()) return;
    
    StudentWorld* world = getWorld();
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        world->damageSocrates(DAMAGE);
    }
    else if(getFoodEatenSinceLastDivide() == FOOD_NEEDED_TO_DIVIDE) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new RegularSalmonella(newX, newY, world));
        world->addNumberOfBacteria(1);
        world->playSound(SOUND_BACTERIUM_BORN);
        resetFoodEaten();
    }
    else
        tryToEatFood();
    
    if(getMovementPlanDistance() > 0) {
        decreaseMovementPlanDistance();
        tryToMove();
    }
    else {
        int dir;
        if(world->directionToNearestFoodIfWithinDistance(getX(), getY(), MAX_DISTANCE_TO_FOOD, dir)) {
            double newX, newY;
            getPositionInThisDirection(dir, MOVEMENT, newX, newY);
            if(!world->isOverlappingWithDirt(newX, newY))
                moveTo(newX, newY);
            else {
                setDirection(randInt(0, 359));
                resetMovementPlanDistance();
            }
        }
        else {
            setDirection(randInt(0, 359));
            resetMovementPlanDistance();
        }
    }
    
}

void AggressiveSalmonella::doSomething()
{
    if(isDead()) return;
    
    StudentWorld* world = getWorld();
    
    bool shouldNotMoveForFood = false;
    bool shouldSkip = false;
    
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
        if(shouldNotMoveForFood)
            return;
        shouldSkip = true;
    }
    
    if(!shouldSkip && getFoodEatenSinceLastDivide() == FOOD_NEEDED_TO_DIVIDE) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new AggressiveSalmonella(newX, newY, world));
        world->playSound(SOUND_BACTERIUM_BORN);
        world->addNumberOfBacteria(1);
        resetFoodEaten();
        if(shouldNotMoveForFood)
            return;
        shouldSkip = true;
    }
    
    if(!shouldSkip)
        tryToEatFood();
    
    if(shouldNotMoveForFood)
        return;
    
    if(getMovementPlanDistance() > 0) {
        decreaseMovementPlanDistance();
        tryToMove();
    }
    else {
        int dir;
        if(world->directionToNearestFoodIfWithinDistance(getX(), getY(), MAX_DISTANCE_TO_FOOD, dir)) {
            double newX, newY;
            getPositionInThisDirection(dir, MOVEMENT, newX, newY);
            if(!world->isOverlappingWithDirt(newX, newY))
                moveTo(newX, newY);
            else {
                setDirection(randInt(0, 359));
                resetMovementPlanDistance();
            }
        }
        else {
            setDirection(randInt(0, 359));
            resetMovementPlanDistance();
        }
    }
}

void EColi::doSomething()
{
    if(isDead()) return;
    
    StudentWorld* world = getWorld();
    
    if(world->isOverlappingWithSocrates(getX(), getY()))
        world->damageSocrates(DAMAGE);
    else if(getFoodEatenSinceLastDivide() == FOOD_NEEDED_TO_DIVIDE) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new EColi(newX, newY, world));
        world->addNumberOfBacteria(1);
        world->playSound(SOUND_BACTERIUM_BORN);
        resetFoodEaten();
    }
    else
        tryToEatFood();
    
    int dirToTry;
    if(world->directionToSocratesIfWithinDistance(getX(), getY(),
                                                  MAX_DISTANCE_TO_SOCRATES, dirToTry)) {
        for(int i=0; i<MOVEMENT_TRIES; i++) {
            double newX, newY;
            getPositionInThisDirection(dirToTry, 2, newX, newY);
            if(!world->isOverlappingWithDirt(newX, newY)) {
                moveTo(newX, newY);
                return;
            }
            dirToTry = (dirToTry + 10) % 360;
        }
    }
}

///////////////////////////////////////////////////////////////////////////
//  EXPIRABLE IMPLEMENTATION
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

void Expirable::takeDamage(int damage)
{
    setDead();
}

void RestoreHealthGoodie::giveReward()
{
    getWorld()->healSocrates(HEAL_AMOUNT);
}

void FlameThrowerGoodie::giveReward()
{
    getWorld()->refillSocratesFlames(REFILL_AMOUNT);
}

void ExtraLifeGoodie::giveReward()
{
    getWorld()->incLives();
}

void Fungus::giveReward()
{
    // There is no reward for a fungus, mwahahaha!
    getWorld()->damageSocrates(DAMAGE);
}
