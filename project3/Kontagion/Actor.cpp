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

int Socrates::getSprayCount() const
{
    return m_spray_count;
}

int Socrates::getFlameCount() const
{
    return m_flame_count;
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

int Pit::size()
{
    int count = 0;
    for(int i=0; i<NUM_OF_BACTERIA_TYPES; i++)
        count += m_bacteriaCount[i];
    
    return count;
}

bool Pit::isEmpty()
{
    return size() == 0;
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
        // notify StudentWorld that bacterium is dead
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

int Bacterium::getDirectionToActor(Actor *actor) const
{
    double dir = atan2(actor->getY() - getY(), actor->getX() - getX());
    const double PI = atan(1) * 4;
    dir = dir / PI * 180;
    return dir;
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
    StudentWorld* world = getWorld();
    Actor* overlappingFood = world->findNearestFood(getX(), getY());
    if(overlappingFood != nullptr &&
            world->isOverlapping(getX(), getY(), overlappingFood->getX(), overlappingFood->getY())) {
        eatFood();
        overlappingFood->setDead();
    }
}

void AggressiveSalmonella::doSomething()
{
    if(isDead()) return;
    
    StudentWorld* world = getWorld();
    Socrates* socrates = world->getSocrates();
    double distToSocrates = getWorld()->distance(getX(), getY(),
                                                 socrates->getX(), socrates->getY());
    
    bool shouldNotMoveForFood = false;
    bool shouldSkip = false;
    if(distToSocrates <= 72) {
        double newX, newY;
        int dir = getDirectionToActor(socrates);
        getPositionInThisDirection(dir, 3, newX, newY);
        if(!world->isOverlappingWithDirt(newX, newY))
            moveTo(newX, newY);
        shouldNotMoveForFood = true;
    }
    
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        socrates->takeDamage(DAMAGE);
        if(shouldNotMoveForFood)
            return;
        shouldSkip = true;
    }
    
    if(!shouldSkip && getFoodEatenSinceLastDivide() == 3) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new AggressiveSalmonella(newX, newY, world));
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
        Actor* nearestFood = world->findNearestFood(getX(), getY());
        if(nearestFood != nullptr &&
           world->distance(getX(), getY(), nearestFood->getX(), nearestFood->getY()) <= 128) {
            double newX, newY;
            int dir = getDirectionToActor(nearestFood);
            getPositionInThisDirection(dir, 3, newX, newY);
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

void RegularSalmonella::doSomething()
{
    if(isDead()) return;
    
    StudentWorld* world = getWorld();
    if(world->isOverlappingWithSocrates(getX(), getY())) {
        world->getSocrates()->takeDamage(DAMAGE);
    }
    else if(getFoodEatenSinceLastDivide() == 3) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new RegularSalmonella(newX, newY, world));
        resetFoodEaten();
    }
    else
        tryToEatFood();
    
    if(getMovementPlanDistance() > 0) {
        decreaseMovementPlanDistance();
        tryToMove();
    }
    else {
        Actor* nearestFood = world->findNearestFood(getX(), getY());
        if(nearestFood != nullptr &&
           world->distance(getX(), getY(), nearestFood->getX(), nearestFood->getY()) <= 128) {
            double newX, newY;
            int dir = getDirectionToActor(nearestFood);
            getPositionInThisDirection(dir, 3, newX, newY);
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
    Socrates* socrates = world->getSocrates();
    
    if(world->isOverlappingWithSocrates(getX(), getY()))
        world->getSocrates()->takeDamage(DAMAGE);
    else if(getFoodEatenSinceLastDivide() == 3) {
        double newX, newY;
        calculateNewBacteriumDistance(newX, newY);
        world->addActor(new EColi(newX, newY, world));
        resetFoodEaten();
    }
    else
        tryToEatFood();
    
    double distToSocrates = getWorld()->distance(getX(), getY(),
                                                 socrates->getX(), socrates->getY());
    if(distToSocrates <= 256) {
        int dirToTry = getDirectionToActor(socrates);
        for(int i=0; i<10; i++) {
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
    getWorld()->getSocrates()->healToAmount(HEAL_AMOUNT);
}

void FlameThrowerGoodie::giveReward()
{
    getWorld()->getSocrates()->refillFlames(REFILL_AMOUNT);
}

void ExtraLifeGoodie::giveReward()
{
    getWorld()->incLives();
}

void Fungus::giveReward()
{
    // There is no reward for a fungus, mwahahaha!
    getWorld()->getSocrates()->takeDamage(DAMAGE);
}
