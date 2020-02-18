#include "Actor.h"
#include "StudentWorld.h"
using namespace std;

///////////////////////////////////////////////////////////////////////////
//  SOCRATES IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Socrates::Socrates(double startX, double startY, StudentWorld* world)
 : Damageable(IID_PLAYER, startX, startY, 0, world, STARTING_HEALTH)
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
    Actor* overlappingDamageable = getWorld()->findOverlapWithDamageable(getX(), getY());
    if(overlappingDamageable != nullptr) {
        overlappingDamageable->takeDamage(m_damage);
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
    
}

void Pit::doSomething()
{
    
}

///////////////////////////////////////////////////////////////////////////
//  EXPIRABLE IMPLEMENTATION
///////////////////////////////////////////////////////////////////////////

Expirable::Expirable(int imageID, double startX, double startY,
                     StudentWorld* world, int pointValue, bool playSoundOnTouch)
 : Damageable(imageID, startX, startY, 1, world, 0)
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
