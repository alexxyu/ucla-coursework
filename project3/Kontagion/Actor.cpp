#include "Actor.h"
#include "GameWorld.h"
#include "StudentWorld.h"
#include <cmath>

// // // // // // // // // // // // // //
//       SOCRATES IMPLEMENTATION       //
// // // // // // // // // // // // // //

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
    
    else {
        // replenish sprays
    }
}

void Socrates::adjustPosition(int degree)
{
    const double PI = 4 * atan(1);
    
    int newAngle = (getDirection() + 180 + degree) % 360;
    double newX = VIEW_RADIUS * cos(newAngle * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
    double newY = VIEW_RADIUS * sin(newAngle * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
    
    moveTo(newX, newY);
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

void Projectile::doSomething()
{
    if(isDead())
        return;
    
    // check overlap with damageable object
    
    moveForward(SPRITE_RADIUS*2);
    
    // check if dissipated
    if(getWorld()->distance(getX(), getY(), m_startX, m_startY) >= m_maxDistance)
        setDead();
}

// // // // // // // // // // // // // //
//         PIT IMPLEMENTATION          //
// // // // // // // // // // // // // //

Pit::Pit(double startX, double startY, StudentWorld* world)
 : Actor(IID_PIT, startX, startY, 0, 1, world)
{
    
}

void Pit::doSomething()
{
    
}
