#include "Actor.h"
#include "GameWorld.h"
#include "StudentWorld.h"
#include <cmath>

// // // // // // // // // // // // // //
//        ACTOR IMPLEMENTATION         //
// // // // // // // // // // // // // //

Actor::Actor(int imageID, double startX, double startY, int dir, int depth, GameWorld* world)
 : GraphObject(imageID, startX, startY, dir, depth)
{
    m_dead = false;
    m_world = world;
}

bool Actor::isDead()
{
    return m_dead;
}

GameWorld* Actor::getWorld()
{
    return m_world;
}

// // // // // // // // // // // // // //
//       SOCRATES IMPLEMENTATION       //
// // // // // // // // // // // // // //

Socrates::Socrates(double startX, double startY, GameWorld* world)
 : Actor(IID_PLAYER, startX, startY, 0, 0, world)
{
    m_spray_count = STARTING_SPRAY_CHARGES;
    m_flame_count = STARTING_FLAME_CHARGES;
    m_health = STARTING_HEALTH;
}

void Socrates::doSomething()
{
    int dir;
    GameWorld* world = getWorld();
    if(world->getKey(dir)) {
        
        switch(dir) {
            case KEY_PRESS_SPACE:
                if(m_spray_count > 0) {
                    // add spray object
                    m_spray_count--;
                    world->playSound(SOUND_PLAYER_SPRAY);
                }
                break;
            case KEY_PRESS_ENTER:
                if(m_flame_count > 0) {
                    // add flame object
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
    double newX = VIEW_DIAMETER * cos(newAngle * 1.0 / 360 * 2 * PI) + VIEW_DIAMETER;
    double newY = VIEW_DIAMETER * sin(newAngle * 1.0 / 360 * 2 * PI) + VIEW_DIAMETER;
    
    moveTo(newX, newY);
    setDirection((newAngle + 180) % 360);
}

int Socrates::getHealth()
{
    return m_health;
}

int Socrates::getSprayCount()
{
    return m_spray_count;
}

int Socrates::getFlameCount()
{
    return m_flame_count;
}

// // // // // // // // // // // // // //
//      DIRTPILE IMPLEMENTATION        //
// // // // // // // // // // // // // //

DirtPile::DirtPile(double startX, double startY, GameWorld* world)
 : Actor(IID_DIRT, startX, startY, 90, 1, world)
{
    
}

void DirtPile::doSomething()
{
    // dirt can't do anything!
    return;
}
