#include "Actor.h"
#include "GameWorld.h"
#include "StudentWorld.h"

Actor::Actor(int imageID, double startX, double startY, GameWorld* world)
 : GraphObject(imageID, startX, startY)
{
    m_dead = false;
    m_world = world;
}

bool Actor::isDead()
{
    return m_dead;
}

Socrates::Socrates(double startX, double startY, GameWorld* world)
 : Actor(IID_PLAYER, startX, startY, world)
{
    
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
                // move
                break;
            case KEY_PRESS_LEFT:
                // move
                break;
            default:
                break;
        }
        
    }
    
    else {
        // replenish sprays
    }
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
