#include "Actor.h"
#include "StudentWorld.h"
#include "GameConstants.h"
#include <string>
#include <cassert>
#include <sstream>
#include <iomanip>
using namespace std;

GameWorld* createStudentWorld(string assetPath)
{
	return new StudentWorld(assetPath);
}

StudentWorld::StudentWorld(string assetPath)
: GameWorld(assetPath)
{
}

StudentWorld::~StudentWorld()
{
    cleanUp();
}

int StudentWorld::init()
{
    // Create new Socrates object
    socrates = new Socrates(0, VIEW_HEIGHT/2, this);
    actors.clear();
    
    int level = getLevel();
    
    int pit_count = 0;
    while(pit_count < level) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER) {
            bool notOverlapping = true;
            for(Actor* actor: actors) {
                if(isOverlapping(x, y, actor->getX(), actor->getY())) {
                    notOverlapping = false;
                    break;
                }
            }
            
            if(notOverlapping) {
                addActor(new Pit(x, y, this));
                pit_count++;
            }
        }
    }

    int num_food = min(5 * level, 25);
    int food_count = 0;
    while(food_count < num_food) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER) {
            bool notOverlapping = true;
            for(Actor* actor: actors) {
                if(isOverlapping(x, y, actor->getX(), actor->getY())) {
                    notOverlapping = false;
                    break;
                }
            }
            
            if(notOverlapping) {
                addActor(new Food(x, y, this));
                food_count++;
            }
        }
    }
    
    int num_dirt = max(180 - 20 * level, 20);
    int dirt_count = 0;
    while(dirt_count < num_dirt) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER) {
            bool notOverlapping = true;
            int i=0;
            for(list<Actor*>::iterator iter = actors.begin(); i<num_food+level; iter++, i++) {
                if(isOverlapping(x, y, (*iter)->getX(), (*iter)->getY())) {
                    notOverlapping = false;
                    break;
                }
            }
            
            if(notOverlapping) {
                addActor(new DirtPile(x, y, this));
                dirt_count++;
            }
        }
    }
    
    return GWSTATUS_CONTINUE_GAME;
}

int StudentWorld::move()
{
    // The term "actors" refers to all bacteria, Socrates, goodies,
    // pits, flames, spray, foods, etc.

    socrates->doSomething();
    
    // Give each actor a chance to do something, incl. Socrates
    for(list<Actor*>::iterator iter = actors.begin(); iter != actors.end(); iter++)
    {
        if(!(*iter)->isDead()) {
            (*iter)->doSomething();
        }
        
        if(socrates->isDead())
            return GWSTATUS_PLAYER_DIED;
        
        // handle whether socrates finished level
    }
    
    // Remove newly-dead actors after each tick
    removeDeadGameObjects();
    
    // Potentially add new actors to the game (e.g., goodies or fungi)
    addNewActors();
    
    // Update the Game Status Line
    updateDisplayText();
               
    // the player hasn’t completed the current level and hasn’t died, so
    // continue playing the current level
    return GWSTATUS_CONTINUE_GAME;
}

void StudentWorld::removeDeadGameObjects()
{
    for(list<Actor*>::iterator iter = actors.begin(); iter != actors.end(); )
    {
        if((*iter)->isDead()) {
            delete *iter;
            iter = actors.erase(iter);
        }
        else
            iter++;
    }
}

void StudentWorld::addNewActors()
{
    int level = getLevel();
    int chanceFungus = max(510 - level * 10, 200);
    if(randInt(0, chanceFungus-1) == 0) {
        // add fungus
    }
    
    int chanceGoodie = max(510 - level * 10, 250);
    if(randInt(0, chanceGoodie-1) == 0) {
        
        double x, y;
        int dir = randInt(0, 359);
        getRadialPosition(dir, x, y);
        
        int whichGoodie = randInt(0, 9);
        Actor* goodie;
        if(whichGoodie <= 5)
            goodie = new RestoreHealthGoodie(x, y, this);
        else if(whichGoodie == 9)
            goodie = new ExtraLifeGoodie(x, y, this);
        else
            goodie = new FlameThrowerGoodie(x, y, this);
        
        actors.push_back(goodie);
    }
}

void StudentWorld::updateDisplayText()
{
    // Score: 004500    Level: 4    Lives: 3    Health: 82    Sprays: 16    Flames: 4
    ostringstream oss;
    oss.fill('0');
    oss << "Score: " << setw(6) << getScore() << "    ";
    oss << "Level: " << getLevel() << "    ";
    oss << "Lives: " << getLives() << "    ";
    oss << "Health: " << socrates->getHealth() << "    ";
    oss << "Sprays: " << socrates->getSprayCount() << "    ";
    oss << "Flames: " << socrates->getFlameCount();
    setGameStatText(oss.str());
}

void StudentWorld::cleanUp()
{
    int count = 0;
    for(list<Actor*>::iterator iter = actors.begin(); iter != actors.end(); ) {
        delete *iter;
        iter = actors.erase(iter);
        count++;
    }

    delete socrates;
}

double StudentWorld::distance(double x1, double y1, double x2, double y2) const
{
    return sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));
}

void StudentWorld::addActor(Actor *actor)
{
    actors.push_back(actor);
}

Actor* StudentWorld::findOverlapWithDamageable(double x, double y) const
{
    for(list<Actor*>::const_iterator iter = actors.begin(); iter != actors.end(); iter++)
        if((*iter)->isDamageable() && isOverlapping(x, y, (*iter)->getX(), (*iter)->getY()))
            return (*iter);
    return nullptr;
}

bool StudentWorld::isOverlappingWithSocrates(double x, double y) const
{
    return isOverlapping(x, y, socrates->getX(), socrates->getY());
}
       
bool StudentWorld::isOverlapping(double x1, double y1, double x2, double y2) const
{
    return (distance(x1, y1, x2, y2) <= 2*SPRITE_RADIUS);
}

void StudentWorld::getRadialPosition(int dir, double &dx, double &dy) const
{
    const double PI = 4 * atan(1);
    
    dx = VIEW_RADIUS * cos(dir * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
    dy = VIEW_RADIUS * sin(dir * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
}
