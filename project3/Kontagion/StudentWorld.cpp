#include "Actor.h"
#include "StudentWorld.h"
#include "GameConstants.h"
#include <string>
#include <cassert>
#include <sstream>
#include <iomanip>
#include <climits>
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
    
    numEnemies = 0;
    int level = getLevel();
    
    for(int pitCount=0; pitCount < level; ) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER &&
           !isOverlappingWithActors(x, y, static_cast<int>(actors.size()))) {
            addActor(new Pit(x, y, this));
            pitCount++;
        }
    }

    int numFood = min(5 * level, 25);
    for(int foodCount=0; foodCount < numFood; ) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER &&
           !isOverlappingWithActors(x, y, static_cast<int>(actors.size()))) {
            addActor(new Food(x, y, this));
            foodCount++;
        }
    }
    
    int numDirt = max(180 - 20 * level, 20);
    for(int dirtCount=0; dirtCount < numDirt; ) {
        int x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        
        if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) <= MAX_OJBECT_DIST_FROM_CENTER &&
           !isOverlappingWithActors(x, y, numFood + level)) {
            addActor(new DirtPile(x, y, this));
            dirtCount++;
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
    for(Actor* a: actors) {
        if(!(a->isDead())) {
            a->doSomething();
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
        double x, y;
        int dir = randInt(0, 359);
        getRadialPosition(dir, x, y);
        
        actors.push_back(new Fungus(x, y, this));
    }
    
    int chanceGoodie = max(510 - level * 10, 250);
    if(randInt(0, chanceGoodie-1) == 0) {
        
        double x, y;
        int dir = randInt(0, 359);
        getRadialPosition(dir, x, y);
        
        int whichGoodie = randInt(0, 9);
        if(whichGoodie <= 5)
            actors.push_back(new RestoreHealthGoodie(x, y, this));
        else if(whichGoodie == 9)
            actors.push_back(new ExtraLifeGoodie(x, y, this));
        else
            actors.push_back(new FlameThrowerGoodie(x, y, this));
    }
}

void StudentWorld::updateDisplayText()
{
    // Score: 004500  Level: 4  Lives: 3  Health: 82  Sprays: 16  Flames: 4
    ostringstream oss;
    const string textDivider = "  ";
    oss.fill('0');
    
    int score = getScore();
    if (score >= 0)
        oss << "Score: " << setw(6) << getScore() << textDivider;
    else
        oss << "Score: -" << setw(5) << -getScore() << textDivider;
    oss << "Level: " << getLevel() << textDivider;
    oss << "Lives: " << getLives() << textDivider;
    oss << "Health: " << socrates->getHealth() << textDivider;
    oss << "Sprays: " << socrates->getSprayCount() << textDivider;
    oss << "Flames: " << socrates->getFlameCount();
    setGameStatText(oss.str());
}

void StudentWorld::cleanUp()
{
    for(Actor* a: actors) {
        delete a;
    }

    if(socrates != nullptr)
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

Actor* StudentWorld::findNearestFood(double x, double y) const
{
    int closestDistance = INT_MAX;
    Actor* closestFood = nullptr;
    for(Actor* a: actors) {
        if(a->isFood() && distance(x, y, a->getX(), a->getY()) < closestDistance) {
            closestDistance = distance(x, y, a->getX(), a->getY());
            closestFood = a;
        }
    }
    return closestFood;
}

bool StudentWorld::damageDamageable(double x, double y, int damage)
{
    for(Actor* a: actors) {
        if(a->isDamageable() && isOverlapping(x, y, a->getX(), a->getY())) {
            a->takeDamage(damage);
            return true;
        }
    }
    
    return false;
}

bool StudentWorld::isOverlappingWithSocrates(double x, double y) const
{
    return isOverlapping(x, y, socrates->getX(), socrates->getY());
}

bool StudentWorld::isOverlappingWithDirt(double x, double y) const
{
    for(Actor* a: actors)
        if(a->isDirtPile() && isOverlapping(x, y, a->getX(), a->getY()))
            return true;
    return false;
}
 
bool StudentWorld::isOverlappingWithActors(double x, double y, int numToCheck) const
{
    list<Actor*>::const_iterator iter = actors.begin();
    for(int i=0; i<numToCheck && iter!=actors.end(); iter++, i++)
        if(isOverlapping(x, y, (*iter)->getX(), (*iter)->getY()))
            return true;
    
    return false;
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
