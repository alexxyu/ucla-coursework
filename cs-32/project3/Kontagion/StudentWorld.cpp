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
    m_socrates = new Socrates(0, VIEW_HEIGHT/2, this);
    
    m_numEnemies = 0;
    int level = getLevel();
    
    // generate pits
    int numPits = level;
    for(int pitCount=0; pitCount < numPits; ) {
        double x, y;
        getRandomPointInDish(x, y);
        
        if(canAddToWorld(x, y, static_cast<int>(m_actors.size()))) {
            addActor(new Pit(x, y, this));
            pitCount++;
        }
    }

    // generate food
    int numFood = min(5 * level, 25);
    for(int foodCount=0; foodCount < numFood; ) {
        double x, y;
        getRandomPointInDish(x, y);
        
        if(canAddToWorld(x, y, static_cast<int>(m_actors.size()))) {
            addActor(new Food(x, y, this));
            foodCount++;
        }
    }
    
    // generate dirt piles
    int numDirt = max(180 - 20 * level, 20);
    for(int dirtCount=0; dirtCount < numDirt; ) {
        double x, y;
        getRandomPointInDish(x, y);
        
        if(canAddToWorld(x, y, numFood+numPits)) {
            addActor(new DirtPile(x, y, this));
            dirtCount++;
        }
    }
    
    return GWSTATUS_CONTINUE_GAME;
}

int StudentWorld::move()
{
    m_socrates->doSomething();

    // give each actor a chance to do something, including Socrates
    for(Actor* a: m_actors) {
        if(!(a->isDead())) {
            a->doSomething();
        }
        
        if(m_socrates->isDead()) {
            decLives();
            playSound(SOUND_PLAYER_DIE);
            return GWSTATUS_PLAYER_DIED;
        }
        
        if(m_numEnemies == 0) {
            playSound(SOUND_FINISHED_LEVEL);
            return GWSTATUS_FINISHED_LEVEL;
        }
    }
    
    // remove newly-dead actors after each tick
    removeDeadGameObjects();
    
    // potentially add new actors to the game (e.g., goodies or fungi)
    addNewActors();
    
    // update the game status text line
    updateDisplayText();
               
    // the player hasn’t completed the current level and hasn’t died, so
    // continue playing the current level
    return GWSTATUS_CONTINUE_GAME;
}

void StudentWorld::removeDeadGameObjects()
{
    for(list<Actor*>::iterator iter = m_actors.begin(); iter != m_actors.end(); )
    {
        if((*iter)->isDead()) {
            delete *iter;
            iter = m_actors.erase(iter);
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
        
        m_actors.push_back(new Fungus(x, y, this));
    }
    
    int chanceGoodie = max(510 - level * 10, 250);
    if(randInt(0, chanceGoodie-1) == 0) {
        double x, y;
        int dir = randInt(0, 359);
        getRadialPosition(dir, x, y);
        
        int whichGoodie = randInt(0, 9);
        if(whichGoodie <= 5)
            m_actors.push_back(new RestoreHealthGoodie(x, y, this));
        else if(whichGoodie == 9)
            m_actors.push_back(new ExtraLifeGoodie(x, y, this));
        else
            m_actors.push_back(new FlameThrowerGoodie(x, y, this));
    }
}

void StudentWorld::updateDisplayText()
{
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
    oss << "Health: " << m_socrates->getHealth() << textDivider;
    oss << "Sprays: " << m_socrates->getSprayCount() << textDivider;
    oss << "Flames: " << m_socrates->getFlameCount();
    setGameStatText(oss.str());
}

void StudentWorld::cleanUp()
{
    for(Actor* a: m_actors) {
        delete a;
    }

    if(m_socrates != nullptr) {
        delete m_socrates;
        m_socrates = nullptr;
    
    }
    
    m_actors.clear();
}

bool StudentWorld::canAddToWorld(double x, double y, int numToCheck) const
{
    if(distance(x, y, VIEW_WIDTH/2, VIEW_HEIGHT/2) > MAX_OJBECT_DIST_FROM_CENTER)
        return false;
    
    list<Actor*>::const_iterator iter = m_actors.begin();
    for(int i=0; i<numToCheck && iter!=m_actors.end(); iter++, i++)
        if(isOverlapping(x, y, (*iter)->getX(), (*iter)->getY()))
            return false;
    
    return true;
}

void StudentWorld::addActor(Actor *actor)
{
    m_actors.push_back(actor);
}

bool StudentWorld::isOverlappingWithSocrates(double x, double y) const
{
    return isOverlapping(x, y, m_socrates->getX(), m_socrates->getY());
}

bool StudentWorld::isOverlappingWithDirt(double x, double y) const
{
    for(Actor* a: m_actors)
        if(a->isDirtPile() && distance(x, y, a->getX(), a->getY()) <= SPRITE_WIDTH/2)
            return true;
    return false;
}

void StudentWorld::getRadialPosition(int dir, double &dx, double &dy) const
{
    const double PI = 4 * atan(1);
    
    dx = VIEW_RADIUS * cos(dir * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
    dy = VIEW_RADIUS * sin(dir * 1.0 / 360 * 2 * PI) + VIEW_RADIUS;
}

double StudentWorld::distance(double x1, double y1, double x2, double y2) const
{
    return sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));
}

bool StudentWorld::findAndEatOverlappingFood(double x, double y)
{
    for(Actor* a: m_actors) {
        if(a->isFood() && isOverlapping(x, y, a->getX(), a->getY())) {
            a->setDead();
            return true;
        }
    }
    
    return false;
}

bool StudentWorld::directionToNearestFoodIfWithinDistance(double x, double y, double dist, int& dir)
{
    int closestDistance = INT_MAX;
    Actor* closestFood = nullptr;
    for(Actor* a: m_actors) {
        if(a->isFood()) {
            double currDistance = distance(x, y, a->getX(), a->getY());
            if(currDistance < closestDistance && currDistance <= dist) {
                closestDistance = currDistance;
                closestFood = a;
            }
        }
    }
    
    if(closestFood != nullptr) {
        dir = getDirectionToActor(closestFood, x, y);
        return true;
    }
    
    return false;
}

bool StudentWorld::directionToSocratesIfWithinDistance(double x, double y, double dist, int &dir)
{
    double distToSocrates = distance(x, y, m_socrates->getX(), m_socrates->getY());
    if(distToSocrates < dist) {
        dir = getDirectionToActor(m_socrates, x, y);
        return true;
    }
    
    return false;
}

bool StudentWorld::damageDamageable(double x, double y, int damage)
{
    for(Actor* a: m_actors) {
        if(a->isDamageable() && isOverlapping(x, y, a->getX(), a->getY()) && !(a->isDead())) {
            static_cast<Damageable*>(a)->takeDamage(damage);
            return true;
        }
    }
    
    return false;
}

void StudentWorld::damageSocrates(int damage)
{
    m_socrates->takeDamage(damage);
}

void StudentWorld::healSocrates(int amount)
{
    m_socrates->healToAmount(amount);
}

void StudentWorld::refillSocratesFlames(int amount)
{
    m_socrates->refillFlames(amount);
}

int StudentWorld::getDirectionToActor(Actor *actor, double x, double y) const
{
    double dir = atan2(actor->getY() - y, actor->getX() - x);
    const double PI = atan(1) * 4;
    dir = dir / PI * 180;
    return dir;
}

bool StudentWorld::isOverlapping(double x1, double y1, double x2, double y2) const
{
    return (distance(x1, y1, x2, y2) <= SPRITE_WIDTH);
}

void StudentWorld::getRandomPointInDish(double &x, double &y)
{
    x = VIEW_WIDTH/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
    y = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
}

void StudentWorld::addNumberOfBacteria(int amount) { m_numEnemies += amount; }
void StudentWorld::decrementNumberOfBacteria() { m_numEnemies--; }
