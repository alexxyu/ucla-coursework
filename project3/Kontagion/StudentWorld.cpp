#include "Actor.h"
#include "StudentWorld.h"
#include "GameConstants.h"
#include <string>
#include <cassert>
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
    
    int level = getLevel();
    
    /*
    for(int i=0; i<level; i++) {
        // add pits
    }

    int num_food = min(5 * level, 25);
    for(int i=0; i<num_food; i++) {
        // add food
    }
    */
    
    int num_dirt = max(180 - 20 * level, 20);
    for(int i=0; i<num_dirt; i++) {
        int x = VIEW_HEIGHT/2 + randInt(-MAX_OJBECT_DIST_FROM_CENTER, MAX_OJBECT_DIST_FROM_CENTER);
        int y = VIEW_HEIGHT/2 + randInt(-static_cast<int>(sqrt(MAX_OJBECT_DIST_FROM_CENTER*MAX_OJBECT_DIST_FROM_CENTER - (x-VIEW_WIDTH/2)*(x-VIEW_HEIGHT/2))), static_cast<int>(sqrt(MAX_OJBECT_DIST_FROM_CENTER*MAX_OJBECT_DIST_FROM_CENTER - (x-VIEW_WIDTH/2)*(x-VIEW_HEIGHT/2))));
        
        Actor* dirt = new DirtPile(x, y, this);
        actors.push_back(dirt);
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
    updateDisplayText(); // update the score/lives/level text at screen top
               
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
    int chance_fungus = min(510 - level * 10, 200);
    if(randInt(0, chance_fungus-1) == 0) {
        // add fungus
    }
    
    int chance_goodie = min(510 - level * 10, 250);
    if(randInt(0, chance_goodie-1) == 0) {
        // add goodie
    }
}

void StudentWorld::updateDisplayText()
{
    // Score: 004500 Level: 4 Lives: 3 health: 82 Sprays: 16 Flames: 4
    // string text = "Score: " + getScore() + " Level: " << getLevel() << " Lives: " << getLives();
    string text = "";
    setGameStatText(text);
}

void StudentWorld::cleanUp()
{
    int count = 0;
    for(list<Actor*>::iterator iter = actors.begin(); iter != actors.end(); ) {
        delete *iter;
        iter = actors.erase(iter);
        count++;
    }

    actors.clear();
    delete socrates;
}
