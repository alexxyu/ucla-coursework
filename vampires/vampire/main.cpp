// vampires.cpp

#include <iostream>
#include <string>
#include <utility>
#include <cstdlib>
#include <cctype>
#include "Vampire.h"
#include "Player.h"
#include "Arena.h"
#include "Game.h"
#include "globals.h"
using namespace std;

///////////////////////////////////////////////////////////////////////////
// main()
///////////////////////////////////////////////////////////////////////////

int main()
{
      // Create a game
      // Use this instead to create a mini-game:   Game g(3, 5, 2);
    Game g(10, 12, 40);

      // Play the game
    g.play();
}
