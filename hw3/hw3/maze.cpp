// maze.cpp

#include <iostream>
#include <string>
using namespace std;

const char VISITED_MARKER = '#';

bool pathExists(string maze[], int nRows, int nCols, int sr, int sc, int er, int ec)
{
    // If the start location is equal to the ending location, then we've
    //     solved the maze, so return true.
    if(sr == er && sc == ec)
        return true;
    
    // Mark the start location as visted.
    maze[sr][sc] = VISITED_MARKER;
    
    // For each of the four directions,
    // If the location one step in that direction (from the start
    // location) is unvisited,
    //    then call pathExists starting from that location (and
    //        ending at the same ending location as in the
    //        current call).
    //     If that returned true,
    //         then return true.
    
    if(maze[sr][sc+1] != VISITED_MARKER && maze[sr][sc+1] != 'X')
        if(pathExists(maze, nRows, nCols, sr, sc+1, er, ec))
            return true;
    
    if(maze[sr+1][sc] != VISITED_MARKER && maze[sr+1][sc] != 'X')
        if(pathExists(maze, nRows, nCols, sr+1, sc, er, ec))
            return true;
    
    if(maze[sr][sc-1] != VISITED_MARKER && maze[sr][sc-1] != 'X')
        if(pathExists(maze, nRows, nCols, sr, sc-1, er, ec))
            return true;
    
    if(maze[sr-1][sc] != VISITED_MARKER && maze[sr-1][sc] != 'X')
        if(pathExists(maze, nRows, nCols, sr-1, sc, er, ec))
            return true;
    
    return false;
}

int main()
{
    string maze[10] = {
        "XXXXXXXXXX",
        "X...X..X.X",
        "X.XXX....X",
        "X.X.XXXX.X",
        "XXX......X",
        "X...X.XX.X",
        "X.X.X..X.X",
        "X.XXXX.X.X",
        "X..X...X.X",
        "XXXXXXXXXX"
    };

    if (pathExists(maze, 10,10, 1,4, 1,8))
        cout << "Solvable!" << endl;
    else
        cout << "Out of luck!" << endl;
}

