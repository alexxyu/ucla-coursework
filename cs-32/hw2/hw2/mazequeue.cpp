//  mazequeue.cpp

#include <iostream>
#include <queue>
#include <string>
using namespace std;

const char VISITED_MARKER = '#';
bool pathExists(string maze[], int nRows, int nCols, int sr, int sc, int er, int ec);
bool shouldCheck(string maze[], int nRows, int nCols, int targetRow, int targetCol);

class Coord
{
  public:
    Coord(int rr, int cc) : m_r(rr), m_c(cc) {}
    int r() const { return m_r; }
    int c() const { return m_c; }
  private:
    int m_r;
    int m_c;
};

bool shouldCheck(string maze[], int nRows, int nCols, int targetRow, int targetCol)
{
    // out of bounds of maze
    if(targetRow < 0 || targetCol < 0 || targetRow >= nRows || targetCol >= nCols)
        return false;

    // already visited position, or position is a wall
    if(maze[targetRow][targetCol] == VISITED_MARKER || maze[targetRow][targetCol] == 'X')
        return false;
    
    return true;
}

// Return true if there is a path from (sr,sc) to (er,ec)
// through the maze; return false otherwise
bool pathExists(string maze[], int nRows, int nCols, int sr, int sc, int er, int ec)
{
    // Push the starting coordinate (sr,sc) onto the coordinate queue and
    // update maze[sr][sc] to indicate that the algorithm has encountered
    // it (i.e., set maze[sr][sc] to have a value other than '.').
    
    queue<Coord> coordQueue;
    Coord c(sr, sc);
    coordQueue.push(c);
    maze[sr][sc] = VISITED_MARKER;
    
    while(!coordQueue.empty()) {
        // Pop the top coordinate off the queue. This gives you the current
        //    (r,c) location that your algorithm is exploring.
        Coord curr = coordQueue.front();
        coordQueue.pop();
        
        // If the current (r,c) coordinate is equal to the ending coordinate,
        // then we've solved the maze so return true!
        int currRow = curr.r();
        int currCol = curr.c();
        if(currRow == er && currCol == ec)
            return true;
        
        if(shouldCheck(maze, nRows, nCols, currRow, currCol+1)) {
            // If you can move EAST and haven't encountered that cell yet,
            // then push the coordinate (r,c+1) onto the queue and update
            // maze[r][c+1] to indicate the algorithm has encountered it.
            Coord c(currRow, currCol+1);
            coordQueue.push(c);
            maze[currRow][currCol+1] = VISITED_MARKER;
        }
        
        if(shouldCheck(maze, nRows, nCols, currRow+1, currCol)) {
            // If you can move SOUTH and haven't encountered that cell yet,
            // then push the coordinate (r+1,c) onto the queue and update
            // maze[r+1][c] to indicate the algorithm has encountered it.
            Coord c(currRow+1, currCol);
            coordQueue.push(c);
            maze[currRow+1][currCol] = VISITED_MARKER;
        }
        
        if(shouldCheck(maze, nRows, nCols, currRow, currCol-1)) {
            // If you can move WEST and haven't encountered that cell yet,
            // then push the coordinate (r,c-1) onto the queue and update
            // maze[r][c-1] to indicate the algorithm has encountered it.
            Coord c(currRow, currCol-1);
            coordQueue.push(c);
            maze[currRow][currCol-1] = VISITED_MARKER;
        }
        
        if(shouldCheck(maze, nRows, nCols, currRow-1, currCol)) {
            // If you can move NORTH and haven't encountered that cell yet,
            // then push the coordinate (r-1,c) onto the queue and update
            // maze[r-1][c] to indicate the algorithm has encountered it.
            Coord c(currRow-1, currCol);
            coordQueue.push(c);
            maze[currRow-1][currCol] = VISITED_MARKER;
        }
    }
    
    // no solution
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

    if (pathExists(maze, 10,10, 4,3, 1,8))
        cout << "Solvable!" << endl;
    else
        cout << "Out of luck!" << endl;
}
