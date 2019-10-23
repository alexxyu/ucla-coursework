//
//  Plot project
//
//  Created by Alex Yu on 10/21/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include "grid.h"
#include <iostream>
#include <cctype>
#include <cassert>
using namespace std;

const int NUM_ROWS = 20;
const int NUM_COLS = 30;

const int HORIZ = 0;
const int VERT = 1;

const int FG = 0;
const int BG = 1;

const int DISTANCE_ERROR = 101;

void plotHorizontalLine(int r, int c, int distance, char ch)
{
    if(distance >= 0)
    {
        int cols = getCols();
        for(int n=0; n<=distance && c + n <= cols; n++)
        {
            setChar(r, c + n, ch);
        }
    }
}

void plotVerticalLine(int r, int c, int distance, char ch)
{
    if(distance >= 0)
    {
        int rows = getRows();
        for(int m=0; m<=distance && r + m <= rows; m++)
        {
            setChar(r + m, c, ch);
        }
    }
}

void plotRectangle(int r, int c, int height, int width, char ch)
{
    plotHorizontalLine(r, c, width, ch);            // upper-left to upper-right side
    plotHorizontalLine(r + height, c, width, ch);   // lower-left to lower-right side
    plotVerticalLine(r, c, height, ch);             // upper-left to lower-left side
    plotVerticalLine(r, c + width, height, ch);     // upper-right to lower-right side
}

bool canPlotLine(int r, int c, int distance, int dir, char plotChar, int fgbg)
{
    // check if inputs are possible
    if( (dir != HORIZ && dir != VERT) ||
        (fgbg != FG && fgbg != BG) ||
        (!isprint(plotChar)) )
        return false;
    
    // check if starting point is valid in grid
    if( (r < 1 || r > getRows()) ||
        (c < 1 || c > getCols()) )
        return false;
    
    // check if line can be fully drawn in grid
    if( (dir == HORIZ && distance < 0 && c + distance < 1) ||
        (dir == HORIZ && distance > 0 && c + distance > getCols()) ||
        (dir == VERT && distance < 0 && r + distance < 1) ||
        (dir == VERT && distance > 0 && r + distance > getRows()) )
        return false;
    
    return true;
}

bool plotLine(int r, int c, int distance, int dir, char plotChar, int fgbg)
{
    // check if function can successfully draw a line in the grid
    if(!canPlotLine(r, c, distance, dir, plotChar, fgbg))
        return false;
    
    // finds which direction the line will be drawn (up, down, left, right)
    int dirModifier = (distance < 0) ? -1 : 1;
    int colModifier = (dir == HORIZ) ? dirModifier : 0;
    int rowModifier = (dir == VERT) ? dirModifier : 0;
    
    // iterate through each point in the line
    int absDistance = abs(distance);
    for(int n=0; n<=absDistance; n++)
    {
        if( (fgbg == BG && getChar(r, c) == ' ') ||
            (fgbg == FG) )
            setChar(r, c, plotChar);
        
        r += rowModifier;
        c += colModifier;
        
        /*
        if(dir == HORIZ)
        {
            // find the current column, which depends on the sign of the distance
            int currCol = c + n;
            if(distance < 0)
                currCol = c - n;
            
            // only draw in background if empty character at current position
            if(fgbg == BG && getChar(r, currCol) == ' ')
                setChar(r, currCol, plotChar);
            else if(fgbg == FG)
                setChar(r, currCol, plotChar);
        }
        else
        {
            // find the current row, which depends on the sign of the distance
            int currRow = r + n;
            if(distance < 0)
                currRow = r - n;
            
            // only draw in background if empty character at current position
            if(fgbg == BG && getChar(currRow, c) == ' ')
                setChar(currRow, c, plotChar);
            else if(fgbg == FG)
                setChar(currRow, c, plotChar);
        }
        */
    }
    
    return true;
    
}

int parseDistance(string commandString, int& index)
{
    if(index >= commandString.size())
        return DISTANCE_ERROR;
        
    char firstChar = commandString[index];
    if(!isdigit(firstChar) && firstChar!= '-')
        return DISTANCE_ERROR;
    
    int sign = 1;
    if(firstChar == '-')
    {
        index++;
        sign = -1;
    }
    
    int distValue = 0;
    
    if(index < commandString.size() && isdigit(commandString[index]))
        distValue += commandString[index++] - '0';
    else return DISTANCE_ERROR;
    
    if(index < commandString.size() && isdigit(commandString[index]))
        distValue = distValue * 10 + commandString[index++] - '0';
    
    return distValue * sign;
}

int performCommands(string commandString, char& plotChar, int& mode, int& badPos)
{
    int index = 0;
 
    int currRow = 1;
    int currCol = 1;
    while(index < commandString.size())
    {
        int dist;
        
        // index of the current command being parsed in the string
        int commIndex = index;
        
        char currChar = tolower(commandString[index++]);
        switch(currChar)
        {
            case 'h':
            case 'v':
                dist = parseDistance(commandString, index);
                if(dist == DISTANCE_ERROR)
                {
                    badPos = index;
                    return 1;
                }
                
                if(!plotLine(currRow, currCol, dist, (currChar == 'h') ? HORIZ : VERT, plotChar, mode))
                {
                    badPos = commIndex;
                    return 2;
                }
                
                if(currChar == 'h') currCol += dist;
                else currRow += dist;
                
                // cout << "Distance parsed: " << dist << endl;
                // cout << "Current string index: " << index << endl;
                
                break;
            case 'b':
            case 'f':
                mode = (currChar == 'f') ? FG : BG;
                if(index >= commandString.size() || !isprint(commandString[index]))
                {
                    badPos = index;
                    return 1;
                }
                plotChar = commandString[index++];
                break;
            case 'c':
                clearGrid();
                plotChar = '*';
                mode = FG;
                break;
            default:
                badPos = commIndex;
                return 1;
        }
        
    }
    
    return 0;
    
}

int main()
{
    setSize(NUM_ROWS, NUM_COLS);

    clearGrid();
    char currentChar = '*';
    int currentMode = FG;
    for (;;)
    {
        cout << "Enter a command string: ";
        string cmd;
        getline(cin, cmd);
        if (cmd == "")
            break;
        int position;
        int status = performCommands(cmd, currentChar, currentMode, position);
        switch (status)
        {
          case 0:
            draw();
            break;
          case 1:
                cout << "Syntax error at position " << position+1 << endl;
            break;
          case 2:
                cout << "Cannot perform command at position " << position+1 << endl;
            break;
          default:
              // It should be impossible to get here.
                cerr << "performCommands returned " << status << "!" << endl;
        }
    }
    
    /*
    const int middle = getCols() / 2;
    setChar(6, middle, 'E');
    setChar(8, middle, 'L');
    setChar(9, middle, 'O');
    setChar(7, middle, 'L');
    setChar(5, middle, 'H');
    if (getChar(6, middle) == 'E')
        setChar(10, middle, '!');
    // plotRectangle(3, 3, 0, 4, '*');
    
    assert(plotLine(8, 3, 12, HORIZ, '*', BG));
    draw();
     */
    
    /*
    setSize(20, 20);
    clearGrid();
    assert(plotLine(1, 1, 0, HORIZ, 'H', FG));
    assert(plotLine(1, 2, 0, HORIZ, 'i', FG));
    assert(plotLine(1, 3, 0, HORIZ, '!', FG));
    draw();  //  displays  Hi!  in the top row of the grid
    assert(plotLine(1, 3, 0, HORIZ, ' ', FG));
    draw();  //  displays  Hi   in the top row of the grid
    assert(plotLine(1, 1, 10, HORIZ, ' ', BG));
    draw();  //  displays  Hi   in the top row of the grid
    assert( ! plotLine(1, 1, 10, HORIZ, '\n', FG));
    draw();  //  displays  Hi   in the top row of the grid
    */
}
