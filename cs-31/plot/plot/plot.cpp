//
//  Project 3 -- Plot
//
//  Created by Alex Yu on 10/21/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include "grid.h"
#include <iostream>
#include <cctype>
#include <cassert>
using namespace std;

const int NUM_ROWS = 20;            // number of rows in the grid
const int NUM_COLS = 30;            // number of columns in the grid

const int HORIZ = 0;                // value for a horizontal line
const int VERT = 1;                 // value for a vertical line

const int FG = 0;                   // value for foreground plot
const int BG = 1;                   // value for background plot

const int DISTANCE_ERROR = 101;     // value of error returned by parseDistance function
                                    // note: relies on fact that distance integer has to have less than 3 digits

void plotHorizontalLine(int r, int c, int distance, char ch)
{
    if(distance >= 0)
        for(int n=0; n<=distance && c + n <= NUM_COLS; n++)
            setChar(r, c + n, ch);
}

void plotVerticalLine(int r, int c, int distance, char ch)
{
    if(distance >= 0)
        for(int m=0; m<=distance && r + m <= NUM_ROWS; m++)
            setChar(r + m, c, ch);
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
    if( r < 1 || r > NUM_ROWS || c < 1 || c > NUM_COLS )
        return false;
    
    // check if line can be fully drawn in grid
    if( (dir == HORIZ && distance < 0 && c + distance < 1) ||
        (dir == HORIZ && distance > 0 && c + distance > NUM_COLS) ||
        (dir == VERT && distance < 0 && r + distance < 1) ||
        (dir == VERT && distance > 0 && r + distance > NUM_ROWS) )
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
        // only plots if in foreground mode or if empty space while in background mode
        if( (fgbg == BG && getChar(r, c) == ' ') || (fgbg == FG) )
            setChar(r, c, plotChar);
        
        // adjust the current column and row
        r += rowModifier;
        c += colModifier;
    }
    
    return true;
}

int parseDistance(string commandString, int& index)
{
    if(index >= commandString.size())
        return DISTANCE_ERROR;
        
    // first char in current command can be only a digit or a '-' sign
    char firstChar = commandString[index];
    if(!isdigit(firstChar) && firstChar!= '-')
        return DISTANCE_ERROR;
    
    // adjusts sign of distance
    int sign = 1;
    if(firstChar == '-')
    {
        index++;
        sign = -1;
    }
    
    int distValue = 0;
    
    // parses the first digit
    if(index < commandString.size() && isdigit(commandString[index]))
        distValue += commandString[index++] - '0';
    else return DISTANCE_ERROR;
    
    // parses the second digit if present
    if(index < commandString.size() && isdigit(commandString[index]))
        distValue = distValue * 10 + commandString[index++] - '0';
    
    return distValue * sign;
}

int performCommands(string commandString, char& plotChar, int& mode, int& badPos)
{
    int index = 0;
 
    int currRow = 1;
    int currCol = 1;
    
    // flag for if line cannot be entirely plot;
    // will only raise error if command is syntactically correct
    bool cannotPlot = false;
    
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
                
                // raises error if plot command syntax is incorrect
                if(dist == DISTANCE_ERROR)
                {
                    badPos = index;
                    return 1;
                }
                
                // tracks error if whole line could not be plot
                if(!plotLine(currRow, currCol, dist, (currChar == 'h') ? HORIZ : VERT, plotChar, mode) && !cannotPlot)
                {
                    badPos = commIndex;
                    cannotPlot = true;
                }
                
                if(currChar == 'h') currCol += dist;
                else currRow += dist;
                
                // cerr << "Distance parsed: " << dist << endl;
                // cerr << "Current string index: " << index << endl;
                
                break;
            case 'b':
            case 'f':
                // raises error if character to set is not present or not printable
                if(index >= commandString.size() || !isprint(commandString[index]))
                {
                    badPos = index;
                    return 1;
                }
                mode = (currChar == 'f') ? FG : BG;
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
    
    if(cannotPlot)
        return 2;
    
    return 0;
}

int main()
{
    setSize(NUM_ROWS, NUM_COLS);
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
}
