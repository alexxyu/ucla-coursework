//
//  Plot project
//
//  Created by Alex Yu on 10/21/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include "grid.h"
#include <cctype>
#include <iostream>
using namespace std;

const int HORIZ = 0;
const int VERT = 1;

const int FG = 0;
const int BG = 1;

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

bool plotLine(int r, int c, int distance, int dir, char plotChar, int fgbg)
{
    // check conditions for if line can be drawn on grid
    if( (dir != HORIZ && dir != VERT) ||
        (fgbg != FG && fgbg != BG) ||
        (!isprint(plotChar)) ||
        (dir == HORIZ && c + distance > getCols()) ||
        (dir == VERT && r + distance > getRows()) )
        return false;
    
    // iterate through the distances in the line
    for(int n=0; n<=distance; n++)
    {
        if(dir == HORIZ)
        {
            // only draw in background if empty character at current position
            if(fgbg == BG && getChar(r, c + n) == ' ')
                setChar(r, c + n, plotChar);
            else if(fgbg == FG)
                setChar(r, c + n, plotChar);
        }
        else
        {
            // only draw in background if empty character at current position
            if(fgbg == BG && getChar(r + n, c) == ' ')
                setChar(r + n, c, plotChar);
            else if(fgbg == FG)
                setChar(r + n, c, plotChar);
        }
    }
    
    return true;
    
}

int main()
{
    setSize(15, 12);

    const int middle = getCols() / 2;
    setChar(6, middle, 'E');
    setChar(8, middle, 'L');
    setChar(9, middle, 'O');
    setChar(7, middle, 'L');
    setChar(5, middle, 'H');
    if (getChar(6, middle) == 'E')
        setChar(10, middle, '!');
    // plotRectangle(3, 3, 0, 4, '*');
    
    bool canDraw = plotLine(4, 6, 10, VERT, '*', 0);
    cout << canDraw << endl;
    draw();
}

