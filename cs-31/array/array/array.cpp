//
//  Project 4 - Array
//
//  Created by Alex Yu on 11/1/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include <iostream>
#include <string>
#include <cassert>
using namespace std;

void swap(string a[], int pos1, int pos2);
int appendToAll(string a[], int n, string value);
int lookup(const string a[], int n, string target);
int positionOfMax(const string a[], int n);
int rotateLeft(string a[], int n, int pos);
int countRuns(const string a[], int n);
int flip(string a[], int n);
int differ(const string a1[], int n1, const string a2[], int n2);
int subsequence(const string a1[], int n1, const string a2[], int n2);
int lookupAny(const string a1[], int n1, const string a2[], int n2);
int separate(string a[], int n, string separator);

void swap(string a[], int pos1, int pos2)
{
    string temp = a[pos1];
    a[pos1] = a[pos2];
    a[pos2] = temp;
}

int appendToAll(string a[], int n, string value)
{
    if(n < 0) return -1;
    
    for(int i=0; i<n; i++)
        a[i] = a[i] + value;
    return n;
}

int lookup(const string a[], int n, string target)
{
    for(int i=0; i<n; i++)
        if(a[i] == target)
            return i;
    return -1;
}

int positionOfMax(const string a[], int n)
{
    if(n <= 0) return -1;
    
    int maxPos = 0;
    string max = a[0];
    
    for(int i=1; i<n; i++)
    {
        if(a[i] > max)            // saves position of current max
        {
            maxPos = i;
            max = a[i];
        }
    }
    
    return maxPos;
}

int rotateLeft(string a[], int n, int pos)
{
    if(n < 0 || pos < 0 || pos >= n) return -1;
    
    string removed = a[pos];
    for(int i=pos+1; i<n; i++)
        a[i-1] = a[i];           // shift each element after removed one position to the left
    
    a[n-1] = removed;
    return pos;
}

int countRuns(const string a[], int n)
{
    if(n < 0) return -1;
    if(n == 0) return 0;
    
    int runs = 1;
    for(int i=1; i<n; i++)
        if(a[i] != a[i-1])      // new run for every element different from last
            runs++;
    
    return runs;
}

int flip(string a[], int n)
{
    if(n < 0) return -1;
    
    int lo=0, hi=n-1;
    while(lo < hi)
        swap(a, lo++, hi--);    // swap lo index and hi index each turn and close in on center
    
    return n;
}

int differ(const string a1[], int n1, const string a2[], int n2)
{
    if(n1 < 0 || n2 < 0) return -1;
    
    int index = 0;
    while(index < n1 && index < n2)
    {
        if(a1[index] != a2[index])
            return index;
        index++;
    }
    
    return (n1 < n2) ? n1 : n2;     // return smaller of two if elements found to be same
}

int subsequence(const string a1[], int n1, const string a2[], int n2)
{
    if(n1 < 0 || n2 < 0 || n2 > n1) return -1;
    if(n2 == 0) return 0;                       // empty sequence always a subsequence
    
    int a2Pos = 0;
    for(int i=0; i<n1; i++)
    {
        if(a2Pos == n2) return i - n2;          // return start index if subsequence found
        else if(a1[i] == a2[a2Pos]) a2Pos++;    // increment index of subsequence to check
        else a2Pos = 0;                         // reset index because subsequence failed to match
    }
    
    return -1;
}

int lookupAny(const string a1[], int n1, const string a2[], int n2)
{
    if(n1 < 0 || n2 < 0) return -1;

    for(int i=0; i<n1; i++)
        for(int j=0; j<n2; j++)
            if(a1[i] == a2[j])
                return i;

    return -1;
}

int separate(string a[], int n, string separator)
{
    if(n < 0) return -1;

    int lo=0, hi=n-1;
    while(lo < hi)
    {
        // get next elements that are not in correct partition of array
        while(a[lo] < separator) lo++;
        while(a[hi] > separator) hi--;
        
        // swap these incorrectly placed elements
        if(lo < hi)
            swap(a, lo, hi);
    }
    
    // lo will be the index of the first element >= separator, or n if element does not exist
    return lo;
}

int main()
{
    string h[7] = { "mick", "marie", "fiona", "rudy", "", "gordon", "lindsey" };
    assert(lookup(h, 7, "gordon") == 5);
    assert(lookup(h, 7, "fiona") == 2);
    assert(lookup(h, 2, "fiona") == -1);
    assert(positionOfMax(h, 7) == 3);

    string g[4] = { "mick", "marie", "lindsey", "nancy" };
    assert(differ(h, 4, g, 4) == 2);
    assert(appendToAll(g, 4, "?") == 4 && g[0] == "mick?" && g[3] == "nancy?");
    assert(rotateLeft(g, 4, 1) == 1 && g[1] == "lindsey?" && g[3] == "marie?");

    string e[4] = { "fiona", "rudy", "", "gordon" };
    assert(subsequence(h, 7, e, 4) == 2);
    assert(subsequence(g, 4, h, 7) == -1);

    string d[5] = { "marie", "marie", "marie", "rudy", "rudy" };
    assert(countRuns(d, 5) == 2);
    
    string f[3] = { "lindsey", "fiona", "mike" };
    assert(lookupAny(h, 7, f, 3) == 2);
    assert(lookupAny(h, 0, f, 3) == -1);
    assert(flip(f, 3) == 3 && f[0] == "mike" && f[2] == "lindsey");
    
    assert(separate(h, 7, "lindsey") == 3);
    assert(separate(f, 3, "lindsey") == 1);
    
    cerr << "All tests succeeded." << endl;
}
