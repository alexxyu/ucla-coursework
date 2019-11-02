//
//  array.cpp
//
//  Created by Alex Yu on 11/1/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include <iostream>
using namespace std;

int appendToAll(string a[], int n, string value);
int lookup(const string a[], int n, string target);
int positionOfMax(const string a[], int n);
int rotateLeft(string a[], int n, int pos);
int countRuns(const string a[], int n);
int flip(string a[], int n);
int differ(const string a1[], int n1, const string a2[], int n2);
int subsequence(const string a1[], int n1, const string a2[], int n2);
int lookupAny(const string a1[], int n1, const string a2[], int n2);
// int separate(string a[], int n, string separator); DOES NOT WORK

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
    string max;
    
    for(int i=0; i<n; i++)
    {
        int j;
        for(j=0; j<n; j++)
            if(a[i] < a[j])
                break;
        if(j == n) return i;
    }
    
    return -1;
}

int rotateLeft(string a[], int n, int pos)
{
    if(n < 0 || pos < 0 || pos >= n) return -1;
    
    string removed = a[pos];
    for(int i=pos+1; i<n; i++)
        a[i-1] = a[i];
    
    a[n-1] = removed;
    return pos;
    
}

int countRuns(const string a[], int n)
{
    if(n < 0) return -1;
    if(n == 0) return 0;
    
    int runs = 1;
    for(int i=1; i<n; i++)
        if(a[i] != a[i-1])
            runs++;
    
    return runs;
}

int flip(string a[], int n)
{
    if(n < 0) return -1;
    
    int lo=0, hi=n-1;
    while(lo < hi)
    {
        string temp = a[lo];
        a[lo] = a[hi];
        a[hi] = temp;
        lo++;
        hi--;
    }
    
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
    
    return (n1 < n2) ? n1 : n2;
}

int subsequence(const string a1[], int n1, const string a2[], int n2)
{
    if(n1 < 0 || n2 < 0) return -1;
    if(n2 == 0) return 0;
    
    int a2Pos = 0;
    for(int i=0; i<n1; i++)
    {
        if(a2Pos == n2) return i - n2;
        else if(a1[i] == a2[a2Pos]) a2Pos++;
        else a2Pos = 0;
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
    bool hasSep = false;
    while(lo < hi)
    {
        if(a[lo] > separator)
        {
            string temp = a[lo];
            a[lo] = a[hi];
            a[hi] = temp;
            hi--;
        }
        else if(a[lo] == separator) hasSep = true;
        lo++;
    }
    
    if(hasSep)
    {
        for(int i=0; i<n; i++)
        {
            if(a[i] == separator)
            {
                a[i] = a[lo];
                a[lo] = separator;
            }
        }
    }
    
    return lo;
}

int main()
{
    
    string officeholders[5] = { "donald", "lindsey", "mike", "adam", "nancy" };
    
    assert(lookup(officeholders, 5, "adam") == 3);
    assert(lookup(officeholders, 5, "joey") == -1);
    assert(lookup(officeholders, 5, "donald") == 0);
    assert(lookup(officeholders, 5, "") == -1);
    
    flip(officeholders, 5);
    
    string politician[5] = { "mike", "donald", "lindsey", "nancy", "adam" };
    assert(rotateLeft(politician, 5, 1) == 1);
    // for(string s: politician)
    //     cerr << s << endl;
    
    string d[9] = {
        "rudy", "adam", "mike", "mike", "fiona", "fiona", "fiona", "mike", "mike"
    };
    assert(countRuns(d, 9) == 5);
    
    // string persons[6] = { "donald", "lindsey", "marie", "rudy", "fiona", "adam" };
    // assert(positionOfMax(persons, 6) == 3);
    
    string nothing[0];
    assert(lookup(nothing, 0, "hi") == -1);
    
    string folks[7] = { "adam", "", "fiona", "mike", "rudy", "nancy", "donald" };
    string group[6] = { "adam", "", "fiona", "donald", "mike", "rudy" };
    assert(differ(folks, 7, group, 6) == 3);
    assert(differ(folks, 2, group, 3) == 2);
    
    string names[10] = { "gordon", "marie", "nancy", "mick", "adam", "lindsey" };
    string names1[10] = { "marie", "nancy", "mick" };
    assert(subsequence(names, 6, names1, 3) == 1);
    string names2[10] = { "gordon", "mick" };
    assert(subsequence(names, 5, names2, 2) == -1);
    
    string set1[10] = { "donald", "adam", "mick", "marie" };
    assert(lookupAny(names, 6, set1, 4) == 1);
    string set2[10] = { "rudy", "fiona" };
    assert(lookupAny(names, 6, set2, 2) == -1);
    
    string persons[6] = { "donald", "lindsey", "marie", "rudy", "fiona", "adam" };
    assert(separate(persons, 6, "gordon") == 3);
    //for(string s: persons)
    //    cerr << s << endl;
    
    string persons2[4] = { "marie", "nancy", "lindsey", "mike" };
    assert(separate(persons2, 4, "mike") == 2);
    for(string s: persons2)
        cerr << s << endl;
    
    return 0;
    
}

