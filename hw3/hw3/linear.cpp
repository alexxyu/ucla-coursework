// linear.cpp

#include <iostream>
#include <cassert>
using namespace std;

bool somePredicate(double x);

// Return true if the somePredicate function returns false for at
// least one of the array elements; return false otherwise.
bool anyFalse(const double a[], int n)
{
    if(n<=0) return false;
    return !somePredicate(a[0]) || anyFalse(a+1, n-1);
}

// Return the number of elements in the array for which the
// somePredicate function returns true.
int countTrue(const double a[], int n)
{
    if(n<=0) return 0;
    if(somePredicate(a[0]))
        return 1 + countTrue(a+1, n-1);
    return countTrue(a+1, n-1);
}

// Return the subscript of the first element in the array for which
// the somePredicate function returns true.  If there is no such
// element, return -1.
int firstTrue(const double a[], int n)
{
    if(n<=0) return -1;
    if(somePredicate(a[0]))
        return 0;
    int index = 1 + firstTrue(a+1, n-1);
    if(index <= 0)
        return -1;
    return index;
}

// Return the subscript of the smallest element in the array (i.e.,
// return the smallest subscript m such that a[m] <= a[k] for all
// k from 0 to n-1).  If the function is told to examine no
// elements, return -1.
int positionOfSmallest(const double a[], int n)
{
    if(n<=0) return -1;
    
    int restMinIndex = 1 + positionOfSmallest(a+1, n-1);
    if(a[0] <= a[restMinIndex])
        return 0;
    return restMinIndex;
}

// If all n2 elements of a2 appear in the n1 element array a1, in
// the same order (though not necessarily consecutively), then
// return true; otherwise (i.e., if the array a1 does not contain
// a2 as a not-necessarily-contiguous subsequence), return false.
// (Of course, if a2 is empty (i.e., n2 is 0), return true.)
// For example, if a1 is the 7 element array
//    10 50 40 20 50 40 30
// then the function should return true if a2 is
//    50 20 30
// or
//    50 40 40
// and it should return false if a2 is
//    50 30 20
// or
//    10 20 20
bool contains(const double a1[], int n1, const double a2[], int n2)
{
    if(n2 <= 0) return true;
    if(n1 <= 0) return false;
    if(a1[0] == a2[0])
        return contains(a1+1, n1-1, a2+1, n2-1);
      
    return contains(a1+1, n1-1, a2, n2);
}

bool somePredicate(double x)
{
    return x <= 0;
}

int main()
{
    double a[5] = { 1, 1, 5, 6, -3 };
    assert(anyFalse(a, 5));
    assert(firstTrue(a, 4) == -1);
    assert(firstTrue(a, 5) == 4);
    assert(countTrue(a, 5) == 1);
    assert(countTrue(a, 4) == 0);
    assert(positionOfSmallest(a, 5) == 4);
    
    double a1[7] = { 10, 50, 40, 20, 50, 40, 30 };
    {
        double a2[3] = { 50, 30, 20 };
        assert(!contains(a1, 7, a2, 3));
    }
    {
        double a2[3] = { 50, 40, 40 };
        assert(contains(a1, 7, a2, 3));
    }
    {
        double a2[3] = { 50, 20, 30 };
        assert(contains(a1, 7, a2, 3));
        assert(contains(a1, 7, a2, 0));
    }
    
    double a2[3] = { 10, 20, 20 };
    assert(!contains(a1, 7, a2, 3));
    assert(anyFalse(a1, 7));
    assert(firstTrue(a1, 7) == -1);
    assert(positionOfSmallest(a1, 7) == 0);
    
    double b[3] = { -2, -2, 5 };
    assert(anyFalse(b, 3));
    assert(!anyFalse(b, 2));
    assert(!anyFalse(b, 0));
    assert(countTrue(b, 3) == 2);
    assert(countTrue(b, 0) == 0);
    assert(firstTrue(b, 3) == 0);
    assert(positionOfSmallest(b, 3) == 0);
    
    double c[7] = { 1, 8, 3, -1, -5, -5, 10 };
    assert(firstTrue(c, 7) == 3);
    assert(positionOfSmallest(c, 7) == 4);
    assert(positionOfSmallest(c, 4) == 3);
    assert(positionOfSmallest(c, 0) == -1);
    
    cout << "Passed all tests" << endl;
}
