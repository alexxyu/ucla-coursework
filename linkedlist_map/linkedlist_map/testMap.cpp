// testMap.cpp

#include "Map.h"
#include <iostream>
#include <cassert>
using namespace std;

int main()
{
    Map m;  // maps strings to doubles
    assert(m.empty());
    
    //
    ValueType v = -1234.5;
    assert( !m.get("abc", v)  &&  v == -1234.5); // v unchanged by get failure
    
    // Insert to beginning of map
    m.insert("xyz", 9876.5);
    assert(m.size() == 1);
    
    // Test get function
    KeyType k = "hello";
    assert(m.get(0, k, v)  &&  k == "xyz"  &&  v == 9876.5);
    
    // Insert to end of map
    m.insert("hello", 12);
    
    // Test erasing
    m.erase(k);
    assert(m.size() == 1);
    assert(m.get(0, k, v) && k == "hello" && v == 12);
    
    // Test updating
    m.update("hello", 1234.5);
    assert(m.get("hello", v) && v == 1234.5);
    
    // Test map swap
    Map m1;
    m1.insert("Fred", 2.956);
    Map m2;
    m2.insert("Ethel", 3.538);
    m2.insert("Lucy", 2.956);
    m1.swap(m2);
    assert(m1.size() == 2  &&  m1.contains("Ethel")  &&  m1.contains("Lucy")  &&
           m2.size() == 1  &&  m2.contains("Fred"));
    
    // Test reassign
    Map m3, m4;
    m3.insert("Fred", 123);
    m3.insert("Ethel", 456);
    m3.insert("Lucy", 789);
    m3.insert("Ricky", 321);
    m3.insert("Jee", 158);
    reassign(m3, m4);
    for(int i=0; i<m4.size(); i++) {
        KeyType k1, k2;
        ValueType v1, v2;
        m3.get(i, k1, v1);
        m4.get(i, k2, v2);
        assert(v1 != v2);
    }
    
    // Test combine
    Map m5, m6;
    m5.insert("Alex", 0);
    m5.insert("Fred", 123);
    m5.insert("Jane", 873);
    assert(combine(m3, m5, m6));
    assert(m6.size() == 7);
    
    cout << "Passed all tests" << endl;
}
