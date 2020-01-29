// testMap.cpp

#include "Map.h"
#include <iostream>
#include <cassert>
using namespace std;

int main()
{
    Map m;                                      // Default constructor

    // For empty map:
    assert(m.empty());                          // map size should be 0
    assert(m.size() == 0);                      // map size should be 0
    assert(!m.contains("h"));                   // map should contain nothing
    assert(!m.erase("h"));                      // nothing to erase
    ValueType v = -1234.5;
    assert( !m.get("abc", v)  &&  v == -1234.5);    // nothing to get

    // For map with one element:
    assert(m.insert("xyz", 9876.5));                // insert to beginning of map
    assert(m.size() == 1);                          // size should be 1
    assert(m.get("xyz", v) && v == 9876.5);         // try getting value
    KeyType k = "hello";
    assert(m.get(0, k, v)  &&  k == "xyz"  &&  v == 9876.5);    // try getting value by i
    assert(!m.get(1, k, v) &&  k == "xyz"  &&  v == 9876.5);    // no way to get out of bounds
    assert(!m.insert("xyz", 1));                    // cannot insert key if already exists
    assert(m.contains("xyz"));                      // check that contains works
    
    // Testing erase function:
    k = "hello";
    m.insert(k, 456);
    m.insert("alex", 123);
    assert(m.erase(k));                                     // erasing middle element
    k = "alex";
    assert(m.size() == 2 && m.get(1, k, v) && v == 123);    // test get after erasing
    assert(m.erase(k));                            // erasing (sequentially) last element
    k = "xyz";
    assert(m.erase(k));                            // erasing (sequentially) first element
    assert(m.size() == 0);                         // map should be empty
    assert(m.insert("hello", 12));
    assert(m.get(0, k, v) && k == "hello" && v == 12);      // map properly defined after empty
    // Test updating values in map
    assert(m.update("hello", 1234.5));             // value exists in map
    assert(m.get("hello", v) && v == 1234.5);      // value has been updated
    assert(m.insertOrUpdate("bonjour", 1));
    assert(m.get("bonjour", v) && v == 1);         // key-value has been inserted


    // Test map swap
    Map m1;
    m1.insert("Fred", 2.956);
    Map m2;
    m2.insert("Ethel", 3.538);
    m2.insert("Lucy", 2.956);
    m1.swap(m2);
    assert(m1.size() == 2  &&  m1.contains("Ethel")  &&
               m1.contains("Lucy")  && m2.size() == 1  &&
               m2.contains("Fred"));                // elements have been properly swapped

    // Test reassign with odd-sized map
    Map m3, m4;
    m3.insert("Fred", 123);
    m3.insert("Ethel", 456);
    m3.insert("Lucy", 789);
    m3.insert("Ricky", 321);
    m3.insert("Jee", 158);
    for(int i=0; i<m4.size(); i++) {
            KeyType k1, k2;
            ValueType v1, v2;
            m3.get(i, k1, v1);
            m4.get(i, k2, v2);
            assert(v1 != v2);    // makes sure that values have been changed from before
    }

    // Test combining maps
    Map m5, m6;
    m5.insert("Alex", 0);
    m5.insert("Fred", 123);
    m5.insert("Jane", 873);
    assert(combine(m3, m5, m6));    // combine returns true
    assert(m6.size() == 7);         // 5 elements from m3 + 2 from m5 (one overlapping)

    m5.update("Fred", 999);
    assert(!combine(m3, m5, m6));   // combine returns false (key "Fred" has conflicting values)
    assert(m6.size() == 6);         // "Fred" no longer included in map

    Map empty_map;
    Map another_empty_map;
    assert(combine(empty_map, another_empty_map, m6));    // test combining empty maps
    assert(m6.size() == 0);         // resulting map should be empty

    Map n1, n2;
    n1.insert("a", 1);
    n1.insert("b", 2);
    n2.insert("a", 4);
    assert(!combine(n2, n1, n2));                           // combine should return false
    assert(n2.size() == 1 && n2.get(0, k, v) && k == "b");          // test that combine works when result is one of the maps to combine
    
    // Test operator overloading
    m4 = m3;
    assert(m3.size() == m4.size()); // m4 should contain same element values as m3
    m3.erase("Lucy");
    assert(m3.size() == 4 && m4.size() == 5);    // m4 should be unchanged
    m3 = empty_map;
    assert(m3.size() == 0);                      // m3 should be empty

    // Test copy constructor
    Map m7 = m4;
    assert(m4.size() == m7.size());           // m7 should contain same element values as m4
    m4.erase("Lucy");
    assert(m4.size() == 4 && m7.size() == 5); // m7 should be unchanged
    Map m8 = empty_map;
    assert(m8.size() == 0);                   // m8 should be empty
    
    cout << "Passed all tests" << endl;
}
