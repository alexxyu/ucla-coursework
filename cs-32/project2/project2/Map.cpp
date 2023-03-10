//  Map.cpp

#include "Map.h"
#include <iostream>
using namespace std;

Map::Map()
{
    m_map = nullptr;
    m_size = 0;
}

Map::Map(const Map& other)
{
    m_map = nullptr;
    m_size = 0;
    
    // Deep-copy linked-list from reference map
    for(int i=0; i<other.size(); i++) {
        KeyType k;
        ValueType v;
        other.get(i, k, v);
        insert(k, v);
    }
}

Map& Map::operator=(const Map &rhs)
{
    if(&rhs == this)
        return *this;
    
    Map temp(rhs);
    swap(temp);
    return *this;
}

Map::~Map()
{
    // Traverse through linked list and destruct each node in map
    Node* curr = m_map;
    while(curr != nullptr) {
        Node* next = curr->next;
        delete curr;
        curr = next;
    }
}

bool Map::empty() const
{
    return m_size == 0;
}

int Map::size() const
{
    return m_size;
}

bool Map::insert(const KeyType &key, const ValueType &value)
{
    // No elements currently in map, so map is just one node with this key-value pair
    if(m_map == nullptr) {
        m_map = new Node;
        *m_map = {key, value, nullptr, nullptr};
        m_size++;
        return true;
    }
    
    Node* ptr = m_map;
    if(ptr->m_key == key)
        return false;
    
    // Traverse linked list until end
    while(ptr->next != nullptr) {
        
        // key already exists in map, so key-value pair cannot be inserted
        if(ptr->m_key == key)
            return false;
        
        ptr = ptr->next;
    }
    
    // Insert new node with key-value pair
    Node* newNode = new Node;
    *newNode = {key, value, nullptr, ptr};
    ptr->next = newNode;
    
    m_size++;
    
    return true;
}

bool Map::update(const KeyType& key, const ValueType& value)
{
    Node* ptr = m_map;
    
    // Traverse through linked list until node with key found
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            ptr->m_value = value;
            return true;
        }
        ptr = ptr->next;
    }
    
    // Node with key not found
    return false;
}

bool Map::insertOrUpdate(const KeyType &key, const ValueType &value)
{
    if(update(key, value))
        return true;
    
    return insert(key, value);
}

bool Map::erase(const KeyType &key)
{
    if(m_size < 1) return false;
    
    // Boundary case where first element in linked list is to be removed
    if(m_map->m_key == key) {
        
        // Map is now whatever comes after the first node
        Node* curr = m_map;
        m_map = curr->next;
        delete curr;
        
        if(m_map != nullptr)
            m_map->previous = nullptr;
        m_size--;
        return true;
    }
    
    // Traverse through linked list until node with key found
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            Node* prev = ptr->previous;
            Node* next = ptr->next;
            
            // Link previous node to the proceeding node of the current one
            if(prev != nullptr)
                prev->next = next;
            if(next != nullptr)
                next->previous = prev;
            delete ptr;
            m_size--;
            return true;
        }
        
        ptr = ptr->next;
    }

    // Node with key not found
    return false;
}

bool Map::contains(const KeyType &key) const
{
    // Traverse through linked list until node with key found
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key)
            return true;
        ptr = ptr->next;
    }
    
    // Node with key not found
    return false;
}

bool Map::get(const KeyType &key, ValueType &value) const
{
    // Traverse through linked list until node with key found
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            value = ptr->m_value;
            return true;
        }
        ptr = ptr->next;
    }
    
    // Node with key not found
    return false;
}

bool Map::get(int i, KeyType &key, ValueType &value) const
{
    // i out of bounds
    if(i < 0 || i >= m_size)
        return false;
    
    // Traverse through linked list until ith position
    Node* ptr = m_map;
    for(int j=0; j<i; j++)
        ptr = ptr->next;
    
    key = ptr->m_key;
    value = ptr->m_value;
    
    return true;
}

void Map::swap(Map &other)
{
    // Swap pointers to maps
    Node* otherMap = other.m_map;
    other.m_map = m_map;
    m_map = otherMap;
    
    // Swap map sizes
    int otherSize = other.m_size;
    other.m_size = m_size;
    m_size = otherSize;
}

void Map::dump() const
{
    Node* ptr = m_map;
    cerr << "The map contains the following key-value pairs:" << endl;
    while(ptr != nullptr) {
        cerr << ptr->m_key << "\t" << ptr->m_value << endl;
        ptr = ptr->next;
    }
    cerr << endl;
}

bool combine(const Map& m1, const Map& m2, Map& result)
{
    // Copy all key-value pairs from m1 into temp map as starting point
    Map temp = m1;
    
    // Traverse m2, add/delete key-value pairs to/from temp map appropriately
    bool returnValue = true;
    KeyType m2Key;
    ValueType m2Value, m1Value;
    for(int i=0; i<m2.size(); i++) {
        
        m2.get(i, m2Key, m2Value);
        
        if(temp.get(m2Key, m1Value)) {
            // Key exists in both m1 and m2
            
            if(m1Value != m2Value) {
                // Key with different value exists in m2, so key cannot be in result
                temp.erase(m2Key);
                returnValue = false;
            }
        }
        else
            temp.insert(m2Key, m2Value);
    }
    
    // make result the temp map at the very end
    result.swap(temp);
    return returnValue;
}

void reassign(const Map& m, Map& result)
{
    // Copy all key-value pairs from m1 into result as starting point
    result = m;
    
    // result is just m1 if m1 is empty or has only one key-value pair
    if(result.size() <= 1) return;
    
    // Traverse through result map 2 nodes at a time, swap adjacent nodes' values
    int i=0;
    while(i+1 < result.size()) {
        KeyType key1, key2;
        ValueType value1, value2;
        
        result.get(i, key1, value1);
        result.get(i+1, key2, value2);
        
        result.update(key1, value2);
        result.update(key2, value1);
        i += 2;
    }
    
    // In case of odd-sized map, swap first and last nodes' values
    if(result.size() % 2 == 1) {
        KeyType key1, key2;
        ValueType value1, value2;
        result.get(0, key1, value1);
        result.get(result.size()-1, key2, value2);
        result.update(key1, value2);
        result.update(key2, value1);
    }
    
}
