//  Map.cpp

#include "Map.h"
#include <iostream>
using namespace std;

Map::Map()
{
    m_map = nullptr;
    m_size = 0;
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
            prev->next = next;
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

Map::~Map()
{
    // Traverse through linked list and destruct each node
    Node* curr = m_map;
    while(curr != nullptr) {
        Node* next = curr->next;
        delete curr;
        curr = next;
    }
}

Map::Map(const Map& other)
{
    m_size = other.m_size;
    
    // Create first node in map
    Node* other_ptr = other.m_map;
    m_map = new Node;
    *m_map = {other_ptr->m_key, other_ptr->m_value, nullptr, nullptr};
    
    // Traverse through linked list and create new node corresponding to reference
    // map; link nodes together appropriately
    Node* ptr = m_map;
    while(other_ptr->next != nullptr) {
        ptr->next = new Node;
        *(ptr->next) = {(other_ptr->next)->m_key, (other_ptr->next)->m_value, nullptr, ptr};
        other_ptr = other_ptr->next;
        ptr = ptr->next;
    }
}

Map& Map::operator=(const Map &rhs)
{
    if(&rhs == this)
        return *this;
    
    m_size = rhs.m_size;
    
    // Create first node in map
    Node* other_ptr = rhs.m_map;
    m_map = new Node;
    *m_map = {other_ptr->m_key, other_ptr->m_value, nullptr, nullptr};
    
    // Traverse through linked list and create new node corresponding to reference
    // map; link nodes together appropriately
    Node* ptr = m_map;
    while(other_ptr->next != nullptr) {
        ptr->next = new Node;
        *(ptr->next) = {(other_ptr->next)->m_key, (other_ptr->next)->m_value, nullptr, ptr};
        other_ptr = other_ptr->next;
        ptr = ptr->next;
    }
    
    return *this;
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
    // Copy all key-value pairs from m1 into result as starting point
    result = m1;
    
    // Traverse m2, add/delete key-value pairs to/from result appropriately
    bool returnValue = true;
    for(int i=0; i<m2.size(); i++) {
        KeyType key;
        ValueType value;
        m2.get(i, key, value);
        
        ValueType temp;
        
        if(result.get(key, temp)) {
            // Key exists in both m1 and m2
            
            if(temp != value) {
                // Key with different value exists in m2, so key cannot be in result
                result.erase(key);
                returnValue = false;
            }
        }
        else
            result.insert(key, value);
    }
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
