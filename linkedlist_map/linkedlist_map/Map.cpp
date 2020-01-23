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
    if(m_map==nullptr) {
        m_map = new Node;
        *m_map = {key, value, nullptr, nullptr};
        m_size++;
        return true;
    }
    
    Node* ptr = m_map;
    
    // traverse linked list until end
    while(ptr->next != nullptr) {
        // key already exists in map, so key-value pair cannot be inserted
        if(ptr->m_key == key)
            return false;
        
        ptr = ptr->next;
    }
    
    // insert new key-value pair
    Node* newNode = new Node;
    *newNode = {key, value, nullptr, ptr};
    ptr->next = newNode;
    
    m_size++;
    
    return true;
}

bool Map::update(const KeyType& key, const ValueType& value)
{
    Node* ptr = m_map;
    
    // traverse through linked list until key found
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            ptr->m_value = value;
            return true;
        }
        ptr = ptr->next;
    }
    
    // key doesn't exist in map
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
    
    if(m_map->m_key == key) {
        Node* curr = m_map;
        m_map = curr->next;
        delete curr;
        
        if(m_map != nullptr)
            m_map->previous = nullptr;
        m_size--;
        return true;
    }
    
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            Node* prev = ptr->previous;
            Node* next = ptr->next;
            
            prev->next = next;
            next->previous = prev;
            delete ptr;
            m_size--;
            return true;
        }
        
        ptr = ptr->next;
    }

    return false;
}

bool Map::contains(const KeyType &key) const
{
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key)
            return true;
        ptr = ptr->next;
    }
    
    return false;
}

bool Map::get(const KeyType &key, ValueType &value) const
{
    Node* ptr = m_map;
    while(ptr != nullptr) {
        if(ptr->m_key == key) {
            value = ptr->m_value;
            return true;
        }
        ptr = ptr->next;
    }
    
    return false;
}

bool Map::get(int i, KeyType &key, ValueType &value) const
{
    if(i < 0 || i >= m_size)
        return false;
    
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
    
    Node* other_ptr = other.m_map;
    m_map = new Node;
    *m_map = {other_ptr->m_key, other_ptr->m_value, nullptr, nullptr};
    
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
    
    Node* other_ptr = rhs.m_map;
    m_map = new Node;
    *m_map = {other_ptr->m_key, other_ptr->m_value, nullptr, nullptr};
    
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
    bool returnValue = true;
    result = m1;
    for(int i=0; i<m2.size(); i++) {
        KeyType key;
        ValueType value;
        m2.get(i, key, value);
        
        ValueType temp;
        if(result.get(key, temp)) {
            if(temp != value) {
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
    result = m;
    if(result.size() <= 1) return;
    
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
    
    if(result.size() % 2 == 1) {
        KeyType key1, key2;
        ValueType value1, value2;
        result.get(0, key1, value1);
        result.get(result.size()-1, key2, value2);
        result.update(key1, value2);
        result.update(key2, value1);
    }
    
}
