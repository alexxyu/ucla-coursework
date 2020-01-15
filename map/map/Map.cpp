// Map.cpp

#include <iostream>
#include "Map.h"
using namespace std;

Map::Map()
{
    m_size = 0;
}

bool Map::empty() const
{
    return size() == 0;
}

int Map::size() const
{
    return m_size;
}

bool Map::insert(const KeyType &key, const ValueType &value)
{
    if(m_size >= DEFAULT_MAX_ITEMS || contains(key))
        return false;
    
    MapItem item = {key, value};
    m_map[m_size++] = item;
    return true;
}

bool Map::update(const KeyType &key, const ValueType &value)
{
    for(int i=0; i<m_size; i++) {
        if(m_map[i].key == key) {
            MapItem item = {key, value};
            m_map[i] = item;
            return true;
        }
    }
    
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
    for(int i=0; i<m_size; i++) {
        if(m_map[i].key == key) {
            for(int j=i; j<m_size-1; j++)
                m_map[j] = m_map[j+1];
            m_size--;
            return true;
        }
    }
    
    return false;
}

bool Map::contains(const KeyType &key) const
{
    for(int i=0; i<m_size; i++)
        if(m_map[i].key == key)
            return true;
    
    return false;
}

bool Map::get(const KeyType &key, ValueType &value) const
{
    for(int i=0; i<m_size; i++) {
        if(m_map[i].key == key) {
            value = m_map[i].value;
            return true;
        }
    }
    
    return false;
}

bool Map::get(int i, KeyType &key, ValueType &value) const
{
    if(i >= 0 && i < m_size) {
        key = m_map[i].key;
        value = m_map[i].value;
        return true;
    }
    
    return false;
}

void Map::swap(Map &other)
{
    for(int i=0; i<DEFAULT_MAX_ITEMS; i++) {
        MapItem thisItem = m_map[i];
        
        KeyType otherKey;
        ValueType otherValue;
        other.get(i, otherKey, otherValue);
    
        other.m_map[i].key = thisItem.key;
        other.m_map[i].value = thisItem.value;
        
        m_map[i].key = otherKey;
        m_map[i].value = otherValue;
    }
    
    int temp = m_size;
    m_size = other.size();
    other.m_size = temp;
}

void Map::dump() const
{
    if(m_size == 0) {
        cerr << "The map is empty." << endl;
    }
    else {
        cerr << "The map contains the following key-value pairs:" << endl;
        for(int i=0; i<m_size; i++)
            cerr << m_map[i].key << "\t" << m_map[i].value << endl;
        cerr << endl;
    }
}
