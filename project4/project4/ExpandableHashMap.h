// ExpandableHashMap.h

// Skeleton for the ExpandableHashMap class template.  You must implement the first six
// member functions.

#ifndef EXPANDABLE_HASH_MAP
#define EXPANDABLE_HASH_MAP

#include <vector>
#include <list>
#include <iostream>

template<typename KeyType, typename ValueType>
class ExpandableHashMap
{
public:
    ExpandableHashMap(double maximumLoadFactor = 0.5);
    ~ExpandableHashMap();
    void reset();
    int size() const;
    void associate(const KeyType& key, const ValueType& value);

      // for a map that can't be modified, return a pointer to const ValueType
    const ValueType* find(const KeyType& key) const;

      // for a modifiable map, return a pointer to modifiable ValueType
    ValueType* find(const KeyType& key)
    {
        return const_cast<ValueType*>(const_cast<const ExpandableHashMap*>(this)->find(key));
    }

      // C++11 syntax for preventing copying and assignment
    ExpandableHashMap(const ExpandableHashMap&) = delete;
    ExpandableHashMap& operator=(const ExpandableHashMap&) = delete;
    
    void dump() const;

private:
    static const int STARTING_BUCKETS = 8;
    
    struct Entry
    {
        KeyType key;
        ValueType value;
    };
    
    std::vector<std::list<Entry*>> m_map;
    int    m_size;
    double m_currentLoadFactor;
    double m_maxLoadFactor;
    
    unsigned int getBucketNumber(const KeyType& key, size_t size) const;
    void rehash();
};

template<typename KeyType, typename ValueType>
ExpandableHashMap<KeyType, ValueType>::ExpandableHashMap(double maximumLoadFactor)
: m_map(STARTING_BUCKETS)
{
    m_size = 0;
    m_currentLoadFactor = 0;
    m_maxLoadFactor = maximumLoadFactor;
}

template<typename KeyType, typename ValueType>
ExpandableHashMap<KeyType, ValueType>::~ExpandableHashMap()
{
    reset();
}

template<typename KeyType, typename ValueType>
void ExpandableHashMap<KeyType, ValueType>::reset()
{
    for(int i=0; i<m_map.size(); i++) {
        std::list<Entry*>* bucket = &m_map[i];
        for(Entry* entry: *bucket)
            delete entry;
        bucket->clear();
    }
    
    m_size = 0;
    m_currentLoadFactor = 0;
}

template<typename KeyType, typename ValueType>
int ExpandableHashMap<KeyType, ValueType>::size() const
{
    return m_size;
}

template<typename KeyType, typename ValueType>
void ExpandableHashMap<KeyType, ValueType>::associate(const KeyType& key, const ValueType& value)
{
    int idx = getBucketNumber(key, m_map.size());
    std::list<Entry*>* bucket = &(m_map[idx]);
    
    bool shouldInsertNewEntry = true;
    for(Entry* entry: *(bucket)) {
        
        // replace value of entry that has same key
        if(entry->key == key) {
            entry->value = value;
            shouldInsertNewEntry = false;
            break;
        }
    
    }
    
    // inserts new entry if no entry with key already contained
    if(shouldInsertNewEntry) {
        Entry* newEntry = new Entry;
        *newEntry = {key, value};
        bucket->push_front(newEntry);
        m_size++;
    }
    
    // adjust load factor and rehash if needed
    m_currentLoadFactor = (1.0 * m_size) / m_map.size();
    if(m_currentLoadFactor > m_maxLoadFactor)
        rehash();
}

template<typename KeyType, typename ValueType>
const ValueType* ExpandableHashMap<KeyType, ValueType>::find(const KeyType& key) const
{
    int idx = getBucketNumber(key, m_map.size());
    const std::list<Entry*>* bucket = &m_map[idx];
    
    // iterate through bucket searching for key
    for(Entry* entry: *bucket)
        if(entry->key == key)
            return &(entry->value);
    
    return nullptr;
}

template<typename KeyType, typename ValueType>
void ExpandableHashMap<KeyType, ValueType>::rehash()
{
    std::vector<std::list<Entry*>> newMap(m_map.size() * 2);
    
    for(int i=0; i<m_map.size(); i++) {
        std::list<Entry*> bucket = m_map[i];
        for(Entry* currEntry: bucket) {
            // rehash current entry and re-insert into new map
            int idx = getBucketNumber(currEntry->key, newMap.size());
            newMap[idx].push_back(currEntry);
        }
    }
    
    m_map = newMap;
}

template<typename KeyType, typename ValueType>
unsigned int ExpandableHashMap<KeyType, ValueType>::getBucketNumber(const KeyType& key, size_t size) const
{
    unsigned int hasher(const KeyType& k); // prototype
    unsigned int h = hasher(key);
    return h % size;
}

template<typename KeyType, typename ValueType>
void ExpandableHashMap<KeyType, ValueType>::dump() const
{
    std::cerr << "The entries in this map are:" << std::endl;
    for(int i=0; i<m_map.size(); i++) {
        const std::list<Entry*> bucket = m_map[i];
        for(Entry* e: bucket) {
            std::cerr << e->key << "\t" << e->value << std::endl;
        }
    }
    std::cerr << std::endl;
}

#endif
