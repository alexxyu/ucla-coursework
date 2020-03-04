// ExpandableHashMap.h

// Skeleton for the ExpandableHashMap class template.  You must implement the first six
// member functions.

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

private:
    static const int STARTING_BUCKETS = 8;
    
    struct Entry
    {
        KeyType k;
        ValueType v;
        Entry* next;
    };
    
    Entry m_map[STARTING_BUCKETS];
    int   m_size;
    
};

ExpandableHashMap::ExpandableHashMap(double maximumLoadFactor)
{
    m_size = 0;
}

ExpandableHashMap::~ExpandableHashMap()
{
}

void ExpandableHashMap::reset()
{
}

int ExpandableHashMap::size() const
{
    return m_size;
}

void ExpandableHashMap::associate(const KeyType& key, const ValueType& value)
{
}

const ValueType* ExpandableHashMap::find(const KeyType& key) const
{
    return nullptr;  // Delete this line and implement this function correctly
}
