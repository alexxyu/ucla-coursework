// event.cpp

class Event
{
  public:
    Event(string name) { m_name = name; }
    string name() const { return m_name; }
    
    virtual ~Event() {}
    virtual bool isSport() const { return true; }
    virtual string need() const = 0;
    
  private:
    string m_name;
};

class BasketballGame: public Event
{
  public:
    BasketballGame(string game) : Event(game) { }
    
    virtual ~BasketballGame()
    {
        cout << "Destroying the " << name() << " basketball game" << endl;
    }
    
    virtual string need() const { return "hoops"; }
};

class Concert: public Event
{
  public:
    Concert(string act, string genre) : Event(act)
    {
        m_genre = genre;
    }
    
    virtual ~Concert()
    {
        cout << "Destroying the " << name() << " " << m_genre << " concert" << endl;
    }
    
    virtual bool isSport() const { return false; }
    virtual string need() const { return "a stage"; }
    
  private:
    string m_genre;
};

class HockeyGame: public Event
{
  public:
    HockeyGame(string game) : Event(game) { }
    
    virtual ~HockeyGame()
    {
        cout << "Destroying the " << name() << " hockey game" << endl;
    }
    
    virtual string need() const { return "ice"; }
};
