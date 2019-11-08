//
//  Project 5 - Flowers
//
//  Created by Alex Yu on 11/8/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include <iostream>
#include <cstring>
#include "utilities.h"
using namespace std;

const int MAX_WORDS = 9000;
const int MAX_WORD_LEN = 6;
const char WORD_FILEPATH[] = "/Users/alexyu/Desktop/Projects/cs31/flowers/flowers/words.txt";
const int BUFFER_LENGTH = 101;

int playOneRound(const char words[][7], int nWords, int wordnum);
bool doesWordExist(const char words[][MAX_WORD_LEN+1], int nWords, char word[]);
bool isValidTrial(const char trial[], int len);
int indexOf(const char str[], int strLen, char c);
void findFlowersAndBees(int flowerAndBees[], const char mystery[], int mysteryLen, const char trial[], int trialLen);

int main()
{
    char words[MAX_WORDS][MAX_WORD_LEN+1];
    int nWords = getWords(words, MAX_WORDS, WORD_FILEPATH);
    
    if(nWords < 1)
    {
        cout << "No words were loaded, so I can't play the game." << endl;
        return 1;
    }
    
    cerr << nWords << " words loaded." << endl;
    
    int roundsToPlay;
    cout << "How many rounds do you want to play? ";
    cin >> roundsToPlay;
    cin.ignore(10000, '\n');
    
    if(roundsToPlay < 0)
    {
        cout << "The number of rounds must be positive." << endl;
        return 1;
    }
    
    double scoreSum = 0;
    int minimum = 0, maximum = 0;
    for(int round=1; round <= roundsToPlay; round++)
    {
        cout << "\nRound " << round << endl;
        int wordnum = randInt(0, nWords);                   // generates a random word index
        int score = playOneRound(words, nWords, wordnum);   // plays a round
        scoreSum += score;
        
        if(score == 1) cout << "You got it in 1 try.\n";
        else cout << "You got it in " << score << " tries.\n";
        
        // keeps track of the minimum and maximum scores
        minimum = (minimum == 0) ? score : (score < minimum) ? score : minimum;
        maximum = (score > maximum) ? score : minimum;
        
        cout << "Average: " << scoreSum / round << ", minimum: " << minimum << ", maximum: " << maximum << "\n";
    }
    
    return 0;
}

int playOneRound(const char words[][MAX_WORD_LEN+1], int nWords, int wordnum)
{
    int score = 0;
    
    char mystery[MAX_WORD_LEN+1];
    strcpy(mystery, words[wordnum]);
    
    // cerr << "The mystery word is " << mystery << ".\n";
    cout << "The mystery word is " << strlen(mystery) << " letters long.\n";
    
    char trial[MAX_WORD_LEN+1];
    do {
        char buffer[BUFFER_LENGTH];           // C-string used to check whether user input is right length and is all lower-case letters
        
        cout << "Trial word: ";
        cin.getline(buffer, BUFFER_LENGTH);
        
        if(!isValidTrial(buffer, static_cast<int>(strlen(buffer))))
            cout << "Your trial word must be a word of 4 to 6 lower case letters.\n";
        else
        {
            strcpy(trial, buffer);
            
            if(!doesWordExist(words, nWords, trial))
                cout << "I don't know that word.\n";
            else
            {
                int flowersAndBees[] = {0, 0}; // 1st index is num flowers, 2nd is bees
                
                findFlowersAndBees(flowersAndBees, mystery, static_cast<int>(strlen(mystery)), trial, static_cast<int>(strlen(trial)));
                cout << "Flowers: " << flowersAndBees[0] << ", Bees: " << flowersAndBees[1] << "\n";
                score++;
            }
        }
    } while(strcmp(trial, mystery) != 0);
    
 
    return score;
}

bool doesWordExist(const char words[][MAX_WORD_LEN+1], int nWords, char word[])
{
    for(int i=0; i<nWords; i++)
        if(strcmp(words[i], word) == 0)
            return true;                        // matching word found in words array
    
    return false;
}

bool isValidTrial(const char trial[], int len)
{
    if(len < 4 || len > 6) return false;        // not between 4 to 6 characters
    
    for(int i=0; i<len; i++)
        if(trial[i] < 'a' || trial[i] > 'z')    // not entirely lower-case letters
            return false;
    
    return true;
}

int indexOf(const char str[], int strLen, char c)
{
    for(int i=0; i<strLen; i++)
        if(str[i] == c)
            return i;                           // character found in string
    
    return -1;
}

void findFlowersAndBees(int flowerAndBees[], const char mystery[], int mysteryLen, const char trial[], int trialLen)
{
    char mysteryCopy[mysteryLen];               // copy of mystery word to keep track of flowers and bees
    strcpy(mysteryCopy, mystery);
    
    // first find all the flowers
    for(int i=0; i<trialLen; i++)
    {
        if(i < mysteryLen && mystery[i] == trial[i])
        {
            flowerAndBees[0]++;     // flower because character in same position
            mysteryCopy[i] = '!';   // mark the position in string as checked
        }
    }
    
    // then finda all the bees
    for(int i=0; i<trialLen; i++)
    {
        int mysteryIndex = indexOf(mysteryCopy, mysteryLen, trial[i]);
        if(mysteryIndex < 0) continue;
        
        flowerAndBees[1]++;                 // bee
        mysteryCopy[mysteryIndex] = '!';    // mark the position in string as checked
    }
}
