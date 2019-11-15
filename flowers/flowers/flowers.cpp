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
void findFlowersAndBees(int flowerAndBees[], const char mystery[], const char trial[], int trialLen);

int main()
{
    char words[MAX_WORDS][MAX_WORD_LEN+1];
    int nWords = getWords(words, MAX_WORDS, WORD_FILEPATH);   // fill words array
    
    if(nWords < 1)
    {
        cout << "No words were loaded, so I can't play the game." << endl;
        return 1;
    }
    
    int roundsToPlay;
    cout << "How many rounds do you want to play? ";
    cin >> roundsToPlay;
    cin.ignore(10000, '\n');
    
    if(roundsToPlay <= 0)
    {
        cout << "The number of rounds must be positive." << endl;
        return 1;
    }
    
    cout.setf(ios::fixed);
    cout.precision(2);
    
    double scoreSum = 0;
    int minimum = 0, maximum = 0;
    for(int round=1; round <= roundsToPlay; round++)
    {
        cout << "\nRound " << round << endl;
        
        int wordNum = randInt(0, nWords-1);
        cout << "The mystery word is " << strlen(words[wordNum]) << " letters long.\n";
        
        int score = playOneRound(words, nWords, wordNum);   // plays a round
        if(score < 0)
        {
            cout << "Invalid arguments." << endl;
            return 1;
        }
        scoreSum += score;
        
        if(score == 1) cout << "You got it in 1 try.\n";
        else cout << "You got it in " << score << " tries.\n";
        
        // keeps track of the minimum and maximum scores
        minimum = (minimum == 0) ? score : (score < minimum) ? score : minimum;
        maximum = (score > maximum) ? score : maximum;
        
        cout << "Average: " << scoreSum / round << ", minimum: " << minimum << ", maximum: " << maximum << "\n";
    }
    
    return 0;
}

int playOneRound(const char words[][MAX_WORD_LEN+1], int nWords, int wordnum)
{
    if(nWords <= 0 || wordnum < 0 || wordnum >= nWords)
        return -1;
    
    int score = 0;
    
    // char mystery[MAX_WORD_LEN+1];
    // strcpy(mystery, words[wordnum]);
    
    char mystery[] = "feud";
    
    char trial[MAX_WORD_LEN+1];
    
    // play round continuously as long as trial word is not mystery word
    while(true) {
        // used to first check whether input is right length and is all lowercase letters
        char buffer[BUFFER_LENGTH];
        
        cout << "Trial word: ";
        cin.getline(buffer, BUFFER_LENGTH);
        
        if(!isValidTrial(buffer, static_cast<int>(strlen(buffer))))
            cout << "Your trial word must be a word of 4 to 6 lower case letters.\n";
        else
        {
            strcpy(trial, buffer);
            if(strcmp(trial, mystery) == 0)    // correct when trials word is the mystery word
            {
                score++;
                break;
            }
            else if(!doesWordExist(words, nWords, trial))
                cout << "I don't know that word.\n";
            else
            {
                int flowersAndBees[] = {0, 0}; // 1st index is num flowers, 2nd is bees
                
                findFlowersAndBees(flowersAndBees, mystery, trial, static_cast<int>(strlen(trial)));
                cout << "Flowers: " << flowersAndBees[0] << ", Bees: " << flowersAndBees[1] << "\n";
                score++;
            }
        }
    }
 
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

void findFlowersAndBees(int flowerAndBees[], const char mystery[], const char trial[], int trialLen)
{
    // make copy of mystery word to keep track of what characters have been checked
    char mysteryCopy[MAX_WORD_LEN+1];
    strcpy(mysteryCopy, mystery);
        
    // first find all the flowers
    for(int i=0; i<trialLen; i++)
    {
        if(i < strlen(mysteryCopy) && mystery[i] == trial[i])
        {
            flowerAndBees[0]++;         // flower because character in same position in both
            mysteryCopy[i] = '!';       // mark the position in mystery word as flower
        }
    }
    
    // then find all the bees since flowers take priority
    for(int i=0; i<trialLen; i++)
    {
        // tries to find character in mystery string, will skip already marked positions
        int mysteryIndex = indexOf(mysteryCopy, static_cast<int>(strlen(mysteryCopy)), trial[i]);
        if(mysteryIndex < 0) continue;
        
        flowerAndBees[1]++;                 // bee since character found in mystery word
        mysteryCopy[mysteryIndex] = '!';    // mark the position in mystery word as bee
    }
}
