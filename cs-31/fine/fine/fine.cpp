//
//  fine.cpp
//  Project 2
//
//  Calculates the suggested amount for a fine that is to be imposed on a
//  defendant convicted of fraud in the college admissions scandal.
//
//  Created by Alex Yu on 10/10/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include <iostream>
using namespace std;

int main() {

    const double baseFine = 20.0;
    const double firstThresh = 40.0;      // first amount paid tier to be fined
    const double secondThresh = 210.0;    // next amount paid tier to be fined
    
    // asks for and reads defendant name
    cout << "Defendant: ";
    string defendantName;
    getline(cin, defendantName);
    
    // checks whether defendant name is a non-empty string
    if(defendantName == "") {
        cout << "---" << endl;
        cout << "You must enter a defendant name." << endl;
        return 1;
    }
    
    // asks for and reads amount paid to Stinger in thousands
    cout << "Amount paid (in thousands): ";
    double amountPaid;
    cin >> amountPaid;
    
    // checks whether amount paid is non-negative number
    if(amountPaid < 0) {
        cout << "---" << endl;
        cout << "The amount paid must not be negative." << endl;
        return 1;
    }
    
    // asks for and reads whether the defendant claimed s/he was a fake athlete
    cout << "Fake athlete? (y/n): ";
    string fakeAthlete;
    cin.ignore(10000, '\n');      // assures that the previous input doesn't skip this question
    getline(cin, fakeAthlete);
    
    // checks whether fake athlete input is either "y" or "n"
    if(fakeAthlete != "y" && fakeAthlete != "n") {
        cout << "---" << endl;
        cout << "You must enter y or n." << endl;
        return 1;
    }
    
    // converts user input for fake athlete into bool
    bool isFakeAthlete = (fakeAthlete == "y");
    
    cout << "---" << endl;
    
    double fine = baseFine;                  // base fine amount
    if(amountPaid < firstThresh) {
        fine += amountPaid * 0.66;           // fine for under $40 thousand paid
    } else {
        fine += firstThresh * 0.66;          // fine for first $40 thousand paid
        
        amountPaid -= firstThresh;           // check next tier of fines;
        
        double fineRate = 0.10;              // default fine percentage for this bracket
        if(isFakeAthlete) {
            fineRate = 0.22;                 // fine percentage for if defendant claimed to be athlete
        }
        
        if(amountPaid < secondThresh) {
            fine += amountPaid * fineRate;   // fine for under $250 thousand paid total
        } else {
            fine += secondThresh * fineRate; // fine for next $210 thousand paid
            
            amountPaid -= secondThresh;
            fine += amountPaid * 0.14;       // fine for any amount above $250 thousand paid total
            
        }
    }
    
    cout.setf(ios::fixed);
    cout.precision(1);
    cout << "The suggested fine for " << defendantName << " is $" << fine <<
        " thousand." << endl;
    
    return 0;
    
}
