//
//  main.cpp
//  hello
//
//  Created by Alex Yu on 9/27/19.
//  Copyright © 2019 UCLA. All rights reserved.
//

#include <iostream>
#include <vector>
using namespace std;

int main() {

    int n;
    cout << "Enter how many first n fibonacci numbers to print out: \n";
    cin >> n;

    vector<int> dp;
    dp.push_back(1);
    dp.push_back(1);

    for(int i=2; i<n; i++)
        dp.push_back(dp[i-1] + dp[i-2]);
    
    for(int i=0; i<n; i++)
        cout << dp[i] << "\t";
    
    cout << "\n";

}
