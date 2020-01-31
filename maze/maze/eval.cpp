//  eval.cpp

#include "Map.h"
#include <iostream>
#include <string>
#include <stack>
#include <cctype>
#include <cassert>
using namespace std;

// evaluate return values
const int VALID_EVALUATION_CODE    = 0;
const int INVALID_EXPRESSION_ERROR = 1;
const int INVALID_OPERAND_ERROR    = 2;
const int DIVIDE_BY_ZERO_ERROR     = 3;

// helper function declarations
string removeSpaces(string str);
bool isValidOperand(char c);
bool toPostfix(string infix, string& postfix);
bool applyOperator(char operation, int operand1, int operand2, int& result);
int evaluatePostfix(string postfix, const Map& values, int& result);

int evaluate(string infix, const Map& values, string& postfix, int& result);

string removeSpaces(string str)
{
    string result = "";
    for(int i=0; i<str.size(); i++)
        if(str.at(i) != ' ')
            result += str.at(i);
    
    return result;
}

bool isValidOperand(char c)
{
    return (c >= 'a' && c <= 'z');
}

bool isValidOperator(char c)
{
    return c == '+' || c == '-' || c == '*' || c == '/';
}

bool hasLessPrecedence(char op1, char op2)
{
    // precedence value of 0 for + and -, otherwise 1
    
    int op1Precedence = 1;
    if(op1 == '+' || op1 == '-')
        op1Precedence--;
    
    int op2Precedence = 1;
    if(op2 == '+' || op2 == '-')
        op2Precedence--;
    
    return op1Precedence <= op2Precedence;
}

bool toPostfix(string infix, string& postfix)
{
    if(infix == "") return false;
    
    infix = removeSpaces(infix);
    
    string expr = "";
    stack<char> opStack;
    
    for(int i=0; i<infix.size(); i++) {
        char c = infix.at(i);

        switch(c) {
            case '(':
                opStack.push(c);
                break;
            case ')':
                
                // empty expression inside parenthesis
                if(i > 0 && infix.at(i-1) == '(')
                    return false;
                
                // no matching '(' found
                if(opStack.empty())
                    return false;
                
                // pop stack until matching '('
                while(opStack.top() != '(') {
                    expr += opStack.top();
                    opStack.pop();
                    
                    // no matching '(' found
                    if(opStack.empty())
                        return false;
                }
                
                opStack.pop();
                break;
            case '+':
            case '-':
            case '*':
            case '/':
                // operator cannot be first in expression or proceed '(' directly
                // operator cannot be last in expression or precede ')' directly
                if(i == 0 || infix.at(i-1) == '(' ||
                   i == infix.size() - 1 || infix.at(i+1) == ')')
                    return false;
                
                while(!opStack.empty() && opStack.top() != '(' &&
                      hasLessPrecedence(c, opStack.top())) {
                    expr += opStack.top();
                    opStack.pop();
                }
                
                opStack.push(c);
                break;
            default:
                // error if invalid character (eg. '#', 'A')
                if(!isValidOperand(c))
                    return false;
                
                // else, c is an operand
                    
                // operand has to follow '(' or operator if not first in expression
                if(i > 0 && infix.at(i-1) != '(' && !isValidOperator(infix.at(i-1)))
                    return false;
                    
                // operand has to precede ')' or operator if not last in expression
                if(i < infix.size()-1 && infix.at(i+1) != ')' && !isValidOperator(infix.at(i+1)))
                    return false;
                
                expr += c;
                break;
        }
    }
    
    while(!opStack.empty()) {
        char c = opStack.top();
        
        // no matching ')' found
        if(c == '(') return false;
        
        expr += c;
        opStack.pop();
    }
    
    postfix = expr;
    return true;
}

bool applyOperator(char operation, int operand1, int operand2, int& result)
{
    switch(operation) {
        case '+':
            result = operand1 + operand2;
            break;
        case '-':
            result = operand1 - operand2;
            break;
        case '*':
            result = operand1 * operand2;
            break;
        case '/':
            // error if divide by 0
            if(operand2 == 0)
                return false;
            
            result = operand1 / operand2;
            break;
    }
    
    return true;
}

int evaluatePostfix(string postfix, const Map& values, int& result)
{
    stack<int> opStack;
    for(char c: postfix) {
        
        if(c >= 'a' && c <= 'z') {
            // c is a possible operand: check to see if operand is in map
            int value;
            if(values.get(c, value))
                opStack.push(value);
            else
                return INVALID_OPERAND_ERROR;
        }
        else {
            // otherwise, c is an operator
            int operand2 = opStack.top();
            opStack.pop();
            int operand1 = opStack.top();
            opStack.pop();
            
            // try to evaluate
            int eval;
            if(!applyOperator(c, operand1, operand2, eval))
                return DIVIDE_BY_ZERO_ERROR;
            opStack.push(eval);
        }
        
    }
    
    result = opStack.top();
    return VALID_EVALUATION_CODE;
}

int evaluate(string infix, const Map& values, string& postfix, int& result)
{
    if(!toPostfix(infix, postfix))
        return INVALID_EXPRESSION_ERROR;
    
    return evaluatePostfix(postfix, values, result);
}

int main()
{
    char vars[] = { 'a', 'e', 'i', 'o', 'u', 'y', '#' };
    int  vals[] = {  3,  -9,   6,   2,   4,   1  };
    Map m;
    for (int k = 0; vars[k] != '#'; k++)
        m.insert(vars[k], vals[k]);
    string pf;
    int answer;
    assert(evaluate("a+ e", m, pf, answer) == 0  &&
                            pf == "ae+"  &&  answer == -6);
    answer = 999;
    assert(evaluate("", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("a+", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("a i", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("ai", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("()", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("o()", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("y(o+u)", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("y(*o)", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("a+E", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("(a+(i-o)", m, pf, answer) == 1  &&  answer == 999);
      // unary operators not allowed:
    assert(evaluate("-a", m, pf, answer) == 1  &&  answer == 999);
    assert(evaluate("a*b", m, pf, answer) == 2  &&
                            pf == "ab*"  &&  answer == 999);
    assert(evaluate("y +o *(   a-u)  ", m, pf, answer) == 0  &&
                            pf == "yoau-*+"  &&  answer == -1);
    answer = 999;
    assert(evaluate("o/(y-y)", m, pf, answer) == 3  &&
                            pf == "oyy-/"  &&  answer == 999);
    assert(evaluate(" a  ", m, pf, answer) == 0  &&
                            pf == "a"  &&  answer == 3);
    assert(evaluate("((a))", m, pf, answer) == 0  &&
                            pf == "a"  &&  answer == 3);
    cout << "Passed all tests" << endl;
}
