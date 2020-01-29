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

// function declarations
bool isValidOperand(char c);
bool toPostfix(string infix, string& postfix);
bool applyOperator(char operation, int operand1, int operand2, int& result);
int evaluatePostfix(string postfix, const Map& values, int& result);
int evaluate(string infix, const Map& values, string& postfix, int& result);

bool isValidOperand(char c)
{
    return (c >= 'a' && c <= 'z') || c == ' ';
}

bool hasLessPrecedence(char op1, char op2)
{
    int op1Precedence = 2;
    if(op1 == '+' || op1 == '-')
        op1Precedence = 1;
    
    int op2Precedence = 2;
    if(op2 == '+' || op2 == '-')
        op2Precedence = 1;
    
    return op1Precedence <= op2Precedence;
}

bool toPostfix(string infix, string& postfix)
{
    string expr = "";
    stack<char> opStack;
    
    int n_operand = 0, n_operator = 0;
    for(int i=0; i<infix.size(); i++) {
        char c = infix.at(i);

        switch(c) {
            case '(':
                opStack.push(c);
                break;
            case ')':
                // no matching '('
                if(opStack.empty())
                    return false;
                
                // pop stack until matching '('
                while(opStack.top()!='(') {
                    expr += opStack.top();
                    opStack.pop();
                    
                    // no matching '('
                    if(opStack.empty())
                        return false;
                }
                
                opStack.pop();
                break;
            case '+':
            case '-':
            case '*':
            case '/':
                while(!opStack.empty() && opStack.top() != '(' &&
                      hasLessPrecedence(c, opStack.top())) {
                    expr += opStack.top();
                    opStack.pop();
                }
                opStack.push(c);
                n_operator++;
                break;
            default:
                // error if invalid character (eg. '#', 'A')
                if(!isValidOperand(c))
                    return false;
                
                // else, c is an operand
                if(c != ' ') {
                    expr += c;
                    n_operand++;
                    
                    if((i > 0 && (infix.at(i-1) == ')')) ||
                       (i < infix.size()-1 && (infix.at(i+1) == '(')))
                        return false;
                    
                    if(n_operand != n_operator + 1)
                        return false;
                }
                break;
        }
    }
    
    if(n_operand != n_operator + 1)
        return false;
    
    while(!opStack.empty()) {
        char c = opStack.top();
        
        // no matching ')'
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
            // c is a possible operand
            int value;
            if(values.get(c, value))
                opStack.push(value);
            else
                return INVALID_OPERAND_ERROR;
        }
        else {
            // c is an operator
            int operand2 = opStack.top();
            opStack.pop();
            int operand1 = opStack.top();
            opStack.pop();
            
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

/*
int main()
{
    // string infix = "1+2/(4-3)";
    // cout << toPostfix(infix) << endl;
    
    char vars[] = { 'a', 'e', 'i', 'o', 'u', 'y', '#' };
    int  vals[] = {  3,  -9,   6,   2,   4,   1  };
    Map m;
    for (int k = 0; vars[k] != '#'; k++)
        m.insert(vars[k], vals[k]);
    string pf = "";
    int answer;
    // assert(evaluate("a+e", m, pf, answer) == 0  &&
    //                        pf == "ae+"  &&  answer == -6);
    
    cout << evaluate("i/a+e)", m, pf, answer) << endl;
    assert(pf == "");
    return 0;
}
 */

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
