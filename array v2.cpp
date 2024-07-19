//
//  main.cpp
//  Project 6
//
//

#include <iostream>
#include <string>
#include <cctype>
#include <cassert>

using namespace std;

// Function to locate the maximum item in the array
int locateMaximum(const string array[], int n)
{
    // Case when no elements need to be checked
    if (n <= 0)
        return -1;
    
    // Compare each array element and assign max index
    int maxIndex = 0;
    for (int i = 1; i < n; i++)
        if (array[i] > array[maxIndex])
            maxIndex = i;
    return maxIndex;
}

// Function to count numerical floating point values in the array
// Floating point values is defined as any number with a decimal point
int countFloatingPointValues(const string array[], int n)
{
    // Case when no elements need to be checked //
    if (n <= 0)
    {
        return -1;
    }
    int count = 0;
    for (int i = 0; i < n; i++)
    {
        bool isFloatingPoint = true;  // Flag for checking if a number is floating point
        bool hasDecimalPoint = false; // Flag for checking if string has a single decimal point

        for (int j = 0; j < array[i].length(); j++)
        {
            // String should not start with a leading + or -
            if (j == 0 && (array[i][j] == '+' || array[i][j] == '-'))
            {
                isFloatingPoint = false;
                break;
            }

            // If string contains '.', then it should be the only one
            if (array[i][j] == '.')
            {
                if (hasDecimalPoint)
                {
                    isFloatingPoint = false;
                    break;
                }
                hasDecimalPoint = true;
            }

            // In all other cases like commas, do not count it
            else if (!isdigit(array[i][j]))
            {
                isFloatingPoint = false;
                break;
            }
        }
        if (isFloatingPoint && hasDecimalPoint)
        {
            count++;
        }
    }
    return count;
}


// Function to check if there are any capital letters in the array
bool hasNoCapitals(const string array[], int n)
{
    if (n <= 0)
        return true;
    
    // Return false if any character is upper case
    for (int i = 0; i < n; i++)
    {
        for (char c : array[i])
        {
            if (isupper(c))
                return false;
        }
    }
    return true;
}

// Function to shift all elements in the array and count the number of times they were replaced
int shiftLeft(string array[], int n, int amount, string placeholder)
{
    if (n <= 0 || amount < 0)
        return -1;
    
    int shiftCount = min(n, amount); // Choose a valid shift count
    
    // Shift strings
    for (int i = 0; i < n - shiftCount; i++)
    {
        array[i] = array[i + shiftCount];
    }
    
    // Add placeholders
    for (int i = n - shiftCount; i < n; i++)
    {
        array[i] = placeholder;
    }
    return shiftCount;
}

int main()
{
    string folks[8] = {"samwell", "jon", "margaery", "daenerys", "tyrion", "sansa", "magdalena", "jon"};
    string a[6] = {"123", "456", "789", "gamma", "beta", "delta"};
    string data[5] = {"mamaBbcca", "mamaBbcca", "12", "98.76", "tyrion"};
    string name[10] = {"Andrew", "Bethany", "Christie", "0000076", "00.00.76", "7600+00", "76.000", "Danny", "Edward"};
    string num[7] = {"12.3", "299.3", "+21.2", "002.2.1", "210,2.1", "212.1", "-2818.11.1"};
    string num2[9] = {"55.2", "Beyonce", "Beyonce", "211.2.2", "-79.3.2", "56", "000.19", "++29", "55.2"};
 
    assert(locateMaximum(data, 5) == 4);
    assert(locateMaximum(a, 6) == 3);
    assert(locateMaximum(a, 0) == -1);
    assert(locateMaximum(folks, 8) == 4);
    assert(locateMaximum(name, 10) == 8);
    assert(locateMaximum(num, 7) == 1);
    assert(locateMaximum(name, 1) == 0);
    
    assert(countFloatingPointValues(num, 7) == 3);
    assert(countFloatingPointValues(data, 5) == 1);
    assert(countFloatingPointValues(folks, 1) == 0);
    assert(countFloatingPointValues(a, 3) == 0);
    assert(countFloatingPointValues(name, 9) == 1);
    assert(countFloatingPointValues(num2, 9) == 3);
    assert(countFloatingPointValues(a, 0) == -1);
    
    assert(hasNoCapitals(data, 5) == false);
    assert(hasNoCapitals(folks, 0) == true);
    assert(hasNoCapitals(a, 6) == true);
    assert(hasNoCapitals(num, 7) == true);
    assert(hasNoCapitals(num2, 9) == false);
    
    assert(shiftLeft(data, 5, 2, "Kowloon") == 2);
    assert(shiftLeft(data, 5, 10, "Kennedy Town") == 5);
    assert(shiftLeft(data, -5, 10, "Mong Kok") == -1);
    assert(shiftLeft(data, 5, -5, "Repulse Bay") == -1);
    assert(shiftLeft(name, 3, 2, "Stanley") == 2);
    assert(shiftLeft(a, 6, 12, "Placeholder") == 6);

    cout << "All tests passed!" << endl;
    return 0;
}


