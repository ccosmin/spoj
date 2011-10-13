#include <iostream>
#include <stack>
#include <stdlib.h>
#include <string.h>
#include <cctype>

static char* reverse(char* str)
{
    char* left  = str;
    char* right = left + strlen(str) - 1;
    char  tmp;
    while (left < right) {
        tmp      = *left;
        *left++  = *right;
        *right-- = tmp;
    }
    return str;
}

int main()
{
  int x;
  std::cin >> x;

  for ( int i = 0; i < x; ++i )
  {
    std::stack<char> ops;
    std::string out;
    char a[1000];
    std::cin >> a;
    for ( int k = 0; k < strlen(a); ++k )
    {
      if ( std::isalpha(a[k]) )
      {
        out.append(1, a[k]);
      }
      else if ( a[k] == '+' || a[k] == '-' || a[k] == '*' || a[k] == '/' || a[k] == '^' )
      {
        ops.push(a[k]);
      }
      else if ( a[k] == ')' )
      {
        out.append(1, ops.top());
        ops.pop();
      }
    }
    std::cout << out << std::endl;
  }
}

