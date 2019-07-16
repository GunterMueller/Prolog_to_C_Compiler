//// simple test for embedding compiled code


#include <stdio.h>
#include <assert.h>
#include "pc.h"


extern X prolog(int argc, char *argv[], X result, int *exit_code);
extern X *prolog_variable(int index);


int main()
{
  char *argv[] = { "" };
  X one = prolog(1, argv, NULL, NULL);
  X *global = prolog_variable(0);
  *global = one;
  int x = 0;
  X two = prolog(0, NULL, word_to_fixnum(123), &x);
  assert(x == EXIT_SUCCESS);
  assert(two == word_to_fixnum(2));
  X three = prolog(0, NULL, *global, &x);
  assert(x == EXIT_SUCCESS);
}
