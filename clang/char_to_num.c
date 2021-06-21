#include <stdio.h>
#include <ctype.h>

int main(void) {
  char c = 'A';
  int num;
  if (isdigit(c)) {
    num = c - '0';
  } else {
    num = 0;
  }
  printf("%d\n", num);
  return 0;
}
