#include <stdio.h>
#include <ctype.h>

int main(void) {
  char name[20];
  char first[10];
  char last[10];

  scanf("%9s", first);
  scanf("%9s", last);
  sprintf(name, "%s%s\n", first, last);
  printf(name);
}
