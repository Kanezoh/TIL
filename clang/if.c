#include <stdio.h>

int main(void) 
{ 
  int year;
  scanf("%d", &year);
  if(year%4 ==0) {
    printf("%d年にオリンピックは開催されています。\n", year);
  }
}
