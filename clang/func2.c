#include <stdio.h>

int olympic(int);

int main(void) 
{ 
  int value;
  value = olympic(2000);
  switch (value) {
    case 0:
      printf("開かれない");
      break;
    case 1:
      printf("夏季オリンピック");
      break;
    case 2:
      printf("冬季オリンピック");
      break;
  }
}

int olympic(int year)
{
  if (year % 2 == 0) {
    if (year % 4 == 0) {
      return 1;
    } else {
      return 2;
    }
  } else {
    return 0;
  }
}
