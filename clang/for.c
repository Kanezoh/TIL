#include <stdio.h>

int main(void) 
{ 
  for(int i=1;i<10;i++) {
    for(int j=1;j<10;j++) {
      if (j == 9) {
        printf("%2d \n", i*j);
      } else {
        printf("%2d ", i*j);
      }
    }
  }
}
