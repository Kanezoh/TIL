#include <stdio.h>

int main(void) 
{ 
  int score;
  do {
    scanf("%d", &score);
  } while(score < 0 || score > 100);

  printf("テストの点数は%d点です。", score);
  return 0;
}
