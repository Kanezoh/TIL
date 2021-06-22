#include <stdio.h>
int main() {
  FILE *file;

  file = fopen("dora.txt", "w");
  fprintf(file, "1,野比のび太,0\n"),
  fprintf(file, "2,源静香,90\n"),
  fprintf(file, "3,剛田武,40\n"),
  fprintf(file, "4,骨川スネ夫,70\n"),
  fclose(file);
  return 0;
}
