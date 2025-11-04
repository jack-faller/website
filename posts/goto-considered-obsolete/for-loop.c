#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv) {
  int max = atoi(argv[1]);
  int loop(int i, int sum) {
    if (i < max)
      return loop(i + 1, sum + i);
    else
      return sum;
  }
  printf("%d\n", loop(0, 0));
  return 0;
}
