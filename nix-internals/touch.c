#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
  char *out;
  FILE *fp;

  out = getenv("out");

  fp = fopen(out, "w");

  fclose(fp);
}
