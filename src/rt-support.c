#include <stdio.h>
#include <stdlib.h>

// TODO
int getint(){
  int input;
  scanf("%d", &input);
  return input;
}

void print(int n){
  printf("%d\n", n);  
}

void printfl(float n){
  printf("%f\n", n);  
}

void printch(char n){
  printf("%c", n);  
}