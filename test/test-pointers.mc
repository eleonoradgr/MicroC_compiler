int a = 6;
int *e;

int main(int n){
  int i;
  e = &a;
  int *p = e;
  print(n);
  if(n >= 0){
      print(i);
      do{
        i++;
      }while(i<n);
  }else{
      print(i);
      do{
        i--;
        print(i);
      }while(i>n);
  }
  p = &i;
  *p = *p + 2;
  print(*p);
  float a[] = {1};
  a[0]=a[0]++;
  a[0]=++a[0];
  printfl(a[0]);
  /*segmentation fault expected*/
  p = NULL;
  if(*p == 3){
       print(*e);
  }
     
  return 0;
}