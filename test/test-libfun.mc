/*main with a parameter 
  user must provide 2 input values.
  nested declarations in blocks
*/
int main(int n){
    int m = getint();
    float f = 0.42;
    print(n);
    printfl(f);
    {
        int n = 10;
        print(n);
    }
    return 0;
}