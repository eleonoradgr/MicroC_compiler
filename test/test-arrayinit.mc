//test array initialization

void printArray(int arr[], int n) 
{ 
    int i; 
    for (i = 0; i < n; i = i + 1) 
        print(arr[i]); 
}

void main(){
    int a[5] = {1, 2, 3, 4};
    printArray(a,5);
    int b[3] = {1, 2, 3, 4};
    printArray(b,3);
    int c[];
    printArray(c,1);
}