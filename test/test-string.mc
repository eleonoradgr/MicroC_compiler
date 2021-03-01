void printString(char arr[], int n) 
{ 
    int i; 
    for (i = 0; i < n; i = i + 1) 
        printch( arr[i] );
    
}

void main(){
    int a[] = {1,2,3};
    char b[] = "gatta";
    b[0] = 'm';
    printString(b,6);
    char prova[]="ciao";
    char prova1[]= "io sono una \n stringa \"   \
                    molto particolare";
}