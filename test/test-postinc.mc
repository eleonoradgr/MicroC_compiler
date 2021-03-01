// testing post increment and decrement

int main(){
    /*value bigger that 32 bit */
    int prova = 2147999999785;
    print(prova);
    int i = 5;
    int j = i++;
    if(i==6 && j==5 ){
        print(1);
    }else{
        print(0);
    }
}