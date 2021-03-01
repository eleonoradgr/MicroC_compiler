int main(int pred){
    int x0 = 1;
    do{
        int x1 = x0;
        bool b = (x1 != 1);
        if (b){
            x0 = 2;
        }
    }while(pred != 8)
    
    return x0;
}