i64 main(){
    i64 a = 5;
    i64 b = 3;
    print(a + b);
    print(foo(a+b));
    if a >= b{
        print(10);
    } else {
        print(5);
    }
    a-b
}
i64 foo(i64 param1){
    (param1*param1)-1
}