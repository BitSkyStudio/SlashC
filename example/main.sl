i64 main(){
    i64 a = 5;
    i64 b = 3;
    print(a + b);
    print(foo(a+b));
    if (a >= b) ^ true ^ false{
        print(10);
    } else {
        print(5);
    }
    i64 i = 0;
    while i < 10 {
        i += 1;
        print(i);
    }
    a-b
}
i64 foo(i64 param1){
    (param1*param1)-1
}