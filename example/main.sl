i64 main(){
    i64 a = 5;
    i64 b = 3;
    Counter cnt = 0;
    cnt.count = 20;
    cnt.count2 = 10;
    print(cnt.count * cnt.count2);
    print(foo(a+b));
    print(-if !(a >= b){
        10
    } else {
        5
    });
    i64 i = 0;
    while i < 10 {
        i += 1;
        print(i);
    }
    print(sum(500));
    a-b
}
i64 foo(i64 param1){
    (param1*param1)-1
}
i64 sum(i64 n){
    if n <= 0{
        0
    } else {
        n + sum(n - 1)
    }
}

struct Counter{
    i64 count;
    i64 count2;
}