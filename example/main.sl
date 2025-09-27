i64 main(){
    i64 a = 5;
    i64 b = 3;
    Average avg = Average(foo(3), 2);
    avg.add(10);
    avg.add(15);
    avg.add(30);
    //cnt.increment();
    //Counter::increment(cnt);
    //cnt.increment();
    print(avg.compute());
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

struct Average{
    i64 average;
    i64 count;
    void add(&mut this, i64 value){
        this.average += value;
        this.count += 1;
    }
    i64 compute(&this){
        this.average/this.count
    }
}