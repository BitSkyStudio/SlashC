void main(){
    *i64 int_ptr = new 33;
    *i64 other_ptr = int_ptr;
    int_ptr = 22;
    print(other_ptr);
}
