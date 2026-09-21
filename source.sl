class Span<T> impl Pod{
    let data: *T;
    let len: USize;
    fn static new<T>(data: *T, len: USize): Span<T>{
        Span<T>{
            data,
            len,
        }
    }
    fn end(): *T{
        assert(this.len.cmp_ne(0));
        this.data.ptr_offset(this.len.sub(1))
    }
}