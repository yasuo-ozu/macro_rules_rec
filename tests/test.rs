use macro_rules_rec::recursive;

#[recursive]
macro_rules! gen_fields {
    ($id:ident $($t:tt)*) => {
        #[allow(unused)]
        struct $id {
            $self!(@fields $($t)*)
        }
    };
    (@fields $name:ident: $typ:ty, $($t:tt)*) => {
        $name: $typ, $self!(@fields $($t)*)
    };
    (@fields) => {};
}

gen_fields!(MyStruct field1: usize,);

#[allow(unused)]
trait Trait {}

#[recursive]
macro_rules! emit_impl {
    () => {
        impl<S, T, U> Trait for (S, T, U)
        where
            $self!(@bounds S),
            $self!(@bounds T U),
        {}
    };
    (@bounds S) => { S: Copy };
    (@bounds T) => { T: core::iter::Iterator };
    (@bounds U) => { U: 'static };
    (@bounds $($item:ident)*) => {
        $(
            $self!(@bounds $item)
        ),*
    };
}
emit_impl!();
