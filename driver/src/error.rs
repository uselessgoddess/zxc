use {std::marker::PhantomData, yansi::Paint};

pub trait Emission: Sized {
    fn emit_guarantee(d: Diagnostic<Self>) -> Self;
}

impl Emission for () {
    fn emit_guarantee(d: Diagnostic<Self>) -> Self {
        println!("{}", d.msg)
    }
}

impl Emission for ! {
    fn emit_guarantee(d: Diagnostic<Self>) -> Self {
        panic!("{}", d.msg)
    }
}

pub struct Diagnostic<E: Emission> {
    msg: String,
    _marker: PhantomData<E>,
}

impl<E: Emission> Diagnostic<E> {
    pub fn new(msg: String) -> Self {
        Self { msg, _marker: PhantomData }
    }

    fn emit(self) -> E {
        E::emit_guarantee(self)
    }
}

pub struct EarlyErrorHandler;

impl EarlyErrorHandler {
    pub fn early_fatal(&self, msg: impl Into<String>) -> ! {
        Diagnostic::new(format!("{} {}", Paint::red("error:"), msg.into())).emit()
    }
}
