use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};

#[derive(Debug)]
pub struct Steal<T> {
    value: RwLock<Option<T>>,
}

impl<T> Steal<T> {
    pub fn new(value: T) -> Self {
        Steal { value: RwLock::new(Some(value)) }
    }

    #[track_caller]
    pub fn borrow(&self) -> MappedRwLockReadGuard<'_, T> {
        let borrow = self.value.read();
        if borrow.is_none() {
            panic!("attempted to read from stolen value: {}", std::any::type_name::<T>());
        }
        RwLockReadGuard::map(borrow, |opt| opt.as_ref().unwrap())
    }

    #[track_caller]
    pub fn get_mut(&mut self) -> &mut T {
        self.value.get_mut().as_mut().expect("attempt to read from stolen value")
    }

    #[track_caller]
    pub fn steal(&self) -> T {
        let value_ref = &mut *self.value.try_write().expect("stealing value which is locked");
        let value = value_ref.take();
        value.expect("attempt to steal from stolen value")
    }
}
