use alloc::vec::Vec;
use core::fmt::Debug;
use itertools::Itertools;

#[derive(Debug)]
pub enum ConstTableError {
    KeyAlreadyExists,
}

/// Key-value storage with constant size backed by arrays.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstTable<K, V, const N: usize>
where
    K: Copy + PartialEq + Debug,
    V: Copy + Debug,
{
    keys: [K; N],
    values: [V; N],
}

impl<K, V, const N: usize> ConstTable<K, V, N>
where
    K: Copy + PartialEq + Debug,
    V: Copy + Debug,
{
    pub fn new(kvs: [(K, V); N]) -> Self {
        let keys = kvs.map(|(k, _)| k);
        let values = kvs.map(|(_, v)| v);
        Self { keys, values }
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.keys.contains(k)
    }

    /// Adds a new element to the table.
    ///
    /// If the provided key is not already in the table,
    /// returns new [ConstTable] with bigger inner arrays to accomodate
    /// the new element. If the key is already in the table,
    /// returns [ConstTableError::KeyAlreadyExists].
    pub fn push(self, k: K, v: V) -> Result<ConstTable<K, V, { N + 1 }>, ConstTableError> {
        if self.contains_key(&k) {
            return Err(ConstTableError::KeyAlreadyExists);
        }

        let mut keys = self.keys.to_vec();
        keys.push(k);
        let keys: [K; N + 1] = keys.try_into().unwrap();

        let mut values = self.values.to_vec();
        values.push(v);
        let values: [V; N + 1] = values.try_into().unwrap();

        Ok(ConstTable { keys, values })
    }

    pub fn append<const M: usize, const L: usize>(self, kvs: [(K, V); L]) -> ConstTable<K, V, M> {
        let mut keys = self.keys.to_vec();
        let mut values = self.values.to_vec();

        for (k, v) in kvs {
            if !self.contains_key(&k) {
                keys.push(k);
                values.push(v);
            }
        }

        ConstTable {
            keys: keys.try_into().unwrap(),
            values: values.try_into().unwrap(),
        }
    }

    pub fn get(&self, k: &K) -> Option<V> {
        if let Some((idx, _)) = self.keys.iter().find_position(|v| *v == k) {
            return Some(self.values[idx]);
        }
        None
    }

    pub fn key_values(&self) -> Vec<(K, V)> {
        self.keys.into_iter().zip(self.values).collect()
    }

    pub fn len(&self) -> usize {
        N
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;

    #[test]
    fn test_push_size() {
        let t = ConstTable::new([("a", 1), ("b", 3), ("foo", 1337)]);
        assert_eq!(3, t.len());

        let res = t.push("bar", 69);
        assert!(res.is_ok());

        let t = res.unwrap();
        assert_eq!(4, t.len());

        let res = t.push("bar", 420);
        assert!(matches!(res, Err(ConstTableError::KeyAlreadyExists)));

        assert_eq!(4, t.len());
    }

    #[test]
    fn test_append_size() {
        let t = ConstTable::new([("a", 1), ("b", 3), ("foo", 1337)]);
        assert_eq!(t.len(), 3);

        let t: ConstTable<&str, i32, 5> = t.append([("bar", 69), ("baz", 420)]);
        assert_eq!(t.len(), 5);

        let t: ConstTable<&str, i32, 6> = t.append([("bar", 42), ("qux", 1337)]);
        assert_eq!(t.len(), 6);
        assert_eq!(t.get(&"bar"), Some(69));
        assert_eq!(
            t.key_values(),
            vec![
                ("a", 1),
                ("b", 3),
                ("foo", 1337),
                ("bar", 69),
                ("baz", 420),
                ("qux", 1337)
            ]
        );
    }

    #[test]
    #[should_panic]
    fn test_incorrect_size_panic() {
        let t = ConstTable::new([("a", 1), ("b", 3), ("foo", 1337)]);
        let t: ConstTable<&str, i32, 5> = t.append([("b", 69), ("baz", 420)]);
        assert_eq!(5, t.len());
    }
}
