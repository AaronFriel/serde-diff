use crate as serde_diff;
use crate::{Apply, Diff, SerdeDiff};
use insta::assert_yaml_snapshot;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cell::Cell;
use std::fmt::Debug;

#[derive(SerdeDiff, Serialize, Deserialize, PartialEq, Debug, Copy, Clone)]
struct TestStruct {
    a: u32,
    b: f64,
}

fn roundtrip<T: SerdeDiff + Serialize + for<'a> Deserialize<'a> + PartialEq + Debug + Clone>(
    old: T,
    new: T,
) {
    let diff = Diff::serializable(&old, &new);
    let json_diff = serde_json::to_string(&diff).unwrap();
    let mut deserializer = serde_json::Deserializer::from_str(&json_diff);
    let mut target = old.clone();
    Apply::apply(&mut deserializer, &mut target).unwrap();
    assert_eq!(target, new);

    let bincode_diff = bincode::serialize(&diff).unwrap();
    let mut target = old;

    bincode::config()
        .deserialize_seed(Apply::deserializable(&mut target), &bincode_diff)
        .unwrap();
    assert_eq!(target, new);
}

fn partial<T: SerdeDiff + Serialize + for<'a> Deserialize<'a> + PartialEq + Debug + Clone>(
    old: T,
    new: T,
    target: T,
    expected: T,
) {
    let diff = Diff::serializable(&old, &new);
    let json_diff = serde_json::to_string(&diff).unwrap();
    let mut deserializer = serde_json::Deserializer::from_str(&json_diff);
    let mut tmp_target = target.clone();
    Apply::apply(&mut deserializer, &mut tmp_target).unwrap();
    assert_eq!(tmp_target, expected);

    let bincode_diff = bincode::serialize(&diff).unwrap();
    let mut tmp_target = target;
    bincode::config()
        .deserialize_seed(Apply::deserializable(&mut tmp_target), &bincode_diff)
        .unwrap();
    assert_eq!(tmp_target, expected);
}

#[test]
fn test_option() {
    roundtrip(None::<TestStruct>, None);
    roundtrip(None, Some(TestStruct { a: 42, b: 12. }));
    roundtrip(Some(TestStruct { a: 42, b: 12. }), None);
    roundtrip(
        Some(TestStruct { a: 52, b: 32. }),
        Some(TestStruct { a: 42, b: 12. }),
    );

    partial(
        Some(TestStruct { a: 5, b: 2. }),
        Some(TestStruct { a: 8, b: 2. }),
        Some(TestStruct { a: 0, b: 4. }),
        Some(TestStruct { a: 8, b: 4. }),
    );
}

#[test]
fn test_array() {
    partial([0, 1, 2, 3], [0, 1, 9, 3], [4, 5, 6, 7], [4, 5, 9, 7]);

    partial(
        Some([0, 1, 2, 3]),
        Some([0, 1, 9, 3]),
        Some([4, 5, 6, 7]),
        Some([4, 5, 9, 7]),
    );

    partial(
        [
            None,
            Some(TestStruct { a: 5, b: 2. }),
            Some(TestStruct { a: 5, b: 2. }),
        ],
        [
            Some(TestStruct { a: 8, b: 2. }),
            Some(TestStruct { a: 8, b: 2. }),
            None,
        ],
        [None, Some(TestStruct { a: 0, b: 4. }), None],
        [
            Some(TestStruct { a: 8, b: 2. }),
            Some(TestStruct { a: 8, b: 4. }),
            None,
        ],
    );
}

#[test]
fn test_tuple() {
    roundtrip(
        (None::<TestStruct>, Some(TestStruct { a: 8, b: 2. })),
        (None, Some(TestStruct { a: 8, b: 2. })),
    );

    partial((0, 1, 2, 3), (0, 1, 9, 3), (4, 5, 6, 7), (4, 5, 9, 7));

    partial(
        Some((0, 1, 2, 3)),
        Some((0, 1, 9, 3)),
        Some((4, 5, 6, 7)),
        Some((4, 5, 9, 7)),
    );

    partial(
        (
            None,
            Some(TestStruct { a: 5, b: 2. }),
            Some(TestStruct { a: 5, b: 2. }),
        ),
        (
            Some(TestStruct { a: 8, b: 2. }),
            Some(TestStruct { a: 8, b: 2. }),
            None,
        ),
        (None, Some(TestStruct { a: 0, b: 4. }), None),
        (
            Some(TestStruct { a: 8, b: 2. }),
            Some(TestStruct { a: 8, b: 4. }),
            None,
        ),
    );
}

#[derive(SerdeDiff, Serialize, Deserialize, Clone, PartialEq, Debug, Default)]
#[serde(from = "MySimpleStruct", into = "MySimpleStruct")]
#[serde_diff(target = "MySimpleStruct")]
struct MyComplexStruct {
    // This field will be serialized
    a: u32,
    // This field will not be serialized, because it is not needed for <some reason>
    b: u32,
}

#[derive(SerdeDiff, Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash, Debug, Default)]
#[serde(rename = "MyComplexStruct", default)]
struct MySimpleStruct {
    a: u32,
}

#[derive(SerdeDiff, Serialize, Deserialize, Clone, PartialEq, Debug, Default)]
struct MyCowStruct<'a> {
    a: Cow<'a, MySimpleStruct>,
}

#[derive(SerdeDiff, Serialize, Deserialize, Clone, PartialEq, Debug, Default)]
struct MyBoxStruct {
    a: Box<MySimpleStruct>,
}

#[derive(SerdeDiff, Serialize, Deserialize, Clone, PartialEq, Debug, Default)]
struct MyCellStruct {
    a: Cell<MySimpleStruct>,
}

impl From<MySimpleStruct> for MyComplexStruct {
    fn from(my_simple_struct: MySimpleStruct) -> Self {
        MyComplexStruct {
            a: my_simple_struct.a,
            b: 0, // this value wasn't serialized, so we'll just default it to zero
        }
    }
}

impl Into<MySimpleStruct> for MyComplexStruct {
    fn into(self) -> MySimpleStruct {
        MySimpleStruct { a: self.a }
    }
}

fn targeted_roundtrip<T, U>(old: T, new: T, expected: T)
where
    T: SerdeDiff + Serialize + for<'a> Deserialize<'a> + PartialEq + Debug + Clone,
    U: SerdeDiff + Serialize + for<'a> Deserialize<'a>,
{
    let diff = Diff::serializable(&old, &new);
    let json_diff = serde_json::to_string(&diff).unwrap();
    let mut deserializer = serde_json::Deserializer::from_str(&json_diff);
    let mut applied = old.clone();
    Apply::apply(&mut deserializer, &mut applied).unwrap();
    assert_eq!(applied, expected);

    let bincode_diff = bincode::serialize(&diff).unwrap();
    let mut applied = old;

    bincode::config()
        .deserialize_seed(Apply::deserializable(&mut applied), &bincode_diff)
        .unwrap();
    assert_eq!(applied, expected);
}

#[test]
fn test_targeted() {
    targeted_roundtrip::<MyComplexStruct, MySimpleStruct>(
        MyComplexStruct { a: 1, b: 777 },
        MyComplexStruct { a: 2, b: 999 },
        MyComplexStruct { a: 2, b: 0 },
    );
    targeted_roundtrip::<Option<MyComplexStruct>, Option<MySimpleStruct>>(
        Some(MyComplexStruct { a: 1, b: 777 }),
        Some(MyComplexStruct { a: 2, b: 999 }),
        Some(MyComplexStruct { a: 2, b: 0 }),
    );
}

#[test]
fn test_cow() {
    roundtrip(
        MyCowStruct {
            a: Cow::Owned(MySimpleStruct { a: 0 }),
        },
        MyCowStruct {
            a: Cow::Owned(MySimpleStruct { a: 10 }),
        },
    );
    let a = MySimpleStruct { a: 0 };
    let b = MySimpleStruct { a: 1 };
    roundtrip(
        MyCowStruct {
            a: Cow::Borrowed(&a),
        },
        MyCowStruct {
            a: Cow::Owned(MySimpleStruct { a: 10 }),
        },
    );
    roundtrip(
        MyCowStruct {
            a: Cow::Owned(MySimpleStruct { a: 0 }),
        },
        MyCowStruct {
            a: Cow::Borrowed(&b),
        },
    );
    roundtrip(
        MyCowStruct {
            a: Cow::Borrowed(&a),
        },
        MyCowStruct {
            a: Cow::Borrowed(&b),
        },
    );
}

#[test]
fn test_box() {
    roundtrip(
        MyBoxStruct {
            a: Box::new(MySimpleStruct { a: 0 }),
        },
        MyBoxStruct {
            a: Box::new(MySimpleStruct { a: 10 }),
        },
    );
}

#[test]
fn test_cell() {
    roundtrip(
        MyCellStruct {
            a: Cell::new(MySimpleStruct { a: 0 }),
        },
        MyCellStruct {
            a: Cell::new(MySimpleStruct { a: 10 }),
        },
    );
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize, SerdeDiff)]
#[serde_diff(opaque)]
struct Foo<'a>(Cow<'a, [u8]>);

#[test]
fn test_generic_opaque() {
    roundtrip(
        Foo(Cow::Owned(vec![1, 2, 3])),
        Foo(Cow::Owned(vec![1, 2, 3])),
    );
}

#[cfg(feature = "im")]
#[test]
fn test_immutable_simple() {
    use im::HashMap;
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};
    type Hasher = BuildHasherDefault<DefaultHasher>;

    let mut this = HashMap::with_hasher(Hasher::default());

    this.insert(0, MySimpleStruct { a: 0 });
    this.insert(20, MySimpleStruct { a: 20 });
    this.insert(300, MySimpleStruct { a: 300 });

    let mut other = this.clone();

    other.insert(20, MySimpleStruct { a: 25 });
    other.remove(&300);
    other.insert(4000, MySimpleStruct { a: 4000 });

    let diff = Diff::serializable(&this, &other);

    // Expect:
    //   0 not present
    //   20 updated
    //   300 removed
    //   4000 inserted
    assert_yaml_snapshot!(diff, @r###"
    ---
    - EnterKey: 20
    - Enter:
        Field: a
    - Value: 25
    - Exit
    - RemoveKey: 300
    - AddKey: 4000
    - Value:
        a: 4000
    - Exit
    "###);

    roundtrip(this, other);
}

#[derive(SerdeDiff, Serialize, Deserialize, PartialEq, Debug, Copy, Clone, Hash, Eq)]
struct Simple {
    a: usize,
}

#[derive(SerdeDiff, Serialize, Deserialize, PartialEq, Debug, Copy, Clone, Hash, Eq)]
struct Complex {
    a: Simple,
    b: (usize, Option<usize>),
}

#[cfg(feature = "im")]
#[test]
fn test_immutable_complex() {
    use im::HashMap;
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};
    type Hasher = BuildHasherDefault<DefaultHasher>;

    let mut this = HashMap::with_hasher(Hasher::default());

    this.insert(
        Complex {
            a: Simple { a: 0 },
            b: (0, None),
        },
        Complex {
            a: Simple { a: 0 },
            b: (0, None),
        },
    );
    this.insert(
        Complex {
            a: Simple { a: 20 },
            b: (0, None),
        },
        Complex {
            a: Simple { a: 20 },
            b: (0, None),
        },
    );
    this.insert(
        Complex {
            a: Simple { a: 300 },
            b: (0, None),
        },
        Complex {
            a: Simple { a: 300 },
            b: (0, None),
        },
    );

    let mut other = this.clone();

    other.insert(
        Complex {
            a: Simple { a: 20 },
            b: (0, None),
        },
        Complex {
            a: Simple { a: 25 },
            b: (0, None),
        },
    );
    other.remove(&Complex {
        a: Simple { a: 300 },
        b: (0, None),
    });
    other.insert(
        Complex {
            a: Simple { a: 4000 },
            b: (0, None),
        },
        Complex {
            a: Simple { a: 4000 },
            b: (0, None),
        },
    );

    let diff = Diff::serializable(&this, &other);

    assert_yaml_snapshot!(diff, @r###"
    ---
    - EnterKey:
        a:
          a: 20
        b:
          - 0
          - ~
    - Enter:
        Field: a
    - Enter:
        Field: a
    - Value: 25
    - Exit
    - Exit
    - RemoveKey:
        a:
          a: 300
        b:
          - 0
          - ~
    - AddKey:
        a:
          a: 4000
        b:
          - 0
          - ~
    - Value:
        a:
          a: 4000
        b:
          - 0
          - ~
    - Exit
    "###);

    roundtrip(this, other);
}

#[test]
fn test_hashmap() {
    use std::collections::HashMap;
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};
    type Hasher = BuildHasherDefault<DefaultHasher>;

    let mut this = HashMap::with_hasher(Hasher::default());

    this.insert(Simple { a: 0 }, Simple { a: 0 });
    this.insert(Simple { a: 20 }, Simple { a: 20 });
    this.insert(Simple { a: 300 }, Simple { a: 300 });

    let mut other = this.clone();

    other.insert(Simple { a: 20 }, Simple { a: 25 });
    other.remove(&Simple { a: 300 });
    other.insert(Simple { a: 4000 }, Simple { a: 4000 });

    let diff = Diff::serializable(&this, &other);

    assert_yaml_snapshot!(diff, @r###"
    ---
    - EnterKey:
        a: 20
    - Enter:
        Field: a
    - Value: 25
    - Exit
    - RemoveKey:
        a: 300
    - AddKey:
        a: 4000
    - Value:
        a: 4000
    - Exit
    "###);

    roundtrip(this, other);
}

#[test]
fn test_hashmap_ints() {
    use std::collections::HashMap;
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};
    type Hasher = BuildHasherDefault<DefaultHasher>;

    let mut this = HashMap::with_hasher(Hasher::default());

    this.insert(0, 0);
    this.insert(20, 20);
    this.insert(300, 300);

    let mut other = this.clone();

    other.insert(20, 25);
    other.remove(&300);
    other.insert(4000, 4000);

    let diff = &Diff::serializable(&this, &other);
    let diff_str = serde_json::to_string(diff).unwrap();
    let mut deserializer = serde_json::Deserializer::from_str(&diff_str);

    assert_yaml_snapshot!(&this, @r###"
    ---
    0: 0
    20: 20
    300: 300
    "###);
    assert_yaml_snapshot!(&other, @r###"
    ---
    0: 0
    20: 25
    4000: 4000
    "###);
    assert_yaml_snapshot!(&diff, @r###"
    ---
    - EnterKey: 20
    - Value: 25
    - RemoveKey: 300
    - AddKey: 4000
    - Value: 4000
    - Exit
    "###);

    let mut this_and_that = this.clone();
    Apply::apply(&mut deserializer, &mut this_and_that).unwrap();

    assert_eq!(&this_and_that, &other);

    roundtrip(this, other);
}

#[test]
fn test_hashmap_int_struct() {
    use std::collections::HashMap;
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};
    type Hasher = BuildHasherDefault<DefaultHasher>;

    let mut this = HashMap::with_hasher(Hasher::default());

    this.insert(0, Simple { a: 0 });
    this.insert(20, Simple { a: 20 });
    this.insert(300, Simple { a: 300 });

    let mut other = this.clone();

    other.insert(20, Simple { a: 25 });
    other.remove(&300);
    other.insert(4000, Simple { a: 4000 });

    let diff = &Diff::serializable(&this, &other);
    let diff_str = serde_json::to_string(diff).unwrap();
    let mut deserializer = serde_json::Deserializer::from_str(&diff_str);

    assert_yaml_snapshot!(&this, @r###"
    ---
    0:
      a: 0
    20:
      a: 20
    300:
      a: 300
    "###);
    assert_yaml_snapshot!(&other, @r###"
    ---
    0:
      a: 0
    20:
      a: 25
    4000:
      a: 4000
    "###);
    assert_yaml_snapshot!(&diff, @r###"
    ---
    - EnterKey: 20
    - Enter:
        Field: a
    - Value: 25
    - Exit
    - RemoveKey: 300
    - AddKey: 4000
    - Value:
        a: 4000
    - Exit
    "###);

    let mut this_and_that = this.clone();
    Apply::apply(&mut deserializer, &mut this_and_that).unwrap();

    assert_eq!(&this_and_that, &other);
    // let diff = Diff::serializable(&this, &other);

    roundtrip(this, other);
}
