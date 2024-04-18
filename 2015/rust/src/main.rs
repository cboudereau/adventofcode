fn unfold<T, F, S>(seed: T, state: S, generator: F) -> T
where
    F: Fn(S, &T) -> Option<(S, T)>,
{
    let mut item = seed;
    let mut internal_state = state;
    while let Some((new_state, v)) = generator(internal_state, &item) {
        item = v;
        internal_state = new_state;
    }
    item
}

// this implementation uses an unfold approach which breaks as soon as bad strings is checked
fn part1_unfold() -> usize {
    fn is_vowel(x: &char) -> bool {
        const VOWELS: [char; 5] = ['a', 'e', 'i', 'o', 'u'];
        VOWELS.contains(x)
    }

    fn has_bad_strings(x: &[char; 2]) -> bool {
        const BAD_STRINGS: [[char; 2]; 4] = [['a', 'b'], ['c', 'd'], ['p', 'q'], ['x', 'y']];
        BAD_STRINGS.contains(x)
    }

    unfold(
        0,
        include_str!("../../day5.txt").split("\r\n"),
        |mut input_line, counter| {
            input_line.next().map(|line| {
                let (is_invalid, has_same_letter_twice, vowels_counter) = unfold(
                    (false, false, 0),
                    (None, line.chars()),
                    |(previous_char, mut chars),
                     (is_invalid, has_same_letter_twice, vowels_counter)| match (
                        is_invalid,
                        chars.next(),
                    ) {
                        (true, _) => None,
                        (false, None) => None,
                        (false, Some(c)) => {
                            let vowels_counter = if is_vowel(&c) { 1 } else { 0 } + vowels_counter;
                            let entry = previous_char
                                .map(|p| {
                                    let is_invalid = *is_invalid || has_bad_strings(&[p, c]);
                                    let has_same_letter_twice = *has_same_letter_twice || p == c;
                                    (is_invalid, has_same_letter_twice, vowels_counter)
                                })
                                .unwrap_or_else(|| {
                                    (*is_invalid, *has_same_letter_twice, vowels_counter)
                                });
                            let state = (Some(c), chars);
                            Some((state, entry))
                        }
                    },
                );
                let counter = if !is_invalid && has_same_letter_twice && vowels_counter > 2 {
                    1
                } else {
                    0
                } + counter;
                (input_line, counter)
            })
        },
    )
}

// this implementation uses a fold which does not break as soon as bad strings is checked
fn part1_fold() -> usize {
    let input = include_str!("../../day5.txt").split("\r\n");

    struct State {
        previous: Option<char>,
        vowels_counter: usize,
        has_same_letter_twice: bool,
        has_bad_string: bool,
    }

    impl State {
        fn zero() -> Self {
            Self {
                previous: None,
                vowels_counter: 0,
                has_same_letter_twice: false,
                has_bad_string: false,
            }
        }

        fn is_vowel(x: &char) -> bool {
            const VOWELS: [char; 5] = ['a', 'e', 'i', 'o', 'u'];
            VOWELS.contains(x)
        }

        fn has_same_letter_twice(&self, x: &char) -> bool {
            self.previous.map(|p| p == *x).unwrap_or(false)
        }

        fn has_bad_strings(&self, x: &char) -> bool {
            const BAD_STRINGS: [(char, char); 4] = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')];
            self.previous
                .map(|p| BAD_STRINGS.contains(&(p, *x)))
                .unwrap_or(false)
        }

        fn append(self, x: &char) -> Self {
            if self.has_bad_string {
                self
            } else {
                let vowels_counter = if Self::is_vowel(x) { 1 } else { 0 } + self.vowels_counter;
                let has_same_letter_twice =
                    self.has_same_letter_twice || Self::has_same_letter_twice(&self, x);
                let previous = Some(*x);
                let has_bad_string = self.has_bad_string || Self::has_bad_strings(&self, x);
                Self {
                    previous,
                    has_bad_string,
                    vowels_counter,
                    has_same_letter_twice,
                }
            }
        }

        fn is_valid(&self) -> bool {
            !self.has_bad_string && self.has_same_letter_twice && self.vowels_counter > 2
        }
    }

    input.fold(0, |acc, x| {
        if x.chars()
            .fold(State::zero(), |state, c| state.append(&c))
            .is_valid()
        {
            acc + 1
        } else {
            acc
        }
    })
}

fn part1_dummy() -> usize {
    let input = include_str!("../../day5.txt").split("\r\n");
    let mut counter = 0;
    for line in input {
        let mut previous: Option<char> = None;
        let mut vowels_counter: usize = 0;
        let mut has_one_same_letter_twice = false;
        let mut has_bad_strings = false;

        const VOWELS: [char; 5] = ['a', 'e', 'i', 'o', 'u'];
        for c in line.chars() {
            match previous {
                None => {
                    previous = Some(c);
                    if VOWELS.contains(&c) {
                        vowels_counter += 1;
                    }
                }
                Some(p) => {
                    previous = Some(c);
                    const BAD_STRINGS: [[char; 2]; 4] =
                        [['a', 'b'], ['c', 'd'], ['p', 'q'], ['x', 'y']];
                    if BAD_STRINGS.contains(&[p, c]) {
                        has_bad_strings = true;
                        break;
                    }
                    if VOWELS.contains(&c) {
                        vowels_counter += 1;
                    }

                    if !has_one_same_letter_twice {
                        has_one_same_letter_twice = p == c;
                    }
                }
            }
        }

        if vowels_counter > 2 && has_one_same_letter_twice && !has_bad_strings {
            counter += 1;
        }
    }

    counter
}

fn main() {
    assert_eq!(236, part1_dummy());
    assert_eq!(236, part1_fold());
    assert_eq!(236, part1_unfold());
}

#[test]
fn test_part1_dummy() {
    assert_eq!(236, part1_dummy())
}

#[test]
fn test_part1_fold() {
    assert_eq!(236, part1_fold())
}

#[test]
fn test_part1_unfold() {
    assert_eq!(236, part1_unfold())
}

#[test]
fn unfold_test() {
    let x = unfold(0, 10, |state, previous| {
        if state > 0 {
            Some((state - 1, previous + 1))
        } else {
            None
        }
    });

    assert_eq!(10, x)
}
