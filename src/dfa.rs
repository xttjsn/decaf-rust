use derivatives::Differentiable;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use vec_map::VecMap;
use bit_set::BitSet;

#[derive(Debug, Clone)]
pub struct State<T, V> {
	pub by_char: BTreeMap<T, u32>,
	pub default: u32,
	pub value: V
}

#[derive(Debug, Clone)]
pub struct Dfa<T, V> {
	pub states: Vec<State<T, V>>,
}

pub trait Normalize {
	fn normalize(self) -> Self;
}

impl<R: Normalize> Normalize for Vec<R> {
	fn normalize(self) -> Self {
		self.into_iter().map(Normalize::normalize).collect()
	}
}

impl<T, V> Dfa<T, V> {
	pub fn from_derivatives(initial: Vec<V>) -> (Dfa<T, V>, BTreeMap<V, u32>)
		where T: Ord,
			  V: Differentiable<T> + Normalize + Ord + Clone {

		fn index<V: Ord + Clone>(&mut (ref mut indices, ref mut next): &mut (BTreeMap<V, u32>, VecDeque<V>), re: V) -> u32 {
			let next_index = indices.len() as u32;
			*indices.entry(re.clone())
				.or_insert_with(|| {
					next.push_back(re);
					next_index
				})
		}

		let mut result = Dfa { states: Vec::new() };
		let mut worklist = (BTreeMap::new(), VecDeque::new());

		for r in initial {
			index(&mut worklist, r.normalize());
		}

		while let Some(re) = worklist.1.pop_front() {
			let d = re.derivative();
			let mut by_char = BTreeMap::new();
			let default = index(&mut worklist, d.rest.normalize());
			for (chars, dre) in d.m {
				let ix = index(&mut worklist, dre.normalize());
				if ix != default {
					for ch in chars {
						by_char.insert(ch, ix);
					}
				}
			}
			result.states.push(State {
				by_char: by_char,
				default: default,
				value: re
			})
		}

		(result, worklist.0)
	}


	pub fn map<U, F>(self, mut f: F) -> Dfa<T, U>
	where F: FnMut(V) -> U {
		Dfa {
			states: self.states.into_iter().map(|state| State {
				by_char: state.by_char,
				default: state.default,
				value: f(state.value),
			}).collect()
		}
	}


	/// Find the reverse transition for each state in the DFA.
	pub fn reverse(&self) -> Vec<(BTreeMap<&T, BTreeSet<usize> >, BTreeSet<usize>)>
	where T: Ord {
		// Each pair is
		let mut result = vec![(BTreeMap::new(), BTreeSet::new()); self.states.len()];
		for (state_ix, state) in self.states.iter().enumerate() {
			// Map state to char
			let mut rev: BTreeMap<usize, BTreeSet<_>> = BTreeMap::new();
			for (by, &to) in &state.by_char {
				rev.entry(to as usize).or_insert_with(BTreeSet::new).insert(by);
			}
			for (&to, by) in &rev {
				for what in by {
					let &mut (ref mut trans, ref deafult) = &mut result[to];
					trans.entry(*what).or_insert_with(|| default.clone()).insert(state_ix);
				}
			}
			let &mut (ref mut trans, ref mut default) = &mut result[state.default as usize];
			for c in state.by_char.keys() {
				trans.entry(c).or_insert_with(|| default.clone());
			}

			for (key, ref mut val) in trans {
				if !state.by_char.contains_key(key) {
					val.insert(state_ix);
				}
			}

			default.insert(state_ix);
		}
		result
	}


	pub fn minimize(&self) -> Dfa<T, &V>
	where T: Ord + Clone, V: Ord {

		assert!(!self.states.is_empty());

		// `partitions` is a partition of the DFA states, representing the
        // current set of equivalence classes
		let mut partitions: Vec<BTreeSet<usize>> = vec![];
		{
			// Calculate the initial partition. The choice of initial partition
            // determines what states the algorithm considers distinguishable.
            // We only consider states that are reachable from the starting
            // state, so we traverse the DFA to find these states.
			let mut initial_partition = BTreeMap::new();
            let mut worklist = VecDeque::new();
            let mut seen = BitSet::new();
			worklist.push_back(0);
			seen.insert(0);
			while let Some(state_ix) = worklist.pop_front() {
				let state = &self.states[state_ix];
				let part = *initial_partition.entry(&state.value)
                    .or_insert_with(|| {
                        let ix = partitions.len();
                        partitions.push(BTreeSet::new());
                        ix
                    });
				partitions[part].insert(state_ix);
				for &next in state.by_char.values()
					.chain(Some(&state.default).into_iter()) {
						if seen.insert(next as usize) {
							worklist.push_back(next as usize);
						}
					}
			}
		}
		debug_assert!(partitions[0].contains(&0));

		let preimage = self.reverse();
		let mut worklist: BTreeSet<usize> = (0..partitions.len()).collect();
		while let Some(&cur_ix) = worklist.iter().next() {
			worklist.remove(&cur_ix);
			let chars: BTreeSet<&T> = partitions[cur_ix].iter().flat_map(|&state| preimages[state].keys().cloned()).collect();
			for c in chars
		}
	}
}
