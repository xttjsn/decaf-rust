/*
Graphviz representation for regex
 */
use dot;
use std::borrow::Cow;
use std::io::Write;
use std::iter::FromIterator;
use crate::regex::Regex;
use crate::regex::Regex::*;

type Nd<'a> = &'a Regex<char>;
type Ed<'a> = (&'a Regex<char>, &'a Regex<char>);

struct Graph<'a> {
	edges: Vec<Ed<'a>>,
	nodes: Vec<Nd<'a>>,
}

fn build_graph<'a>(g: &mut Graph<'a>, re: &'a Regex<char>) {
	match re {
		Null(_) | Empty(_)  => {
			// Leaf nodes
			g.nodes.push(re);
		}
		Chars(_, __) => {
			g.nodes.push(re);
		}
		Kleene(r, _) | Not(r, _) => {
			build_graph(g, r);
			g.edges.push((re, r)); // Add edge
		}
		Cat(rs, _) | Alt(rs, _) => {
			for r in rs {
				build_graph(g, r);
				g.edges.push((re, r)); // Add edge
			}
		}
	}
}

pub fn render_graph<'a, W: Write>(re: &'a Regex<char>, output: &mut W) {
	let mut g = Graph { edges:vec![], nodes: vec![] };
	build_graph(&mut g, re);
	dot::render(&g, output).unwrap();
}

impl<'a, 'b> dot::Labeller<'a, Nd<'b>, Ed<'b>> for Graph<'b> {
	fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new("regex_graph").unwrap() }

	fn node_id(&'a self, n: &Nd<'b>) -> dot::Id<'a> {
		match n {
			Null(id) => dot::Id::new(format!("Null_{}", id)).unwrap(),
			Empty(id) => dot::Id::new(format!("Empty_{}", id)).unwrap(),
			Chars(cs, id) => {
				let s = String::from_iter(cs);
				dot::Id::new(format!("Chars_{}_{}", id, s)).unwrap()
			}
			Kleene(_, id) => dot::Id::new(format!("Kleene_{}", id)).unwrap(),
			Cat(_, id) => dot::Id::new(format!("Cat_{}", id)).unwrap(),
			Not(_, id) => dot::Id::new(format!("Not_{}", id)).unwrap(),
			Alt(_, id) => dot::Id::new(format!("Alt_{}", id)).unwrap()
		}
	}
}

impl<'a, 'b> dot::GraphWalk<'a, Nd<'b>, Ed<'b>> for Graph<'b> {
	fn nodes(&self) -> dot::Nodes<'a, Nd<'b>> {
		let &Graph { ref edges, ref nodes } = self;
		let mut all_nodes = Vec::with_capacity(edges.len());
		for &(s, t) in edges {
			all_nodes.push(s); all_nodes.push(t);
		}
		for &n in nodes {
			all_nodes.push(n);
		}
		all_nodes.sort();
		all_nodes.dedup();
		Cow::Owned(all_nodes)
	}

	fn edges(&'a self) -> dot::Edges<'a, Ed<'b>> {
		let &Graph { ref edges, ref nodes } = self;
		Cow::Borrowed(&edges[..])
	}

	fn source(&self, e: &Ed<'b>) -> Nd<'b> { e.0 }

	fn target(&self, e: &Ed<'b>) -> Nd<'b> { e.1 }
}
