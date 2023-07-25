use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::iter;

macro_rules! set_extends {
    ($( $x:expr ),*) => {{
        let mut set = HashSet::new();
        $(set.extend($x);)*
        set
    }};
}

#[derive(Debug, Clone)]
pub enum Graph<T: PartialEq + Eq + Hash + Clone + Display + Debug> {
    Empty,
    Vertex(T),
    Overlay(Box<Graph<T>>, Box<Graph<T>>),
    Connect(Box<Graph<T>>, Box<Graph<T>>),
}

impl<T: PartialEq + Eq + Hash + Clone + Display + Debug> Graph<T> {
    pub fn empty() -> Self {
        Graph::Empty
    }

    pub fn vertex(v: T) -> Self {
        Graph::Vertex(v)
    }

    pub fn vertices(vs: Vec<T>) -> Self {
        Self::overlays(vs.into_iter().map(|value| Self::vertex(value)).collect())
    }

    pub fn overlay(self, y: Self) -> Self {
        Graph::Overlay(Box::new(self), Box::new(y))
    }

    pub fn overlays(xs: Vec<Graph<T>>) -> Self {
        xs.into_iter().fold(Graph::empty(), |y, x| x + y)
    }

    pub fn connect(self: Self, y: Self) -> Self {
        Graph::Connect(Box::new(self), Box::new(y))
    }

    pub fn connects(xs: Vec<Graph<T>>) -> Self {
        xs.into_iter().fold(Graph::empty(), |y, x| y * x)
    }

    pub fn clique(vs: Vec<T>) -> Self {
        Self::connects(vs.iter().map(|v| vertex!(v.clone())).collect())
    }

    pub fn foldg<U>(
        x: Graph<T>,
        empty: &dyn Fn() -> U,
        vertex: &dyn Fn(T) -> U,
        overlay: &dyn Fn(U, U) -> U,
        connect: &dyn Fn(U, U) -> U,
    ) -> U {
        match x {
            Graph::Empty => empty(),
            Graph::Vertex(v) => vertex(v),
            Graph::Overlay(y, z) => overlay(
                Graph::foldg(*y, empty, vertex, overlay, connect),
                Graph::foldg(*z, empty, vertex, overlay, connect),
            ),
            Graph::Connect(y, z) => connect(
                Graph::foldg(*y, empty, vertex, overlay, connect),
                Graph::foldg(*z, empty, vertex, overlay, connect),
            ),
        }
    }

    fn simple(x: Graph<T>, y: Graph<T>, op: &dyn Fn(Graph<T>, Graph<T>) -> Graph<T>) -> Graph<T> {
        let z = op(x.clone(), y.clone());

        if x == z {
            x
        } else if y == z {
            y
        } else {
            z
        }
    }

    pub fn simplify(&self) -> Graph<T> {
        Self::foldg(
            self.clone(),
            &Self::empty,
            &Self::vertex,
            &|y, z| Self::simple(y, z, &Self::overlay),
            &|y, z| Self::simple(y, z, &Self::connect),
        )
    }

    pub fn vertex_set(&self) -> HashSet<T> {
        Self::foldg(
            self.clone(),
            &|| HashSet::new(),
            &|x| HashSet::from([x]),
            &|x, y| set_extends!(x, y),
            &|x, y| set_extends!(x, y),
        )
    }

    pub fn edge_set(&self) -> HashSet<(T, T)> {
        match self {
            Graph::Empty => HashSet::new(),
            Graph::Vertex(_) => HashSet::new(),
            Graph::Overlay(x, y) => set_extends!(x.edge_set(), y.edge_set()),
            Graph::Connect(x, y) => set_extends!(
                x.edge_set(),
                y.edge_set(),
                x.vertex_set()
                    .iter()
                    .flat_map(|vx| iter::repeat(vx.clone()).zip(y.vertex_set()))
            ),
        }
    }
}

impl<T: PartialEq + Eq + Hash + Clone + Display + Debug> std::ops::Add for Graph<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.overlay(other)
    }
}

impl<T: PartialEq + Eq + Hash + Clone + Display + Debug> std::ops::Mul for Graph<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        self.connect(other)
    }
}

impl<T: PartialEq + Eq + Hash + Clone + Display + Debug> Display for Graph<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Graph::*;

        match self {
            Empty => write!(f, "ε"),
            Vertex(v) => write!(f, "{}", v),
            Overlay(x, y) => write!(f, "{} + {}", x, y),
            Connect(x, y) => match (x.as_ref(), y.as_ref()) {
                (Overlay(_, _), Overlay(_, _)) => write!(f, "({}) * ({})", x, y),
                (Overlay(_, _), _) => write!(f, "({}) * {}", x, y),
                (_, Overlay(_, _)) => write!(f, "{} * ({})", x, y),
                (_, _) => write!(f, "{} * {}", x, y),
            },
        }
    }
}

impl<T: PartialEq + Eq + Hash + Clone + Display + Debug> PartialEq for Graph<T> {
    fn eq(&self, other: &Self) -> bool {
        self.vertex_set() == other.vertex_set() && self.edge_set() == other.edge_set()
    }
}

#[macro_export]
macro_rules! empty {
    () => {
        Graph::empty()
    };
}

#[macro_export]
macro_rules! vertex {
    ($value:expr) => {
        Graph::vertex($value)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let x: Graph<i32> = empty!() + empty!() * empty!();
        assert_eq!(x, Graph::Empty);
    }

    #[test]
    fn connect() {
        let x = vertex!(1) * (vertex!(2) + vertex!(3));
        assert_eq!(x, vertex!(1) * vertex!(2) + vertex!(1) * vertex!(3));
    }

    #[test]
    fn clique() {
        let x = Graph::clique(vec![1, 2, 3]);
        assert_eq!(
            x,
            vertex!(1) * (vertex!(2) + vertex!(3)) + vertex!(2) * vertex!(3)
        );
    }

    #[test]
    fn simplify() {
        let x = vertex!(1) + vertex!(1);
        let y = x.simplify();

        match y {
            Graph::Vertex(v) => assert_eq!(v, 1),
            _ => assert!(false),
        }
    }

    #[test]
    fn edge_set() {
        let x = vertex!(1) * (vertex!(2) + vertex!(3));
        let y = x.edge_set();

        assert_eq!(y, HashSet::from([(1, 2), (1, 3)]));
    }
}
