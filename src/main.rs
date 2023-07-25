use algebraic_graphs_rust::*;

fn main() {
    let x = (((vertex!(1) + empty!()) * vertex!(2) + vertex!(3)) * vertex!(4)).simplify();
    println!("{}", x);

    let vertex_set = x.vertex_set();
    let edge_set = x.edge_set();

    println!(
        "{}",
        vertex_set
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    );
    println!(
        "{}",
        edge_set
            .iter()
            .map(|(v1, v2)| format!("({}, {})", v1, v2))
            .collect::<Vec<String>>()
            .join(", ")
    );

    let x = Graph::clique(vec![1, 2, 3]).simplify();
    println!("{}", x);

    let x = (vertex!(1) * (vertex!(2) + vertex!(3)) + vertex!(2) * vertex!(3)).simplify();
    println!("{}", x);
}
