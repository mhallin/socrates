use crate::gaf::GroundTerm;

use super::new_tal_context;

fn assert_instances(expected: &[GroundTerm<'_>], mut actual: Vec<GroundTerm<'_>>) {
    let mut expected = expected.to_vec();

    expected.sort();
    actual.sort();

    assert_eq!(expected, actual);
}

#[test]
fn iterate_interpreted() {
    let ctx = new_tal_context();

    assert_instances(
        &[
            GroundTerm::new_ident("false"),
            GroundTerm::new_ident("true"),
        ],
        ctx.types.instance_iter(ctx.get_type("bool")),
    );
}

#[test]
fn iterate_supertype() {
    let ctx = new_tal_context();

    assert_instances(
        &[
            GroundTerm::new_ident("false"),
            GroundTerm::new_ident("true"),
            GroundTerm::new_ident("l1"),
            GroundTerm::new_ident("u1"),
        ],
        ctx.types.instance_iter(ctx.get_type("value")),
    );
}

#[test]
fn iterate_number() {
    let mut ctx = new_tal_context();

    ctx.exec_unit(
        r"
:integertype t [3..9];
",
    )
    .expect("Could not set up integer type");

    assert_instances(
        &[
            GroundTerm::Number(3),
            GroundTerm::Number(4),
            GroundTerm::Number(5),
            GroundTerm::Number(6),
            GroundTerm::Number(7),
            GroundTerm::Number(8),
            GroundTerm::Number(9),
        ],
        ctx.types.instance_iter(ctx.get_type("t")),
    );
}
