use super::{Context, new_tal_context};

#[test]
fn infer1() {
    let _ = env_logger::try_init();
    let mut ctx = new_tal_context();

    let f = ctx.exec("forall x Holds(0, is-open(), x);")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("bool", vec![]));
}

#[test]
fn infer2() {
    let _ = env_logger::try_init();
    let mut ctx = new_tal_context();

    let f = ctx.exec("exists t, u, l Holds(t+5, loc(u), l);")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("timepoint", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("uav", vec![]));
    ctx.assert_variable_type(&f, (0, 2), &ctx.build_active_type("location", vec![]));
}

#[test]
fn infer3() {
    let _ = env_logger::try_init();
    let mut ctx = new_tal_context();

    let f = ctx.exec("exists t, u Holds(t+5, foo(u), l1);")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("timepoint", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("location", vec![]));
}

#[test]
fn infer4() {
    let _ = env_logger::try_init();
    let mut ctx = new_tal_context();

    let f = ctx.exec("exists t, u, a Holds(t+5, foo3(u, u1, a), l1);")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("timepoint", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("location", vec![]));
    ctx.assert_variable_type(&f, (0, 2), &ctx.build_active_type("uav", vec![]));
}

#[test]
fn infer5() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[A,B] f(A,B);

:predicate P(A, B, t[A,B]);

forall x,y P(x,y,f(x,y));
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("a", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("b", vec![]));
}

#[test]
fn infer6() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[C,D] f(C,D);

:predicate P(A, B, t[A,B]);

:instances a ai {a1,a2};

forall x,y P(x,y,f(a1,y));
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("b", vec![]));
}

#[test]
fn infer7() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[C,D] f(C,D);

:predicate P(A, B, t[A,B]);

:instances a ai {a1,a2};
:instances b bi {b1};

forall x,y P(x,b1,f(a1,y));
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("bi", vec![]));
}

#[test]
fn infer8() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[C,D] f(C,D);
:function E g(E);

:predicate P(A, B, t[A,B]);

:instances a ai {a1,a2};
:instances b bi {b1};

forall x,y P(x,b1,f(a1,g(y)));

")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("bi", vec![]));
}

#[test]
fn infer9() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;

:type t[a,b];

:function t[C,D] f(C,D);
:function E g(E);

:instances a ai {a1,a2};
:instances b bi {b1};

:predicate P(A, B, t[A,B]);
:predicate Q(ai);

forall x,y,z P(x,b1,f(z,g(y))) & Q(x);
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("bi", vec![]));
    ctx.assert_variable_type(&f, (0, 2), &ctx.build_active_type("ai", vec![]));
}

#[test]
fn infer10() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[C,D] f(C,D);
:function E g(E);

:instances a ai {a1,a2};
:instances b bi {b1};

:predicate P(A, B, t[A,B]);
:predicate Q(A, A);

forall x Q(g(g(x)),a1);
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
}

#[test]
fn infer11() {
    let _ = env_logger::try_init();
    let mut ctx = Context::new();

    let f = ctx.exec(r"
:type a;
:type b;
:type t[a,b];

:function t[C,D] f(C,D);
:function E g(E);

:instances a ai {a1,a2};
:instances b bi {b1};

:predicate P(A, B, t[A,B]);
:predicate Q(A, A);

forall x, y [ P(x, y, f(a1, b1)) -> (exists z Q(z, x)) ];
")
        .expect("Could not emit formula");

    ctx.assert_variable_type(&f, (0, 0), &ctx.build_active_type("ai", vec![]));
    ctx.assert_variable_type(&f, (0, 1), &ctx.build_active_type("bi", vec![]));
    ctx.assert_variable_type(&f, (1, 0), &ctx.build_active_type("ai", vec![]));
}
