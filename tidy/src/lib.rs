#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.pass("ui/if.src");
    t.pass("ui/not.src");
    t.pass("ui/lazy_op.src");
    t.pass("ui/main.src");
    t.pass("ui/sample.src");
    t.pass("ui/never.src");
    t.pass("ui/fib.src");
    t.pass("ui/mut.src");

    // t.pass("ui/ref.src"); -- want infer casts

    // t.compile_fail("ui/max.src"); -- lint is not compile error now
    t.compile_fail("ui/empty.src");
    t.compile_fail("ui/never_fail.src");
    t.compile_fail("ui/redef.src");
    t.compile_fail("ui/mut_ref_collapse.src");
    t.compile_fail("ui/fn_args.src");

    {
        t.pass("ui/ptr/offset.src");
        t.pass("ui/ptr/memset.src");
    }

    {
        t.pass("ui/infer/int.src");

        t.compile_fail("ui/infer/int_err.src");
    }

    {
        t.compile_fail("ui/lints/arithmetic_overflow.src");
    }
}
