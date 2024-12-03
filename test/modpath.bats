setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
}

@test "Modpath exists" {
    $MODPATH
}

long_relative_and_absolute () (
    MODPATH="$(realpath $MODPATH)"
    cd /tmp                     # Because it's short and always there.
    $MODPATH --simple --path a:b:c --relative x --absolute y
)

@test "Long Relative and Absolute" {
    run long_relative_and_absolute
    assert_output "'a:b:c:x:/tmp/y'"
}

short_relative_and_absolute () (
    MODPATH="$(realpath $MODPATH)"
    cd /tmp                     # Because it's short and always there.
    $MODPATH --simple --path a:b:c -R x -A y
)

@test "Short Relative and Absolute" {
    run short_relative_and_absolute
    assert_output "'a:b:c:x:/tmp/y'"
}

@test "Long Before a" {
    run $MODPATH --simple --relative --path a:b:c --before a x
    assert_output "'x:a:b:c'"
}

@test "Long Before b" {
    run $MODPATH --simple --relative --path a:b:c --before b x
    assert_output "'a:x:b:c'"
}

@test "Long Before c" {
    run $MODPATH --simple --relative --path a:b:c --before c x
    assert_output "'a:b:x:c'"
}

@test "Long Before missing x" {
    run $MODPATH --simple --relative --path a:b:c --before x y
    assert_output "$MODPATH: warning: x is not in path to add y before it; adding at start
'y:a:b:c'"
}

@test "Short Before a" {
    run $MODPATH --simple --relative --path a:b:c -b a x
    assert_output "'x:a:b:c'"
}

@test "Short Before b" {
    run $MODPATH --simple --relative --path a:b:c -b b x
    assert_output "'a:x:b:c'"
}

@test "Short Before c" {
    run $MODPATH --simple --relative --path a:b:c -b c x
    assert_output "'a:b:x:c'"
}

@test "Short Before missing x" {
    run $MODPATH --simple --relative --path a:b:c -b x y
    assert_output "$MODPATH: warning: x is not in path to add y before it; adding at start
'y:a:b:c'"
}

@test "Long After a" {
    run $MODPATH --simple --relative --path a:b:c --after a x
    assert_output "'a:x:b:c'"
}

@test "Long After b" {
    run $MODPATH --simple --relative --path a:b:c --after b x
    assert_output "'a:b:x:c'"
}

@test "Long After c" {
    run $MODPATH --simple --relative --path a:b:c --after c x
    assert_output "'a:b:c:x'"
}

@test "Long After missing x" {
    run $MODPATH --simple --relative --path a:b:c --after x y
    assert_output "$MODPATH: warning: x is not in path to add y after it; adding at end
'a:b:c:y'"
}

@test "Short After a" {
    run $MODPATH --simple --relative --path a:b:c -a a x
    assert_output "'a:x:b:c'"
}

@test "Short After b" {
    run $MODPATH --simple --relative --path a:b:c -a b x
    assert_output "'a:b:x:c'"
}

@test "Short After c" {
    run $MODPATH --simple --relative --path a:b:c -a c x
    assert_output "'a:b:c:x'"
}

@test "Short After missing x" {
    run $MODPATH --simple --relative --path a:b:c -a x y
    assert_output "$MODPATH: warning: x is not in path to add y after it; adding at end
'a:b:c:y'"
}

@test "Cmd" {
    run $MODPATH --cmd --relative --path a:b:c x
    assert_output "path 'a:b:c:x'"
}

current () (
    MODPATH="$(realpath $MODPATH)"
    cd /tmp
    $MODPATH --simple --path a:b:c --current   
)
@test "Current" {
    run current
    assert_output "'a:b:c:/tmp'"
}
