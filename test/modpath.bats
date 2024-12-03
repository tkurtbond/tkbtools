setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
}

@test "Modpath exists" {
    $MODPATH
}

@test "Before a" {
    run $MODPATH --simple --relative --path a:b:c --before a x
    assert_output "'x:a:b:c'"
}

@test "Before b" {
    run $MODPATH --simple --relative --path a:b:c --before b x
    assert_output "'a:x:b:c'"
}

@test "Before c" {
    run $MODPATH --simple --relative --path a:b:c --before c x
    assert_output "'a:b:x:c'"
}


@test "After a" {
    run $MODPATH --simple --relative --path a:b:c --after a x
    assert_output "'a:x:b:c'"
}

@test "After b" {
    run $MODPATH --simple --relative --path a:b:c --after b x
    assert_output "'a:b:x:c'"
}

@test "After c" {
    run $MODPATH --simple --relative --path a:b:c --after c x
    assert_output "'a:b:c:x'"
}
