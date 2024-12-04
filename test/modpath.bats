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

@test "Csh" {
    run $MODPATH --csh --relative --path a:b:c x
    assert_output "setenv PATH 'a:b:c:x'"
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

@test "Long Delete" {
    run $MODPATH --simple --relative --path b:a:b:c:b:d:b:e:b --delete b
    assert_output "'a:c:d:e'"
}

@test "Short Delete" {
    run $MODPATH --simple --relative --path b:a:b:c:b:d:b:e:b -d  b
    assert_output "'a:c:d:e'"
}

@test "Long Delete of Missing Item" {
    run $MODPATH --simple --relative --path a:b:c --delete x
    assert_output "$MODPATH: warning: x is not in path to delete it
'a:b:c'"
}

@test "Short Delete of Missing Item" {
    run $MODPATH --simple --relative --path a:b:c -d  x
    assert_output "$MODPATH: warning: x is not in path to delete it
'a:b:c'"
}

@test "Long Empty" {
    run $MODPATH --simple --relative --path a:b:c --empty
    assert_output "'a:b:c:'"
}

@test "Short Empty" {
    run $MODPATH --simple --relative --path a:b:c -E
    assert_output "'a:b:c:'"
}

@test "Long Empty before" {
    run $MODPATH --simple --relative --path a:b:c --before b --empty
    assert_output "'a::b:c'"
}

@test "Short Empty before" {
    run $MODPATH --simple --relative --path a:b:c -b b -E
    assert_output "'a::b:c'"
}

@test "Long Empty after" {
    run $MODPATH --simple --relative --path a:b:c --after b --empty
    assert_output "'a:b::c'"
}

@test "Short Empty after" {
    run $MODPATH --simple --relative --path a:b:c -a b -E
    assert_output "'a:b::c'"
}

@test "Long end" {
    run $MODPATH --simple --relative --path a:b:c --end x
    assert_output "'a:b:c:x'"
}


@test "Short end" {
    run $MODPATH --simple --relative --path a:b:c -e x
    assert_output "'a:b:c:x'"
}

exists_actually_exists () (
    MODPATH="$(realpath $MODPATH)"
    DIR=$(mktemp -d)
    mkdir -p $DIR/x
    cd $DIR
    $MODPATH --simple --relative --path a:b:c --exists x
    cd ..
    rm -rf $DIR
)

@test "Exists actually Exists" {
    run exists_actually_exists
    assert_output "'a:b:c:x'"
}


exists_does_not_exist () (
    DIR=$(mktemp -d)
    cd $DIR
    $MODPATH --simple --relative --path a:b:c --exists x
    cd ..
    rm -rf $DIR
)

@test "Exists does not exist" {
    MODPATH="$(realpath $MODPATH)"
    run exists_does_not_exist
    assert_output "$MODPATH: warning: pathname does not exist: x
'a:b:c'"
}

@test "Input Separator" {
    run $MODPATH --simple --relative --insep ';' --path 'a;b;c'
    assert_output "'a:b:c'"
}

#@test "msys" {
#    run $MODPATH --simple --relative --path a:b:c --msys
#}

long_ivar () (
    XXX=q:r:s $MODPATH --warnings --relative --path a:b:c --ivar XXX x
)

@test "Long ivar" {
    run long_ivar
    assert_output "PATH='q:r:s:x'
export PATH"
}

short_ivar () (
    XXX=q:r:s $MODPATH --warnings --relative --path a:b:c -I XXX x
)

@test "Short ivar" {
    run short_ivar
    assert_output "PATH='q:r:s:x'
export PATH"
}

@test "Long name" {
    run $MODPATH --warnings --relative --path a:b:c --name OUTPATH
    assert_output "OUTPATH='a:b:c'
export OUTPATH"
}

@test "Short name" {
    run $MODPATH --warnings --relative --path a:b:c -n OUTPATH
    assert_output "OUTPATH='a:b:c'
export OUTPATH"
}

@test "Nice" {
    run $MODPATH --relative --path a:b:c --nice
    assert_output "a
b
c"
}

@test "Long outsep" {
    run $MODPATH --relative --path a:b:c --outsep /
    assert_output "PATH='a/b/c'
export PATH"
}

@test "Short outsep" {
    run $MODPATH --relative --path a:b:c -o /
    assert_output "PATH='a/b/c'
export PATH"
}

@test "Long Path" {
    run $MODPATH --simple --relative --path a:b:c
    assert_output "'a:b:c'"
}

@test "Short Path" {
    run $MODPATH --simple --relative -p a:b:c
    assert_output "'a:b:c'"
}

@test "Quiet" {
    run $MODPATH --quiet
    cat /dev/null | assert_output -
}
