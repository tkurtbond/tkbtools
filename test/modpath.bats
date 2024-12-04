# -*- sh -*-
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

exists_actually_exists_before_b () (
    MODPATH="$(realpath $MODPATH)"
    DIR=$(mktemp -d)
    mkdir -p $DIR/x
    cd $DIR
    $MODPATH --simple --relative --path a:b:c --before b --exists x
    cd ..
    rm -rf $DIR
)

# This test fails on all three variants, because it doesn't actually put X
# before b.
@test "Exists actually Exists before b" {
    #skip
    run exists_actually_exists_before_b
    assert_output "'a:x:b:c'"
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

@test "Long ivar nonexistant" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --ivar VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: error: unable to get path from environment variable VAR_DOES_NOT_EXIST"
    )
}

@test "Short ivar nonexistant" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH -I VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: error: unable to get path from environment variable VAR_DOES_NOT_EXIST"
    )
}

@test "Long ivar nonexistant warnings" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --warnings --ivar VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: warning: unable to get path from environment variable VAR_DOES_NOT_EXIST
PATH=''
export PATH"
    )
}

@test "Short ivar nonexistant warnings" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --warnings -I VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: warning: unable to get path from environment variable VAR_DOES_NOT_EXIST
PATH=''
export PATH"
    )
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

@test "Long relative" {
    run $MODPATH --simple --relative --path a:b:c x
    assert_output "'a:b:c:x'"
}

@test "Short relative" {
    run $MODPATH --simple -R --path a:b:c x
    assert_output "'a:b:c:x'"
}

@test "Long sep" {
    run $MODPATH --simple --relative --sep / --path a/b/c x
    assert_output "'a/b/c/x'"
}

@test "Short sep" {
    run $MODPATH --simple --relative -S / --path a/b/c x
    assert_output "'a/b/c/x'"
}

@test "Sh" {
    run $MODPATH --relative --path a:b:c x
    assert_output "PATH='a:b:c:x'
export PATH"
}

@test "Simple" {
    run $MODPATH --simple --relative --path a:b:c x
    assert_output "'a:b:c:x'"
}

@test "Long Start" {
    run $MODPATH --simple --relative --path a:b:c --start x
    assert_output "'x:a:b:c'"
}

@test "Short Start" {
    run $MODPATH --simple --relative --path a:b:c -s x
    assert_output "'x:a:b:c'"
}

@test "Long Unique" {
    run $MODPATH --simple --relative --path a:1:b:1:c:1:1 --unique
    assert_output "'a:1:b:c'"
}

@test "Short Unique" {
    run $MODPATH --simple --relative --path a:1:b:1:c:1:1 -u
    assert_output "'a:1:b:c'"
}

long_var () (
    VAR=q:r:s $MODPATH --var VAR
)

@test "Long var" {
    run long_var
    assert_output "VAR='q:r:s'
export VAR"
}

short_var () (
    VAR=q:r:s $MODPATH -v VAR
)

@test "Short var" {
    run short_var
    assert_output "VAR='q:r:s'
export VAR"
}

@test "Long var nonexistant" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --var VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: error: unable to get path from environment variable VAR_DOES_NOT_EXIST"
    )
}

@test "Short var nonexistant" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH -v VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: error: unable to get path from environment variable VAR_DOES_NOT_EXIST"
    )
}


@test "Long var nonexistant warning" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --warnings --var VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: warning: unable to get path from environment variable VAR_DOES_NOT_EXIST
VAR_DOES_NOT_EXIST=''
export VAR_DOES_NOT_EXIST"
    )
}

@test "Short var nonexistant warning" {
    (
        unset VAR_DOES_NOT_EXIST
        run $MODPATH --warnings -v VAR_DOES_NOT_EXIST
        assert_output "$MODPATH: warning: unable to get path from environment variable VAR_DOES_NOT_EXIST
VAR_DOES_NOT_EXIST=''
export VAR_DOES_NOT_EXIST"
    )
}

# -V/--version don't don't give the same answers right now, so don't test them.
