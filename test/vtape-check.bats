# -*- sh -*-
#
# Tests for Scripts/vtape-check.
#
# Run from the repository root:
#     ./test/vtape-check.sh
# or directly:
#     ./test/bats/bin/bats test/vtape-check.bats
#
# Set VTAPE_CHECK to test a different copy of the script.
#
# All fixtures are built in $BATS_TEST_TMPDIR, a fresh directory per test
# that bats creates and removes itself.

# For run --separate-stderr and BATS_TEST_TMPDIR.
bats_require_minimum_version 1.5.0

setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
    VTAPE_CHECK="${VTAPE_CHECK:-$BATS_TEST_DIRNAME/../Scripts/vtape-check}"
    T=$BATS_TEST_TMPDIR
}

# make_image NAME [crlf]
# Create $T/NAME.vtape.xz and $T/NAME.vtape.sha512sum.TEST, matching the
# format the Windows side produces ("<hash> *NAME.vtape").  With "crlf",
# the checksum file gets Windows line endings.
make_image () {
    local name=$1 eol=${2:-}
    head -c 200000 /dev/urandom > "$T/$name.vtape"
    local hash
    hash=$(sha512sum < "$T/$name.vtape" | awk '{print $1}')
    if [[ $eol == crlf ]]; then
        printf '%s *%s.vtape\r\n' "$hash" "$name" > "$T/$name.vtape.sha512sum.TEST"
    else
        printf '%s *%s.vtape\n' "$hash" "$name" > "$T/$name.vtape.sha512sum.TEST"
    fi
    xz "$T/$name.vtape"         # replaces NAME.vtape with NAME.vtape.xz
}

# Replace NAME's checksum with one that can't match.
break_checksum () {
    printf '%0128d *%s.vtape\n' 0 "$1" > "$T/$1.vtape.sha512sum.TEST"
}

@test "script exists and is executable" {
    assert [ -x "$VTAPE_CHECK" ]
}

@test "no arguments prints usage and exits 2" {
    run --separate-stderr "$VTAPE_CHECK"
    assert_failure 2
    [[ $stderr == *Usage:* ]]
}

@test "pass: announces PASS, deletes uncompressed copy, keeps .xz" {
    make_image good
    local before
    before=$(sha512sum < "$T/good.vtape.xz")
    run "$VTAPE_CHECK" "$T/good.vtape"
    assert_success
    assert_output --partial "PASS"
    assert [ ! -e "$T/good.vtape" ]
    assert [ -f "$T/good.vtape.xz" ]
    assert_equal "$(sha512sum < "$T/good.vtape.xz")" "$before"
}

@test "fail: announces FAIL with both hashes, keeps uncompressed copy, exits 1" {
    make_image bad
    break_checksum bad
    run --separate-stderr "$VTAPE_CHECK" "$T/bad.vtape"
    assert_failure 1
    [[ $stderr == *"FAIL"* ]]
    [[ $stderr == *"expected 0000"* ]]
    [[ $stderr == *"actual   "* ]]
    assert [ -f "$T/bad.vtape" ]
    assert [ -f "$T/bad.vtape.xz" ]
}

@test "checksum file with Windows CRLF line endings passes" {
    make_image crlf crlf
    run "$VTAPE_CHECK" "$T/crlf.vtape"
    assert_success
    assert_output --partial "PASS"
}

@test "checksum file with uppercase hash passes" {
    make_image upper
    local f="$T/upper.vtape.sha512sum.TEST"
    tr 'a-f' 'A-F' < "$f" > "$f.new" && mv "$f.new" "$f"
    run "$VTAPE_CHECK" "$T/upper.vtape"
    assert_success
}

@test "accepts the .xz name as the argument" {
    make_image byxz
    run "$VTAPE_CHECK" "$T/byxz.vtape.xz"
    assert_success
    assert_output --partial "PASS"
    assert [ ! -e "$T/byxz.vtape" ]
}

@test "works with a relative name from the current directory" {
    make_image rel
    cd "$T"
    run "$VTAPE_CHECK" rel.vtape
    assert_success
    assert [ ! -e rel.vtape ]
}

@test "handles a .bz2 compressed image" {
    head -c 200000 /dev/urandom > "$T/bz.vtape"
    printf '%s *bz.vtape\n' "$(sha512sum < "$T/bz.vtape" | awk '{print $1}')" \
           > "$T/bz.vtape.sha512sum.TEST"
    bzip2 "$T/bz.vtape"
    run "$VTAPE_CHECK" "$T/bz.vtape"
    assert_success
    assert [ -f "$T/bz.vtape.bz2" ]
    assert [ ! -e "$T/bz.vtape" ]
}

@test "name without .vtape is rejected with exit 2" {
    run --separate-stderr "$VTAPE_CHECK" "$T/something"
    assert_failure 2
    [[ $stderr == *"doesn't end in .vtape"* ]]
}

@test "missing compressed file exits 2" {
    printf '%0128d *nocomp.vtape\n' 0 > "$T/nocomp.vtape.sha512sum.TEST"
    run --separate-stderr "$VTAPE_CHECK" "$T/nocomp.vtape"
    assert_failure 2
    [[ $stderr == *"no compressed file"* ]]
}

@test "missing checksum file exits 2 and creates nothing" {
    make_image nosum
    rm "$T/nosum.vtape.sha512sum.TEST"
    run --separate-stderr "$VTAPE_CHECK" "$T/nosum.vtape"
    assert_failure 2
    [[ $stderr == *"found 0"* ]]
    assert [ ! -e "$T/nosum.vtape" ]
}

@test "two checksum files exit 2 and create nothing" {
    make_image twosum
    cp "$T/twosum.vtape.sha512sum.TEST" "$T/twosum.vtape.sha512sum.OTHER"
    run --separate-stderr "$VTAPE_CHECK" "$T/twosum.vtape"
    assert_failure 2
    [[ $stderr == *"found 2"* ]]
    assert [ ! -e "$T/twosum.vtape" ]
}

@test "checksum file without a SHA-512 hash exits 2" {
    make_image junk
    echo "not a hash" > "$T/junk.vtape.sha512sum.TEST"
    run --separate-stderr "$VTAPE_CHECK" "$T/junk.vtape"
    assert_failure 2
    [[ $stderr == *"no SHA-512 hash"* ]]
}

@test "existing uncompressed file is not overwritten" {
    make_image exists
    echo "precious" > "$T/exists.vtape"
    run --separate-stderr "$VTAPE_CHECK" "$T/exists.vtape"
    assert_failure 2
    [[ $stderr == *"already exists"* ]]
    assert_equal "$(cat "$T/exists.vtape")" "precious"
}

@test "corrupt .xz: FAIL, partial file removed, .xz kept" {
    make_image corrupt
    local size
    size=$(stat -c %s "$T/corrupt.vtape.xz")
    truncate -s $((size / 2)) "$T/corrupt.vtape.xz"
    run --separate-stderr "$VTAPE_CHECK" "$T/corrupt.vtape"
    assert_failure 1
    [[ $stderr == *"decompression"*"failed"* ]]
    assert [ ! -e "$T/corrupt.vtape" ]
    assert [ -f "$T/corrupt.vtape.xz" ]
}

@test "several images: each checked, exit status is the worst result" {
    make_image one
    make_image two
    break_checksum two
    make_image three
    run --separate-stderr "$VTAPE_CHECK" "$T/one.vtape" "$T/two.vtape" "$T/three.vtape"
    assert_failure 1
    [[ $output == *"PASS: $T/one.vtape"* ]]
    [[ $stderr == *"FAIL: $T/two.vtape"* ]]
    [[ $output == *"PASS: $T/three.vtape"* ]]
    assert [ ! -e "$T/one.vtape" ]
    assert [ -f "$T/two.vtape" ]
    assert [ ! -e "$T/three.vtape" ]
}

@test "setup error outranks a failed check in the exit status" {
    make_image fails
    break_checksum fails
    run "$VTAPE_CHECK" "$T/fails.vtape" "$T/missing.vtape"
    assert_failure 2
}
