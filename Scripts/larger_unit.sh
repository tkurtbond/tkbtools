function larger_unit () {
    local var1=$1 var2=$2 dividend=$3 divisor=$4 quotient remainder
    ((quotient=dividend / divisor)) # number of bigger unit
    ((remainder=dividend % divisor)) # smaller units remaining
    printf '%s=%s %s=%s\n' $var1 $quotient $var2 $remainder
}

function test_larger_unit () {
    let days1=2 hours1=3 minutes1=23 seconds1=15
    ((seconds=seconds1 + (minutes1 * 60) + (hours1 * 60 * 60)
	+ (days1 * 24 * 60 * 60)))
    printf "total seconds: %d\n" $seconds
    eval $(larger_unit minutes seconds $seconds 60)
    eval $(larger_unit hours minutes $minutes 60)
    eval $(larger_unit days hours $hours 24)

    printf "original:  days: %d hours: %d minutes: %d seconds: %d\n" \
	$days1 $hours1 $minutes1 $seconds1
    printf "calulated: days: %d hours: %d minutes: %d seconds: %d\n" \
	$days $hours $minutes $seconds
}
