program vax_redim_best
    option type = explicit, constant type = integer, &
        size = integer long, size = real double 
    record rec
        string s = 13
	long   i
    end record rec     

    ! Don't declare the parameters, and everything works, even passing
    ! different types.
    external sub redim 

    declare long stat, i
    i = 1
    dim rec rec(1 to i)
    print "lbound (rec) at start: "; lbound (rec)
    print "ubound (rec) at start: "; ubound (rec)
    call redim (rec(), 3, 10)
    print "lbound (rec) after redim: "; lbound (rec)
    print "ubound (rec) after redim: "; ubound (rec)

    dim string s(1 to i)
    print "lbound (s) at start: "; lbound (s)
    print "ubound (s) at start: "; ubound (s)
    call redim (s(), 3, 10)
    print "lbound (s) after redim: "; lbound (s)
    print "ubound (s) after redim: "; ubound (s)

    dim long l(1 to i)
    print "lbound (l) at start: "; lbound (l)
    print "ubound (l) at start: "; ubound (l)
    call redim (l(), 3, 10)
    print "lbound (l) after redim: "; lbound (l)
    print "ubound (l) after redim: "; ubound (l)


end program ! vax_redim_best

function long redim (long s, long low, long high)
    option type = explicit, constant type = integer, &
        size = integer long, size = real double
    external long function bas$rt_dim_bounds (long by value, long by value, long by value)
    declare long r
    r = bas$rt_dim_bounds (loc (s), low, high)
end function r ! redim
