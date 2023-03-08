#!/usr/bin/perl

foreach $arg (@ARGV) {
    rldd($arg);
}

sub rldd {
    $o = $_[0];
    return if $done{$o};
    ++$done{$o};
    open(LDD, "ldd $o |");
    while (<LDD>) {
	print;
	++$do{$1} if /=> ([^ ]+)/ && !$done{$1};
    }
    close(LDD);
    foreach $o (keys %do) {
	rldd($o);
    }
}
exit 0;

=head1 Orignal netnews article

From: per@hedeland.org (Per Hedeland)
Newsgroups: comp.unix.bsd.freebsd.misc
Subject: Re: Gnu and Gnome library hell...?
Organization: None
Lines: 49
Message-ID: <c01kfh$2foc$1@hedeland.org>
References: <W7CdnSd-Zf7cnoHdRVn-gw@adelphia.com> <mMqdnUM8E9VOjoLdRVn_iw@adelphia.com> <pan.2004.02.03.20.16.03.356307@ntlworld.com> <ArGdnbaIVuFwOrzdRVn-iQ@adelphia.com>
X-Newsreader: trn 4.0-test76 (Apr 2, 2001)
Originator: per@hedeland.org (Per Hedeland)
Date: Sat, 7 Feb 2004 03:02:09 +0000 (UTC)
NNTP-Posting-Host: 195.149.129.26
X-Complaints-To: news@ecrc.de
X-Trace: news.ecrc.de 1076123107 195.149.129.26 (Sat, 07 Feb 2004 04:05:07 MET)
NNTP-Posting-Date: Sat, 07 Feb 2004 04:05:07 MET
Path: corp.newsgroups.com!propagator5-maxim!propagator2-maxim!feed-maxim.newsfeeds.com!newsfeed.icl.net!newsfeed.fjserv.net!news-FFM2.ecrc.net!news.ecrc.de!not-for-mail
Xref: corp comp.unix.bsd.freebsd.misc:10246

In article <ArGdnbaIVuFwOrzdRVn-iQ@adelphia.com> Ben Crowell
<see@my.sig> writes:
>>> Hmm...but I compiled rubrica /after/ upgrading the library, so doesn't
>>> that indicate the rubrica port is just plain broken?
>> 
>> Not really.
>> 
>> If rubrica depends upon a library that also depends upon the old library,
>> then the dependency still gets sucked in to rubrica. The library with the
>> hanging dependency needs to be recompiled in order to work properly.
>> Rubrica may just work after that or it may need to be rebuilt.
>
>Interesting! Any suggestion as to how I could go about detecting whether
>that's the actual situation? I guess the error message I'm getting is
>from the loader...dunno if the loader has enough information to be
>able to tell which shared library was requesting the shared library
>that wasn't there.

It has to have enough info, it's the one that's hunting them all down
through the chain of dependencies. That doesn't mean that it will
actually tell though.:-) But you can do what it does with 'ldd' - start
with ldd on the binary, then ldd on each of the libraries listed, etc.
Something you'd really rather have the computer do for you... - see
below.:-)

--Per Hedeland
per@hedeland.org

rldd.pl------------------------------
#!/usr/bin/perl

foreach $arg (@ARGV) {
    rldd($arg);
}

sub rldd {
    $o = $_[0];
    return if $done{$o};
    ++$done{$o};
    open(LDD, "ldd $o |");
    while (<LDD>) {
	print;
	++$do{$1} if /=> ([^ ]+)/ && !$done{$1};
    }
    close(LDD);
    foreach $o (keys %do) {
	rldd($o);
    }
}

=cut
