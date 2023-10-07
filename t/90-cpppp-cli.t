#! /usr/bin/env perl
use Test2::V0;
use v5.20;
use FindBin;
use autodie;
use File::Temp;

my $tmp= File::Temp->newdir();
my $in_cpppp= "$tmp/source.cp";
my $out_c= "$tmp/out.c";
my $out_h= "$tmp/out.h";
sub slurp { open my $fh, '<', $_[0]; $/= undef; <$fh> }
sub spew { open my $fh, '>', $_[0]; $fh->print($_[1]); $fh->close; }
sub run_cpppp {
   my ($args, $data)= @_;
   open(my $cmd, '|-', $^X, "$FindBin::RealBin/../bin/cpppp", @$args);
   $cmd->print($data);
   $cmd->close;
   $?
}

subtest basic_output => sub {
   -e $_ && unlink $_ for $out_c, $out_h;
   is( run_cpppp([ -o => $out_c ], <<END), 0, 'exec cpppp' );
## for (0..2) {
#define THING_\$_ \$_
## }
END
   is( slurp($out_c), "#define THING_0 0\n#define THING_1 1\n#define THING_2 2\n" );
};

subtest split_output => sub {
   -e $_ && unlink $_ for $out_c, $out_h;
   is( run_cpppp([ '--section-out', "public=$out_h", -o => $out_c ], <<END), 0 );
## section PUBLIC;
int foo(int x);
## section PRIVATE;
int foo(int x) { return x + 1; }
END
   is( slurp($out_h), "int foo(int x);\n", 'out.h' );
   is( slurp($out_c), "int foo(int x) { return x + 1; }\n", 'out.c' );
};

done_testing;
