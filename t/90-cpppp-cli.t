#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use v5.20;
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

subtest splice_output_into_file => sub {
   -e $_ && unlink $_ for $out_c, $out_h;
   spew($out_c, <<END);
Line 1
Line 2
// BEGIN GENERATED_TEXT
// END GENERATED_TEXT
Line 5
END
   is( run_cpppp([ '--section-out', 'public='.$out_c.'@GENERATED_TEXT' ], <<END), 0 );
## section PUBLIC;
Injected line 1
Injected line 2
## section PRIVATE;
Injected line 3
END
   is( slurp($out_c), <<END, 'out.c' );
Line 1
Line 2
// BEGIN GENERATED_TEXT
Injected line 1
Injected line 2
// END GENERATED_TEXT
Line 5
END
   is( run_cpppp([ '-o', $out_c.'@GENERATED_TEXT' ], ''), 0 );
   is( slurp($out_c), <<END, 'out.c' );
Line 1
Line 2
// BEGIN GENERATED_TEXT
// END GENERATED_TEXT
Line 5
END
};

done_testing;
