#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use v5.20;

use CodeGen::Cpppp::AntiCharacter;

my $anticomma= CodeGen::Cpppp::AntiCharacter->new(qr/,/, qr/\s*/);
my $antispace= CodeGen::Cpppp::AntiCharacter->new(qr/\s*/);

subtest basic_concat => sub {
   is( (',' . $anticomma), '' );
   is( (', ' . $anticomma), ' ' );
   is( (",\n\t" . $anticomma), "\n\t" );
   is( ("\n,\n,\n,\n" . $anticomma), "\n,\n,\n\n" );
   is( ("foo\n" . $antispace), "foo" );
   is( ("foo\n    " . $antispace), "foo" );
};

subtest interpolation => sub {
   my $x= "a,b,";
   my $y= "c,d,";
   is( "$x$y$anticomma", "a,b,c,d" );
   $y= '';
   is( "$x$y$anticomma", "a,b" );
};

# When used in templates, there are cases where an anticharacter gets appended
# to a template fragment, and then that fragment gets appended to a string
# as part of another fragment, which finally gets appended to the main text
# being built, and the anticharacter needs to survive the concatenation so that
# it can remove something from the main text.
subtest multi_concat => sub {
   is( ("," . (", " .  (" \n" . $anticomma ))), ",  \n" );
   is( ("," . (" " .  " \n$anticomma")), "  \n" );
   is( ("foo" . (";\n" . ("\n   \n" . $antispace))), "foo;" );
};

done_testing;
