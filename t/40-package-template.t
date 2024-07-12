#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use CodeGen::Cpppp;
use CodeGen::Cpppp::Template qw( create_template );

ok( eval { require "$FindBin::RealBin/lib/TemplateAsPackage.pm" }, 'require TemplateAsPackage' )
   or do { diag $@; exit 2 };

my $t;
ok( eval { $t= TemplateAsPackage->new }, 'new TemplateAsPackage' )
   or do { diag $@; exit 2 };

is( $t->output, <<'END', 'initial output' );

Initial Line of Output

END

like( my $class= create_template(<<'END'), qr/^CodeGen::Cpppp::/, 'create_template' );
## my $x= "test";

$x

END

$t= $class->new;
is( $t->output, <<'END', 'initial output' );

test

END

done_testing;
