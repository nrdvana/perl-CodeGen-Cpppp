#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use v5.20;
use warnings;
use experimental qw/ signatures lexical_subs postderef /;
use CodeGen::Cpppp;
use CodeGen::Cpppp::Template 'create_template';

my $cpppp= CodeGen::Cpppp->new;

my $template_from_string= create_template(<<END);
if (something) {
   something_else();
}
END

say $template_from_string;
say $template_from_string->isa('CodeGen::Cpppp::Template');

{
   package TemplateFromPackage;
   use CodeGen::Cpppp::Template -setup => 0;
   
   template_builder->parse(<<END, __FILE__, __LINE__+1)
if (cond) {
## template($template_from_string);
}
END
}

$cpppp->new_template('TemplateFromPackage');

is( $cpppp->output->get, <<END );
if (cond) {
   if (something) {
      something_else();
   }
}
END

done_testing;
