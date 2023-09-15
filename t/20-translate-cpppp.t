#! /usr/bin/env perl
use Test2::V0;
use CodeGen::Cpppp;
use Data::Printer;

my $cpppp= CodeGen::Cpppp->new;

my @tests= (
   {  name => "just perl",
      code => <<~'C' , file => __FILE__, line => __LINE__,
      ## say "it worked";
      ## my $x= 5;
      ## say "x= $x";
      C
      expect => <<~'pl'
      # line 11 "t/20-translate-cpppp.t"
      say "it worked";
      my $x= 5;
      say "x= $x";
      pl
   },
   {  name => "just C",
      code => <<~'C' , file => __FILE__, line => __LINE__,
      struct vec {
         float x, y, z;
      };
      C
      expect => <<~'pl' ,
      $self->_render_code_block(do { my @expr_subs;
      ($self->{code_block_templates}[0], \@expr_subs)
      });
      pl
   },
   {  name => 'C in a perl loop',
      code => <<~'C', file => __FILE__, line => __LINE__,
      ## for (3..4) {
      struct Vector$_ { float values[$_] };
      ## }
      C
      expect => <<~'pl',
      # line 36 "t/20-translate-cpppp.t"
      for (3..4) {
      $self->_render_code_block(do { my @expr_subs;
      $expr_subs[0]= sub { my $self= shift;
      # line 37 "t/20-translate-cpppp.t"
      $_
      };
      $expr_subs[1]= sub { my $self= shift;
      # line 37 "t/20-translate-cpppp.t"
      $_
      };
      ($self->{code_block_templates}[1], \@expr_subs)
      });
      # line 38 "t/20-translate-cpppp.t"
      }
      pl
   },
);

for my $t (@tests) {
   my $pl= $cpppp->_translate_cpppp(\$t->{code}, $t->{file}, $t->{line}+1);
   # remove leading whitespace, so that changes in formatting of the code don't break tests
   $pl =~ s/^\s+//mg;
   is( $pl, $t->{expect}, $t->{name} )
      or diag &np([$pl]);
}

done_testing;
