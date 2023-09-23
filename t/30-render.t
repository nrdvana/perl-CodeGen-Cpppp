#! /usr/bin/env perl
use Test2::V0;
use CodeGen::Cpppp;
use Data::Printer;

my $cpppp= CodeGen::Cpppp->new;

my @tests= (
   {  name => "loop, tpl, indent",
      code => <<~'C' , file => __FILE__, line => __LINE__,
      #! /usr/bin/env cpppp
      ## param $min_bits = 8;
      ## param $max_bits = 16;
      ## param $feature_parent = 0;
      ## param $feature_count = 0;
      ##
      ## for (my $bits= $min_bits; $bits <= $max_bits; $bits <<= 1) {
      struct tree_node_$bits {
          uint${bits}_t  left :  ${{$bits-1}},
                         color:  1,
                         right:  ${{$bits-1}},
                         parent,   ## if $feature_parent;
                         count,    ## if $feature_count;
                         $trim_comma $trim_ws;
      };
      ## }
      C
      expect => <<~'C',
      struct tree_node_8 {
          uint8_t  left :  7,
                   color:  1,
                   right:  7;
      };
      struct tree_node_16 {
          uint16_t left : 15,
                   color:  1,
                   right: 15;
      };
      C
   },
   {  name => 'indent double expansion',
      code => <<~'C', file => __FILE__, line => __LINE__,
      ## my $NAMESPACE= 'x';
      ## for my $bits (8, 16) {
      #define ${NAMESPACE}_MAX_TREE_HEIGHT_$bits  ${{2*($bits-1)+1}}
      #define ${NAMESPACE}_MAX_ELEMENTS_$bits     0x${{sprintf "%X", 2**($bits-1)-1}}
      ## }
      C
      expect => <<~'C',
      #define x_MAX_TREE_HEIGHT_8      15
      #define x_MAX_ELEMENTS_8       0x7F
      #define x_MAX_TREE_HEIGHT_16     31
      #define x_MAX_ELEMENTS_16    0x7FFF
      C
   },
   {  name => 'anticomma',
      code => <<~'C' , file => __FILE__, line => __LINE__,
      ## my @x= qw( a b c d e f );
      ## local $"= ',';
      ## my $y= '';
      void fn(@x,$y $trim_comma)
      C
      expect => "void fn(a, b, c, d, e, f )\n",
   },
   {  name => "user function",
      code => <<~'C' , file => __FILE__, line => __LINE__,
      ## my $y= 2;
      ## sub example($x) {
      ##   $x*$x/$y
      ## }
      got ${{example(8)}}
      C
      expect => "got 32\n",
   },
   {  name => "list autocomma",
      code => <<~'C', file => __FILE__, line => __LINE__,
      ## my @x= qw( a b c );
      foo(@x);
      ## @x= ();
      foo(1,2,@x);
      C
      expect => "foo(a, b, c);\nfoo(1,2);\n"
   },
   {  name => "list autostatement autoindent",
      code => <<~'C', file => __FILE__, line => __LINE__,
      ## my @stmt= qw( x++ y-- );
      if (x) {
         @stmt;
      }
      C
      expect => <<~'C'
      if (x) {
         x++;
         y--;
      }
      C
   },
   {  name => 'auto indent after expansion',
      code => <<~'C', file => __FILE__, line => __LINE__,
      ## my @fields= qw( x y z );
      ## my $float_t= 'double';
      struct Foo {
         $float_t  scale,
                   @fields;
      };
      C
      expect => <<~'C'
      struct Foo {
         double scale,
                x,
                y,
                z;
      };
      C
   },
);

for my $t (@tests) {
   my $pkg= $cpppp->compile_cpppp(\$t->{code}, $t->{file}, $t->{line}+1);
   my $out= $pkg->new->output;
   my $c= $out->get;
   is( $c, $t->{expect}, $t->{name} )
      or diag &np([$out]);
}

done_testing;
