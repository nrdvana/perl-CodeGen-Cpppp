#! /usr/bin/env perl
use Test2::V0;
use CodeGen::Cpppp;
use Data::Printer;

my $cpppp= CodeGen::Cpppp->new;

my @tests= (
   {  name => "just perl",
      code => <<~'C' , file => __FILE__, line => __LINE__,
      ## for (my $bits= 8; $bits <= 32; $bits <<= 1) {
      struct tree_node_$bits {
          uint${bits}_t  left: ${{$bits-1}},
                         color: 1,
                         right: ${{$bits-1}};
      };
      ## }
      C
      expect => <<~'C',
      struct tree_node_8 {
          uint8_t left: 7,
                  color: 1,
                  right: 7;
      };
      struct tree_node_16 {
          uint16_t left: 15,
                   color: 1,
                   right: 15;
      };
      struct tree_node_32 {
          uint32_t left: 31,
                   color: 1,
                   right: 31;
      };
      C
   },
);

for my $t (@tests) {
   my $c= $cpppp->compile_template(\$t->{code}, $t->{file}, $t->{line}+1)->render;
   is( $c, $t->{expect}, $t->{name} )
      or diag &np([$c]);
}

done_testing;
