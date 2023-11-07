#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use v5.20;

use CodeGen::Cpppp::CParser;

for (
   [ 'for(int i=0; i<1; i++){}',
      [ [ keyword => 'for', 0, 3 ],
        [ '('     => '(',   3, 1 ],
        [ keyword => 'int', 4, 3 ],
        [ ident   => 'i',   8, 1 ],
        [ '='     => '=',   9, 1 ],
        [ integer => 0,    10, 1 ],
        [ ';'     => ';',  11, 1 ],
        [ ident   => 'i',  13, 1 ],
        [ '<'     => '<',  14, 1 ],
        [ integer => 1,    15, 1 ],
        [ ';'     => ';',  16, 1 ],
        [ ident   => 'i',  18, 1 ],
        [ '++'    => '++', 19, 2 ],
        [ ')'     => ')',  21, 1 ],
        [ '{'     => '{',  22, 1 ],
        [ '}'     => '}',  23, 1 ],
      ]
   ],
) {
   my ($code, $expected)= @$_;
   my @tokens;
   @tokens= CodeGen::Cpppp::CParser::_get_tokens() for $code;
   is( \@tokens, $expected );
}

done_testing;
