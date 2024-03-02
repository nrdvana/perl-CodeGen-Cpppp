#! /usr/bin/env perl
use FindBin;
use lib "$FindBin::RealBin/lib";
use Test2WithExplain;
use v5.20;

use CodeGen::Cpppp::Enum;

subtest values_init => sub {
   my $e= CodeGen::Cpppp::Enum->new(values => [qw(
      One
      Two
      Three
   )]);
   is( [$e->values],
      [
         [ One => 0 ],
         [ Two => 1 ],
         [ Three => 2 ],
      ],
      'Start at 0'
   );
   $e->values([ One => 1 ], 'Two', 'Three');
   is( [$e->values],
      [
         [ One => 1 ],
         [ Two => 2 ],
         [ Three => 3 ],
      ],
      'Start from 1'
   );
   $e->values('Zero','One',[ Four => 4 ],'Five');
   is( [$e->values],
      [
         [ Zero => 0 ],
         [ One => 1 ],
         [ Four => 4 ],
         [ Five => 5 ],
      ],
      'Reset middle of list'
   );
};

subtest increment_expressions => sub {
   my $e= CodeGen::Cpppp::Enum->new;
   $e->values([
      A => '((65 + 0))',
      qw( B C )
   ]);
   is( [$e->values],
      [
         [ A => '((65 + 0))' ],
         [ B => '((65 + 1))' ],
         [ C => '((65 + 2))' ],
      ],
      'Simple addition in parens'
   );
   $e->values([
      A => 'X -  2',
      qw( B C D )
   ]);
   is( [$e->values],
      [
         [ A => 'X -  2' ],
         [ B => 'X + -1' ],
         [ C => 'X +  0' ],
         [ D => 'X +  1' ],
      ],
      'Increment from subtraction'
   );
};

done_testing;
