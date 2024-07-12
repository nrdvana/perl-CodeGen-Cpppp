package CodeGen::Cpppp::CSyntax;

# VERSION
# ABSTRACT: Utility library for things related to C language syntax

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'postderef';
use Exporter::Extensible -exporter_setup => 1;
export qw( is_keyword c_str c_str_escape %keywords %named_string_escape );

=head1 EXPORTS

=head2 is_keyword

  $bool= is_keyword($name)

=cut

our %keywords= map +($_ => 1), qw( 
   auto break case char const continue default do double else enum extern
   float for goto if int long register return short signed sizeof static
   struct switch typedef union unsigned void volatile while

   inline _Bool _Complex _Imaginary

   __FUNCTION__ __PRETTY_FUNCTION__ __alignof __alignof__ __asm
   __asm__ __attribute __attribute__ __builtin_offsetof __builtin_va_arg
   __complex __complex__ __const __extension__ __func__ __imag __imag__ 
   __inline __inline__ __label__ __null __real __real__ 
   __restrict __restrict__ __signed __signed__ __thread __typeof
   __volatile __volatile__ 

   restrict
);

sub is_keyword($name) { defined $keywords{$name} }

=head2 c_str

  $c_string_literal= c_str($perl_string);

Returns a double-quoted string constant.

=head2 c_str_escape

Same as above, but without the quote characters around the result.

=cut

our %named_string_escape= (
   a => "\a", b => "\b", e => "\e", f => "\f",
   n => "\n", r => "\r", t => "\t", v => "\x0B"
);

our %string_char_escapes= (
   '"' => '\\"', '\\' => '\\\\',
   "\a" => '\\a', "\b" => '\\b', "\e" => '\\e', "\f" => '\\f',
   "\n" => '\\n', "\r" => '\\r', "\t" => '\\t', "\x0B" => '\\v',
);

sub c_str_escape($str) {
   $str =~ s|([\0-\x1F\\"\x7F-\xFF])|$string_char_escapes{$1} // sprintf("\\x%02X", ord $1)|gear
}

sub c_str($str) {
   '"'.c_str_escape($str).'"'
}

1;
