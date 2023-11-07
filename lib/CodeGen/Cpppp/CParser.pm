package CodeGen::Cpppp::CParser;

# VERSION
# ABSTRACT: C Parser Utility Library

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'postderef';

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
our %named_escape= (
   a => "\a", b => "\b", e => "\e", f => "\f",
   n => "\n", r => "\r", t => "\t", v => "\x0B"
);
sub _get_tokens {
   pos= 0 unless defined pos;
   my @tokens;
   local our $_type;
   local our $_value;
   while (m{
         \G
         \s* \K # ignore whitespace
         (?|
            # single-line comment
            // ( [^\r\n]* )
            (?{ $_type= 'comment' })

            # block comment
         |  /\* ( (?: [^*]+ | \* (?=[^/]) )* ) \*/
            (?{ $_type= 'comment' })

            # Preprocessor directive
         |  \# \s* ( (?: [^\r\n\\]+ | \\ \r? \n | \\ (?=[^\r\n]) )* )
            (?{ $_type= 'directive' })

            # string literal
         |  " (?{ '' })
               (?|
                  ([^"\\]+)          (?{ $^R . $1 })
               |  \\x ([0-9A-Fa-f]+) (?{ $^R . chr(hex $1) })
               |  \\ ([0-9]{1,3})    (?{ $^R . chr(oct $1) })
               |  \\ \r?\n
               |  \\ (.)             (?{ $^R . ($named_escape{$1} // $1) })
               )*
            "
            (?{ $_type= 'string'; $_value= $^R; })

            # character constant
         |  '  (?|
                  ([^'\\])           (?{ $1 })
               |  \\x ([0-9A-Fa-f]+) (?{ chr(hex $1) })
               |  \\ ([0-9]{1,3})    (?{ chr(oct $1) })
               |  \\ (.)             (?{ $named_escape{$1} // $1 })
               )
            '
            (?{ $_type= 'char'; $_value= $^R; })

            # identifier
         |  ( [A-Za-z_] \w* )
            (?{ $_type= $keywords{$1}? 'keyword' : 'ident' })

            # real number
         |  ( (?: [0-9]+ \. [0-9]* | \. [0-9]+ ) (?: e -? [0-9]+ )? [lLfF]? )
            (?{ $_type= 'float' })

         |  # integer
            (?|
               0x([A-Fa-f0-9]+) (?{ $_value= hex($1) })
            |  0([0-7]+)        (?{ $_value= oct($1) })
            |  ([0-9]+)
            )
            [uU]?[lL]*
            (?{ $_type= 'integer' })

         |  # punctuation and operators
            ( \+\+ | -- | -> | \+=? | -=? | \*=? | /=? | %=? | >>=? | >=? | <<=? | <=?
            | \&\&=? | &=? | \|\|=? | \|=? | ^=? | ==? | !=? | \? | ~
            | [\[\](){};,.:]
            )
            (?{ $_type= $1 })

         |  # all other characters
            (.) (?{ $_type= 'unknown' })
         )
      }xcg
   ) {
      push @tokens, [ $_type, $_value // $1, $-[0], $+[0] - $-[0] ];
      $_value= undef;
   }
   return @tokens;
}

1;
