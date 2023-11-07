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
            // [ \t]* ( [^\r\n]* )   (?{ $_type= 'linecomment' })
            
            # block comment
         |  /\* ( (?: [^*]+ | \* (?=[^/]) )* ) \*/ (?{ $_type= 'blockcomment' })
            
            # Preprocessor directive
         |  \# \s* ( (?: [^\r\n\\]+ | \\ \r? \n | \\ (?=[^\r\n]) )* )
            (?{ $_type= 'directive' })
            
            # string literal
         |  " ( (?: [^"\\]+ | \\ . )* ) "
            (?{ $_type= 'string'; $_value= _parse_c_str($1) })
         
            # character constant
         |  ' ( [^\\] | \\ [^xo] | \\x[0-9A-Fa-f]+ | \\o[0-7]+ ) '
            (?{ $_type= 'char'; $_value= _parse_c_str($1) })
         
            # identifier
         |  ( [A-Za-z_] \w* ) (?{ $_type= $keywords{$1}? 'keyword' : 'ident' })
 
            # real number
         |  ( (?: [0-9]+ [.] [0-9]* | \. [0-9]+ ) (?: e -? [0-9]+ )? [lLfF]? )
            (?{ $_type= 'float' })
         
         |  # integer
            ( (?: 0x([A-Fa-f0-9]+) (?{ $_value= hex($2) })
                | 0([0-7]+)        (?{ $_value= oct($2) })
                | [0-9]+
              )
              [uU]?[lL]*
            )
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
