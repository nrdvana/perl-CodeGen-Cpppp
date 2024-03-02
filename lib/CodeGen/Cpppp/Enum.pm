package CodeGen::Cpppp::Enum;

# VERSION
# ABSTRACT: Base class for template classes created by compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'lexical_subs', 'postderef';
use Scalar::Util 'looks_like_number';
use List::Util 'any';
use CodeGen::Cpppp::CParser;

=head1 SYNOPSIS



=head1 DESCRIPTION

This utility module helps you generate C code for enumerations.

First, you create an instance of this object and load it with a list of
names (and optionally values).  These can come from a supplied list, or you
can parse a header to find them.  This module supports multiple names with the
same value, but not multiple values with the same name.

Next, you can generate the definition of the enum as either #define macros or
a C++ C<< enum { ... } >> syntax.

This also can generate code that looks up the name by the value, or parses a
name to get the value.  The output would generally look like:

  #define FOO_1 1
  #define FOO_2 2
  
  // Returns true if found a match
  extern bool foo_parse(const char *ch, const char *endp, int *enum_val_out);
  extern const char *foo_name(int enum_val);

There are several implementations to choose from, and a sensible one will be
chosen by default based on the patterns in your enum values.

=head1 CONSTRUCTOR

=head2 new

Standard constructor.  Pass values for any of the non-readonly attributes below.

=cut

sub new($class, %attrs) {
   my $self= bless {}, $class;
   $self->$_($attrs{$_}) for keys %attrs;
   return $self;
}

=head1 ATTRIBUTES

=head2 prefix

String to be prefixed onto each name of the enum.  You may then optionally
allow name lookups that match strings without the prefix as well as ones that
include it.

=head2 value_type

Defaults to 'int'.

=head2 values

Returns a list of C<< [ $name, $value ] >>.  You can initialize it with a list
or arrayref containing C<$name> or C<< [ $name, $value ] >>.  Any element
without a value will get the next sequential value from the previous entry,
starting from 0.

=head2 num_format

Controls formatting of integers, as decimal or hex.  One of 'd', 'x', or 'X'.

=head2 max_waste_factor

Permissible amount of wasted space in a lookup table or hash table, above which
a different algorithm will be chosen.

=cut

sub value_type($self, @val) {
   if (@val) {
      $self->{value_type}= $val[0];
      return $self;
   }
   $self->{value_type} // 'int';
}

sub values($self, @val) {
   return $self->set_values(@val) if @val;
   @{ $self->{values} // [] }
}

sub set_values($self, @spec) {
   my @values;
   for (@spec == 1 && ref $spec[0]? @{$spec[0]} : @spec) {
      if ('ARRAY' eq ref) {
         push @values, [ @$_ ];
      } elsif (/^\w+$/) {
         push @values, [ $_ ];
      } else {
         defined $values[-1] or croak "Got an enum value '$_' before a name";
         defined $values[-1][1] and croak "'$_' is not a valid enum name";
         $values[-1][1]= $_;
      }
   }
   # Fill in missing values with next sequential integer
   my $prev= $values[0][1] //= 0;
   for (@values[1..$#values]) {
      if (!defined $_->[1]) {
         my ($base, $ofs, $fmt)= $self->_parse_value_expr($prev);
         $_->[1]= sprintf $fmt, $ofs+1;
      }
      $prev= $_->[1];
   }
   $self->{values}= \@values;
   $self;
}

sub max_waste_factor($self, @val) {
   if (@val) {
      $self->{max_waste_factor}= $val[0];
      return $self;
   }
   $self->{max_waste_factor} // 2;
}

=head2 algorithm

One of 'bsearch', 'hashtable', 'switch'.

For each, the enum is stored in an array of string/value pairs (unless the
values match the array index, then they get omitted).  If this happens to be
the sorted order of the names or sorted order of the values, those
optimizations are taken into account.  Otherwise, the algorithm operates as
follows:

=over

=item bsearch

Secondary arrays of integers are built that refer back to the enum array.
These arrays are searched during name or value lookups using the C library
C<bsearch> function.  This results in the least space usage.

=item hashtable

Hash tables will be built that refer back to the enum array.  Hash function
constants will be chosen to provide the fewest number of collisions possible.
This results in good performance without much added code.

=item switch

The lookup functions will be built using a combination of C 'switch' statements
and binary character-comparison 'if' statements.  This results in the absolute
fastest performance, but may generate a fair amount of parsing code.

=back

=cut

our %_algorithm= map +( $_ => 1 ), qw( bsearch hashtable switch );
sub algorithm($self, @val) {
   if (@val) {
      !defined $val[0] or $_algorithm{$val[0]}
         or croak "Unknown parse_design '$val[0]', expected one of ".join(', ', keys %_algorithm);
      $self->{algorithm}= $val[0];
      return $self;
   }
   $self->{algorithm}
}

sub _parse_value_expr($self, $val) {
   # Make the common case fast
   return '', +$val, '%d'
      if $val =~ /^[-+]?(?:0|[1-9][0-9]*)\Z/;
   # else need to parse the expression
   my @tokens= CodeGen::Cpppp::CParser->tokenize($val);
   my $type_pattern= join '', map $_->type, @tokens;
   # Recognize patterns where a +N occurs at the end of the expression
   # Else, the whole value is the expression and will get '+N' appended.
   return $val, 0, "($val+".($self->{num_format}//'%d').")"
      unless $type_pattern =~ /(^|[-+])integer\W*$/;
   my $context= $1;
   # walk backward to last 'integer' token
   my $i= $#tokens;
   $i-- while $tokens[$i]->type ne 'integer';
   # could be start of string, -N, +N, EXPR-N, EXPR+N, or EXPR OP -N
   my $fmt_str= $val;
   my ($pos, $pos2)= ($tokens[$i]->src_pos, $tokens[$i]->src_pos+$tokens[$i]->src_len);
   my $n= $tokens[$i]->value;
   # If start of string or preceeded by '+', nothing to do.
   # If preceeded by '-', need to convert that to '+' in format string
   if ($context eq '-') {
      $n= -$n;
      $pos= $tokens[$i-1]->src_pos + 1;
      substr($fmt_str, $tokens[$i-1]->src_pos, 1, '+');
   }
   my $num_str= substr($val, $tokens[$i]->src_pos, $tokens[$i]->src_len);
   my $notation= $self->{num_format}
      // $num_str =~ /^-?0x[0-9A-F]+$/? 'X'
       : $num_str =~ /^-?0x[0-9a-f]+$/? 'x'
       : $num_str =~ /^-?0[0-9]+/? 'o'
       : 'd';
   substr($fmt_str, $pos, $pos2-$pos, '%'.($pos2-$pos).$notation);
   # The "base" is everying to the left of the number minus the number of "("
   #  to match the number of ")" to the right of the number
   my $rparen= grep $_->type eq ')', @tokens[$i..$#tokens];
   shift @tokens while $tokens[0]->type eq '(' && $rparen--;
   my $base= substr($val, $tokens[0]->src_pos, $pos-$tokens[0]->src_pos);
   return ($base, $n, $fmt_str);
}

=head2 is_symbolic

True if one if your values is an expression instead of a constant integer.
In this case, only the switch algorithm can be used for lookups.

=head2 is_sequential

Read-only, calculated by whether all the values have adjacent numeric values.

=head2 is_sequentialish

Read-only, calculated by whether all the values can be stored in an array at
the offset of their numeric value without wasting more than C<max_waste_factor>
elements.

=cut

sub is_symbolic($self) {
}

sub is_sequential($self) {
}

sub is_sequentialish($self) {
}

sub _analyze_values($self) {
   my @vals= map +[ $_->[0], $self->_parse_value_expr($_->[1]) ], $self->values;
   my $base_expr= $vals[0][1];
   my %seen_ofs= ( $vals[0][2] => 1 );
   for (@vals[1..$#vals]) {
      # Can't be sequential unless they share a symbolic base expression
      $base_expr= undef, last
         unless $_->[1] eq $base_expr;
      $seen_ofs{$_->[2]}++;
   }
   my %info= (
      vals => \@vals
   );
   if (defined $base_expr) {
      # Find the min/max
      my ($min, $max)= (min(keys %seen_ofs), max(keys %seen_ofs));
      # Is it sequential?
      my ($is_seq, $is_nearly_seq, $gap);
      # don't iterate unless the range is reasonable
      if (($max - $min) <= (1+$self->max_waste_factor) * @vals) {
         $gap= 0;
         for ($min .. $max) {
            $gap++ unless $seen_ofs{$_};
         }
         $is_seq= $gap == 0;
         $is_nearly_seq= $gap <= ($self->max_waste_factor * ($max-$min+1));
      }
      $info{is_seq}= $is_seq;
      $info{is_nearly_seq}= $is_nearly_seq;
      $info{gap}= $gap;
      $info{min}= $min;
      $info{max}= $max;
      $info{base_expr}= $base_expr;
   }
   return \%info;
}

1;
