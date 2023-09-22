package CodeGen::Cpppp::Output;
$INC{'CodeGen/Cpppp/Output.pm'}= 1;
use v5.20;
use warnings;
use Carp;
use Scalar::Util 'looks_like_number';
use List::Util 'max';
use experimental 'signatures';

=head1 DESCRIPTION

C code usually needs generated in different parts, like a public header,
private-to-the-project header, the forward declarations of static function,
and finally the function definitions themselves.
This object encapsulates that concept by storing different "sections" of
generated code that you can later direct to the files where they need to go.

The default sections are:

=over

=item public

Lines of code for the public header.  Maybe also inline functions.
C<priority=10>

=item protected

Lines of code for consumption by other related modules, exposing more data
structures and macros than are appropriate for the public header.
C<priority=20>

=item private

Lines of code that define things that no other compilation unit should need.
These typically would get written into the top of the compilation unit itself.
C<priority=50>

=item impl

The implementation of the compilation unit.  This contains everything else.
C<priority=100>

=back

You can append or prefix blocks of code to any of these sections, or define
additional sections of your own.  The sections you define should be assigned
a C<priority> to help sort them into the list above.  You may use floating
point numbers.

=head1 METHODS

=head2 new

Standard constructor, accpeting key/val list or hashref.

=cut

our %standard_sections= (
   public => 100,
   protected => 200,
   private => 500,
   impl => 10000,
);
sub new($class, @args) {
   bless {
      section_priority => { %standard_sections },
      out => {},
      @args == 1 && ref $args[0]? %{$args[0]}
      : !(@args & 1)? @args
      : croak "Expected hashref or even-length list"
   }, $class;
}

=head2 declare_sections

  $out->declare_sections($name1, $name2, ...);
  $out->declare_sections($name1 => $priority, $name2 => ..);

Declare one or more new sections.  If you omit the priority values, they will
be automatically selected counting upward from the last section below C<'impl'>.

=head2 section_priority

A hashref of C<< { $section_name => $priority } >>.

=head2 section_list

Returns a list of output section names, in the order that they would need
compiled.

=cut

sub section_list($self) {
   my $pri= $self->section_priority;
   sort { $pri->{$a} <=> $pri->{$b} } keys %$pri;
}

sub section_priority($self) {
   $self->{section_priority}
}

sub declare_sections($self, @list) {
   my $pri= $self->section_priority;
   my $max_before_impl= max grep $_ < $pri->{impl}, values %$pri;
   my $next= $max_before_impl + 1;
   while (@list) {
      my $name= shift @list;
      looks_like_number($name) and croak "Expected non-numeric name at '$name'";
      if (looks_like_number($list[0])) {
         $pri->{$name}= shift @list;
      } elsif (!defined $pri->{$name}) {
         $pri->{$name}= $next++;
      }
   }
   $self;
}

=head2 append

  $out->append($section, @code_block);

Add one or more blocks of code to the end of the named section.
The section must be declared.

=head2 prepend

  $out->prepend($section, @code_block);

Add one or more blocks of code to the beginning of the named section.
The section must be declared.

=cut

sub append($self, $section, @code) {
   defined $self->{section_priority}{$section} or croak "Unknown section $section";
   push @{$self->{out}{$section}}, @code;
}
sub prepend($self, $section, @code) {
   defined $self->{section_priority}{$section} or croak "Unknown section $section";
   unshift @{$self->{out}{$section}}, @code;
}

=head2 get

  $all= $out->get;
  $header= $out->get('public','protected');
  $unit= $out->get('private..impl');

Collect the output from all or specified sections.  An empty list returns all
sections.  The special notation '..' returns a range of sections, inclusive.

=cut

sub get($self, @sections) {
   my @sec;
   my $sec_pri= $self->section_priority;
   if (@sections) {
      my %s;
      for (@sections) {
         if (/([^.]+)..([^.]+)/) {
            my $low= $sec_pri->{$1} // croak "Unknown section $1";
            my $high= $sec_pri->{$2} // croak "Unknown section $2";
            for (keys %$sec_pri) {
               $s{$_}++ if $sec_pri->{$_} >= $low && $sec_pri->{$_} <= $high;
            }
         } else {
            $sec_pri->{$_} // croak "Unknown section $_";
            $s{$_}++;
         }
      }
      @sec= sort { $sec_pri->{$a} <=> $sec_pri->{$b} } keys %s;
   } else {
      @sec= sort { $sec_pri->{$a} <=> $sec_pri->{$b} } keys %{ $self->{out} };
   }
   join '', map @{$self->{out}{$_} // []}, @sec;
}

1;