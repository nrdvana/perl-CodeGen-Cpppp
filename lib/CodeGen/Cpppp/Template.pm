package CodeGen::Cpppp::Template;

our $VERSION= 0; # VERSION
# ABSTRACT: Base class for template classes created by compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'lexical_subs', 'postderef';
use Scalar::Util 'looks_like_number';
use Hash::Util;
use CodeGen::Cpppp::TemplateBuilder;
use CodeGen::Cpppp::Output;
use Exporter ();
require version;

=head1 DESCRIPTION

This is the base class for all Template classes compiled from cpppp source.
It also defines the exports that set up the scope for evaluating the template.

=head1 EXPORTS

=over

=item C<-parent>

Initializes @ISA as a subclass of the package being imported.

=item C<-setup>

Initializes @ISA as a subclass of the package being imported, and sets the
compiler flags for strict, warnings, utf8, lexical_subs, signatures,
and postderef.

=item C<:v0>

Exports symbols C<PUBLIC>, C<PROTECTED>, C<PRIVATE>, C<compile_cpppp>

=back

=cut

package CodeGen::Cpppp::Template::Exports {
   use constant {
      PUBLIC     => 'public',
      PROTECTED  => 'protected',
      PRIVATE    => 'private',
   };
   use Carp qw( carp croak );
   use CodeGen::Cpppp::CSyntax qw( c_str c_str_escape );
   sub enum { require CodeGen::Cpppp::Enum; goto \&CodeGen::Cpppp::Enum::new; }

   our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE compile_cpppp create_template
     format_commandline format_timestamp enum c_str c_str_escape
   );
   our %EXPORT_TAGS= (
      'v0' => [qw( PUBLIC PROTECTED PRIVATE compile_cpppp )],
   );
   sub compile_cpppp {
      my ($pkg, $filename, $line)= caller;
      unshift @_, $pkg->template_builder;
      goto $_[0]->can('compile');
   }
   sub create_template {
      my %attrs= @_ == 3? ( source => $_[0], file => $_[1], line => $_[2] )
         : @_ != 1? @_
         : !ref $_[0]? ( source => $_[0] )
         : ref $_[0] eq 'HASH'? %{$_[0]}
         : croak "Expected attribute list, attribute hash, or string of cpppp source";
      CodeGen::Cpppp::TemplateBuilder->new(%attrs)->compile;
   }
   sub format_commandline {
      require CodeGen::Cpppp::Platform;
      CodeGen::Cpppp::Platform::format_commandline(@_);
   }
   sub format_timestamp {
      my @t= gmtime;
      sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ", $t[5]+1900, $t[4]+1, @t[3,2,1,0]
   }
}

sub import {
   my $class= $_[0];
   my $caller= caller;
   for (my $i= 1; $i < @_; $i++) {
      if ($_[$i] eq '-setup') {
         my $ver= version->parse($_[$i+1]);
         croak "This is only CodeGen::Cpppp version ".CodeGen::Cpppp->VERSION
            if $ver > CodeGen::Cpppp->VERSION;
         CodeGen::Cpppp::TemplateBuilder->new(template_class => $caller)->_setup_class;
         splice(@_, $i, 2, ':v'.$ver);
      }
   }
   splice(@_, 0, 1, 'CodeGen::Cpppp::Template::Exports');
   goto \&Exporter::import;
}

=head1 ATTRIBUTES

=head2 context

Weak-reference to the instance of C<CodeGen::Cpppp> which created this template,
if any.  This is automatically set by L<CodeGen::Cpppp/new_template>. Read-only.

=head2 output

Instance of L<CodeGen::Cpppp::Output>.  Read-only.

=head2 current_output_section

Name of the section of output being written.  Read-write.

=cut

sub context { $_[0]{context} }

sub output { $_[0]->flush->{output} }

sub current_output_section($self, $new=undef) {
   if (defined $new) {
      $self->output->has_section($new)
         or croak "No defined output section '$new'";
      $self->_finish_render;
      $self->{current_output_section}= $new;
   }
   $self->{current_output_section};
}

=head2 autocolumn

Whether to look for column-alignment in the template source and try to preserve
that column alignment after all variables have been substituted.

=head2 autocomma

Whether to automatically insert commas when interpolating an array into a
template, based on context.

=head2 autoindent

Whether to guess what the proper indent should be when substituting content
that contains a newline.

=head2 autostatementline

Whether to automatically insert newlines (and maybe indent) when substituting
an array into a template, based on context.

=head2 indent

The per-block indent to use for generated code.  This can be set to either a
number (of spaces) or a literal string to be appended for each level of indent.
If undefined, the indent will be detected from the change in leading whitespace
from the first observed '{' in your template.

This setting does not re-format the existing indentation written in the
template; you need a full code-formatting tool for that.

=head2 emit_POD

By default, Plain Old Documentation (POD) notation found in the template is
assumed to document the template itself, and will be removed from the generated
output.  Set this to true to emit the POD as part of the output.  (but a better
idea is to declare variables like C<< $head1= '=head1' >> and then use those
to generate the output POD)

=cut

sub autocolumn        { $_[0]{autocolumn}       = $_[1]||0 if @_ > 1; $_[0]{autocolumn}        }
sub autocomma         { $_[0]{autocomma}        = $_[1]||0 if @_ > 1; $_[0]{autocomma}         }
sub autoindent        { $_[0]{autoindent}       = $_[1]||0 if @_ > 1; $_[0]{autoindent}        }
sub autostatementline { $_[0]{autostatementline}= $_[1]||0 if @_ > 1; $_[0]{autostatementline} }
sub indent            { $_[0]{indent}           = $_[1]    if @_ > 1; $_[0]{indent} }
sub emit_POD          { $_[0]{emit_POD}         = $_[1]||0 if @_ > 1; $_[0]{emit_POD} }

=head2 template_builder

Returns the TemplateBuilder object which parsed the cpppp source of this template.

=head2 template_parameters

Returns the specification of which parameters are exposed to users of the template.

=cut

# These methods are generated per subclass by the template builder

=head1 CONSTRUCTOR

  $tpl= $template_class->new(%params, %attrs);

The constructor takes object attributes I<and> user-defined template parameters.
When specifying values for parameters, the type of the value must match the
variable-type of the parameter, such as '@array' variables needing arrayref
values.

Running the constructor immediately executes the body of the user's template,
which may initialize variables and define subroutines, and likely also generate
output.  The subs declared in the template are then exposed as methods of this
object.  Calling those methods may generate additional output, which is all
collected in the L</output> object.

=cut

sub new($class, @args) {
   no strict 'refs';
   my %attrs= @args == 1 && ref $args[0]? $args[0]->%*
      : !(@args&1)? @args
      : croak "Expected even-length list or hashref";
   my $params= $class->template_parameters;
   # Make sure each attr is the correct type of ref, for the params.
   for (keys %attrs) {
      if (my $p= $params->{$_}) {
         my $sigil= substr($p->{var}, 0, 1);
         if ($sigil eq '@') { ref $attrs{$_} eq 'ARRAY' or croak("Expected ARRAY for parameter $_"); }
         elsif ($sigil eq '%') { ref $attrs{$_} eq 'HASH' or croak("Expected HASH for parameter $_"); }
         $attrs{$p->{var}}= delete $attrs{$_};
      }
      else {
         croak("Unknown parameter '$_' to template ".$class->template_builder->filename)
            unless $class->can($_);
      }
   }

   my $self= bless {
      autocomma => 1,
      autostatementline => 1,
      (map +($_ => $class->template_builder->{$_}||0), qw(
         autoindent autocolumn convert_linecomment_to_c89
      )),
      indent => $class->template_builder->{indent},
      output => CodeGen::Cpppp::Output->new,
      current_output_section => 'private',
      %attrs,
   }, $class;
   Scalar::Util::weaken($self->{context})
      if $self->{context};
   $self->_template_init;
   $self->flush;
}

# parent class method only gets called when subclass hasn't been compiled
sub _template_init {
   my $self= $_[0];
   $self->template_builder->compile;
   my $method= $self->can('_template_init');
   croak "compile failed to create _template_init method"
      if $method == __PACKAGE__->can('_template_init');
   goto $method;
}

=head1 METHODS

=head2 coerce_parameters

  my $params= $tpl_class->coerce_parameters(\%params);

Given a hashref of potential parameter values, select and coerce the ones to
match the actual parameters of this template.  The keys of this hashref may be
either the bare name of the parameter, or the name-with-sigil.  This allows you
to specify C<< { '$x' => $scalar, '@x' => \@arrayref } >> and for some parameter
named 'x' this method will select the one whose type matches rather than
attempting to coerce the value.

=cut

sub coerce_parameters($class, $params) {
   my %ret;
   my $p_spec= $class->template_parameters;
   for my $k (keys %$p_spec) {
      my $p= $p_spec->{$k};
      my $v= $params->{$p->{var}} // $params->{$k};
      next unless defined $v;
      my $sigil= substr($p->{var}, 0, 1);
      if ($sigil eq '@') {
         $v= ref $v eq 'HASH'? [ keys %$v ] : [ $v ]
            unless ref $v eq 'ARRAY';
      } elsif ($sigil eq '%') {
         # If it isn't a hash, treat it like a list that needs added to a set
         $v= { map +($_ => 1), ref $v eq 'ARRAY'? @$v : ($v) }
            unless ref $v eq 'HASH';
      }
      $ret{$k}= $v;
   }
   \%ret;
}

#sub _template_init_param($self, $name_with_sigil, $ref, $initial_value_sub) {
sub _template_init_param($self, $name_with_sigil, $ref, @initial_value) {
   if (exists $self->{$name_with_sigil}) {
      # Assign the value received from constructor to the variable in the template
        ref $ref eq 'SCALAR'? ($$ref= $self->{$name_with_sigil})
      : ref $ref eq 'ARRAY' ? (@$ref= @{$self->{$name_with_sigil} || []})
      : ref $ref eq 'HASH'  ? (%$ref= %{$self->{$name_with_sigil} || {}})
      : croak "Unhandled ref type ".ref($ref);
   } else {
      # Use the template-provided initial value
        ref $ref eq 'SCALAR'? ($$ref= $initial_value[0]) #_sub->())
      : ref $ref eq 'ARRAY' ? (@$ref= @initial_value)    #_sub->())
      : ref $ref eq 'HASH'  ? (%$ref= @initial_value)    #_sub->())
      : croak "Unhandled ref type ".ref($ref);
   }

   # Now store the variable of the template directly into this hash
   ref $ref eq 'SCALAR'? Hash::Util::hv_store(%$self, $name_with_sigil, $$ref)
   : ($self->{$name_with_sigil}= $ref);
   $ref;
}

sub _template_init_sub($self, $name, $code) {
   $self->{'&'.$name}= $code;
}

=head2 flush

Make sure all output is written to the L</output> object.  Some output is
buffered internally so that formatting (like autoindent) can be applied when
a code block is complete.

=cut

sub flush($self) {
   $self->_finish_render;
   $self;
}

=head2 define_template_macro

This is called during the template constructor to bind a user-defined macro
to the lexical sub that implements it for this template instance.

=cut

sub define_template_macro($self, $name, $code) {
   $self->{template_macro}{$name}= $code;
}

#=head2 define_template_method
#
#This is called during the template constructor to bind the lexical subs of the
#template to methods of the object being created.
#
#=cut
#
#sub define_template_method($self, $name, $code) {
#   $self->{template_method}{$name}= $code;
#}

sub _render_pod_block($self, $i) {
   if ($self->emit_POD) {
      $self->_finish_render;
      my $blocks= $self->template_builder->{pod_blocks};
      $self->{output}->append($self->{current_output_section} => $blocks->[$i]);
   }
}

sub _finish_render($self) {
   return unless defined $self->{current_out};
   # Second pass, adjust whitespace of all column markers so they line up.
   # Iterate from leftmost column rightward.
   for my $group_i (sort { $a <=> $b } keys %{$self->{current_out_colgroup_state}}) {
      delete $self->{current_out_colgroup_state}{$group_i}
         if $self->{current_out_colgroup_state}{$group_i} == 2;
      my $token= _colmarker($group_i);
      # Find the longest prefix (excluding trailing whitespace)
      # Also find the max number of digits following column.
      my ($maxcol, $maxdigit)= (0,0);
      my ($linestart, $col);
      while ($self->{current_out} =~ /[ ]* $token (-? 0x[A-Fa-f0-9]+ | -? \d+)? /gx) {
         $linestart= rindex($self->{current_out}, "\n", $-[0])+1;
         $col= $-[0] - $linestart;
         $maxcol= $col if $col > $maxcol;
         $maxdigit= length $1 if defined $1 && length $1 > $maxdigit;
      }
      $self->{current_out} =~ s/[ ]* $token (?= (-? 0x[A-Fa-f0-9]+ | -? \d+)? )/
         $linestart= rindex($self->{current_out}, "\n", $-[0])+1;
         " "x(1 + $maxcol - ($-[0] - $linestart) + ($1? $maxdigit - length($1) : 0))
         /gex;
   }
   $self->{output}->append($self->{current_output_section} => $self->{current_out});
   $self->{current_out}= '';
}

sub _colmarker($colgroup_id) { join '', "\x{200A}", map chr(0x2000+$_), split //, $colgroup_id; }
sub _str_esc { join '', map +(ord($_) > 0x7e || ord($_) < 0x21? sprintf("\\x{%X}",ord) : $_), split //, $_[0] }

sub _render_code_block {
   my ($self, $i, @expr_subs)= @_;
   local $CodeGen::Cpppp::template_stack[$#CodeGen::Cpppp::template_stack+1]= $self;
   my $block= $self->template_builder->{code_block_templates}[$i];
   my $text= $block->{text};
   # Continue appending to the same output buffer so that autocolumn can
   # inspect the result as a whole.
   my $out= \($self->{current_out} //= '');
   my $at= 0;
   my %colmarker;
   # @subst contains a list of positions in the template body where text
   # may need inserted.
   for my $s (@{$block->{subst}}) {
      $$out .= substr($text, $at, $s->{pos} - $at);
      $at= $s->{pos} + $s->{len};
      # Column marker - may substitute for whitespace during _finish_render
      if ($s->{colgroup}) {
         my $mark= $colmarker{$s->{colgroup}} //= _colmarker($s->{colgroup});
         $$out .= $mark;
         $self->{current_out_colgroup_state}{$s->{colgroup}}= $s->{last}? 2 : 1;
      }
      # Variable interpolation - insert value of one of the @expr_subs here
      elsif (defined $s->{eval_idx}) {
         my $fn= $expr_subs[$s->{eval_idx}]
            or die;
         # Identify the indent settings at this point so that other modules can
         # automatically generate matching code.
         my ($last_char)= ($$out =~ /(\S) (\s*) \Z/x);
         my $cur_line= substr($$out, rindex($$out, "\n")+1);
         (my $indent_prefix= $cur_line) =~ s/\S/ /g;
         local $CodeGen::Cpppp::current_indent_prefix= $indent_prefix;
         local $CodeGen::Cpppp::indent= $self->indent if defined $self->indent;
         # it is "inline" context if non-whitespace occurs on this line already
         my $is_inline= !!($cur_line =~ /\S/);
         local $CodeGen::Cpppp::current_is_inline= $is_inline;

         # Avoid using $_ up to this point so that $_ pases through
         # from the surrounding code into the evals
         my @out= $fn->($self, $out);
         # Expand arrayref and coderefs in the returned list
         @out= @{$out[0]} if @out == 1 && ref $out[0] eq 'ARRAY';
         ref eq 'CODE' && ($_= $_->($self, $out)) for @out;
         @out= grep defined, @out;
         # Now decide how to join this into the code template.
         # If this interpolation does not occur at the beginning of the line,
         my $join_sep= $";
         # Special handling if the user requested a list substitution
         if (ord $s->{eval} == ord '@') {
            $last_char= '' unless defined $last_char;
            if ($self->{autostatementline} && ($last_char eq '{' || $last_char eq ';')
               && substr($text, $s->{pos}+$s->{len}, 1) eq ';'
            ) {
               @out= grep /\S/, @out; # remove items that are only whitespace
               if (!$is_inline && substr($text, $s->{pos}+$s->{len}, 2) eq ";\n") {
                  $join_sep= ";\n";
                  # If no elements, remove the whole line.
                  if (!@out) {
                     $$out =~ s/[ \t]+\Z//;
                     $at+= 2; # skip over ";\n"
                  }
               } else {
                  $join_sep= "; ";
               }
            }
            elsif ($self->{autocomma} && ($last_char eq ',' || $last_char eq '(' || $last_char eq '{')) {
               @out= grep /\S/, @out; # remove items that are only whitespace
               $join_sep= $is_inline? ', ' : ",\n";
               # If no items, or the first nonwhitespace character is a comma,
               # remove the previous comma
               if (!@out || $out[0] =~ /^\s*,/) {
                  $$out =~ s/,(\s*)\Z/$1/;
               }
            }
            elsif ($self->{autoindent} && !$is_inline && $join_sep !~ /\n/) {
               $join_sep .= "\n";
            }
         }
         if (@out) {
            # 'join' doesn't respect concat magic on AntiCharacter :-(
            my $str= shift @out;
            $str .= $join_sep . $_ for @out;
            # Autoindent: if new text contains newline, add current indent to start of each line.
            if ($self->{autoindent} && length $indent_prefix) {
               $str =~ s/\n/\n$indent_prefix/g;
            }
            $$out .= $str;
         }
      }
   }
   $$out .= substr($text, $at);
}

1;

=head1 IMPLEMENTATION

Each template is a class.  When a template is invoked, it creates a new instance
of that class, and during the constructor it "executes" the template body,
generating output.  The execution of the template body may also define attributes
and methods of the new object, and the methods may themselves be templates which
generate more output when called.

The template objects use lexical variabled extensively, since these interpolate
conveniently into C code.  While this is similar to the new Perl object system
in perl 5.40, it is a different implementation that is compatible back to perl
5.20.  It works like this:

  ## param $scalar1= 1;
  ## param @array1= (1,2,3);
  ## param %hash1= ( a => 1, b => 2 );
  ## sub method1($x) {
  "$scalar1 @array1";
  ## }

becomes

  package $GENERATED_NAME;
  use CodeGen::Cpppp::Template -setup => $cpppp_ver;

  sub scalar1 { $_[0]{'$scalar1'} }
  sub array1  { $_[0]{'@array1'} }
  sub hash1   { $_[0]{'%hash1'} }
  sub method1 { goto $_[0]{'&method1'} }
  
  sub _template_init($self, $constructor_parameters) {
    Scalar::Util::weaken($self);
    $self->_template_init_param(scalar1 => \my $scalar1, 1);
    $self->_template_init_param(array1 => \my @array1, (1,2,3));
    $self->_template_init_param(hash1  => \my %hash1, ( a => 1, b => 2 ));
    my sub method1;
    $self->{'&method1'}= \&method1;
    sub method1($x) {
      $self->_template_render_block(1, sub { $scalar1 }, sub { @array1 });
    }
  }

which initializes an internal object structure of:

  {
    '$scalar1'    => 1,
    '@array1'     => [1,2,3],
    '%hash1'      => { a => 1, b => 2 },
    '&method1'    => sub { ... },
  }
