package CodeGen::Cpppp::Template;

# VERSION
# ABSTRACT: Base class for template classes created by compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'postderef';
use Scalar::Util 'looks_like_number';
use Hash::Util;
use CodeGen::Cpppp::Output;
use CodeGen::Cpppp::AntiCharacter;
use Exporter ();
require version;

=head1 DESCRIPTION

This is the base class for all Template classes compiled from cpppp source.
It also defines the exports that set up the scope for evaluating the template.

=head1 EXPORTS

=over

=item C<-setup>

Initializes @ISA to CodeGen::Cpppp::Template (unless it was already initialized)
and sets the compiler flags for strict, warnings, utf8, lexical_subs, signatures,
and postderef.

=item C<:v0>

Exports symbols C<PUBLIC>, C<PROTECTED>, C<PRIVATE>

=back

=cut

package CodeGen::Cpppp::Template::Exports {
   use constant {
      PUBLIC     => 'public',
      PROTECTED  => 'protected',
      PRIVATE    => 'private',
   };
   our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE compile_cpppp );
   our %EXPORT_TAGS= (
      'v0' => \@EXPORT_OK,
   );
   sub compile_cpppp {
      my ($pkg, $filename, $line)= caller;
      my $cpppp;
      if (@_ == 1) {
         # If the argument is multiple lines, assume it is cpppp code
         if (index($_[0], "\n") >= 0) {
            $cpppp= $_[0];
         }
         # if the argument is '__DATA__', read it from DATA
         elsif ($_[0] eq '__DATA__') {
            no strict 'refs';
            my $fh= *{${pkg}.'::DATA'};
            my $pos= $fh->tell;
            local $/= undef;
            $cpppp= <$fh>;
            # now find out what line __DATA__ started on
            eval {
               $fh->seek(0,0);
               $/= \$pos;
               $line= 1 + scalar(()= <$fh> =~ /\n/g);
            } or Carp::carp("Can't determine line number of __DATA__");
            close $fh;
         }
      }
      Carp::croak("parse_cppp argument should either be '__DATA__' or lines of cpppp code ending with '\\n'")
         unless defined $cpppp;
      Carp::croak("cpppp source cannot be empty")
         unless length $cpppp;
      
      my $parse= CodeGen::Cpppp->new->parse_cpppp(\$cpppp, $filename, $line);
      $pkg->_init_parse_data($parse);
      $pkg->_build_BUILD_method(
         $pkg->cpppp_version, $parse->{code}, $filename, $line);
   }
}

sub _tag_for_version($ver) {
   return ':v0';
}

sub import {
   my $class= $_[0];
   my $caller= caller;
   for (my $i= 1; $i < @_; $i++) {
      if ($_[$i] eq '-setup') {
         my $ver= version->parse($_[$i+1]);
         splice(@_, $i, 2, _tag_for_version($ver));
         $class->_setup_derived_package($caller, $ver);
      }
   }
   splice(@_, 0, 1, 'CodeGen::Cpppp::Template::Exports');
   goto \&Exporter::import;
}

our $_next_pkg= 1;
sub _create_derived_package($class, $cpppp_ver, $parse_data) {
   my $pkg= 'CodeGen::Cpppp::Template::_'.$_next_pkg++;
   no strict 'refs';
   @{"${pkg}::ISA"}= ( $class );
   ${"${pkg}::cpppp_version"}= $cpppp_ver;
   $pkg->_init_parse_data($parse_data);
}

sub _setup_derived_package($class, $pkg, $cpppp_ver) {
   strict->import;
   warnings->import;
   utf8->import;
   experimental->import(qw( lexical_subs signatures postderef ));

   no strict 'refs';
   @{"${pkg}::ISA"}= ( $class ) unless @{"${pkg}::ISA"};
   ${"${pkg}::cpppp_version"}= $cpppp_ver;
}

sub _init_parse_data($class, $parse_data) {
   no strict 'refs';
   ${"${class}::_parse_data"}= $parse_data;
   # Create accessors for all of the attributes declared in the template.
   for (keys $parse_data->{template_parameter}->%*) {
      my $name= $_;
      *{"${class}::$name"}= sub { $_[0]{$name} };
   }
   # Expose all of the functions declared in the template
   for (keys $parse_data->{template_method}->%*) {
      my $name= $_;
      *{"${class}::$name"}= sub {
         my $m= shift->{template_method}{$name}
            or croak "Template execution did not define method '$name'";
         goto $m;
      };
   }
   $class;
}

sub cpppp_version($class) {
   no strict 'refs';
   ${"${class}::cpppp_version"} // __PACKAGE__->VERSION
}

sub _gen_perl_scope_functions($class, $cpppp_ver) {
   return (
      '# line '. (__LINE__+1) . ' "' . __FILE__ . '"',
      'my sub param { unshift @_, $self; goto $self->can("_init_param") }',
      'my sub define { unshift @_, $self; goto $self->can("define_template_macro") }',
      'my sub section { unshift @_, $self; goto $self->can("current_output_section") }',
      'my sub template { unshift @_, $self->context; goto $self->context->can("new_template") }',
      'my $trim_comma= CodeGen::Cpppp::AntiCharacter->new(qr/,/, qr/\s*/);',
      'my $trim_ws= CodeGen::Cpppp::AntiCharacter->new(qr/\s*/);',
   );
}

sub _gen_BUILD_method($class, $cpppp_ver, $perl, $src_filename, $src_lineno) {
   return
      "sub ${class}::BUILD(\$self, \$constructor_parameters=undef) {",
      "  Scalar::Util::weaken(\$self);",
      # Inject all the lexical functions that need to be in scope
      $class->_gen_perl_scope_functions($cpppp_ver),
      qq{# line $src_lineno "$src_filename"},
      $perl,
      "}",
}

sub _build_BUILD_method($class, $version, $perl, $src_filename, $src_lineno) {
   no strict 'refs';
   croak "${class}::BUILD is already defined" if defined &{$class.'::BUILD'};
   croak "Compile failed for ${class}::BUILD() : $@"
      unless eval join "\n",
         $class->_gen_BUILD_method($version, $perl, $src_lineno, $src_filename),
         '1';
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

sub _parse_data($class) {
   $class = ref $class if ref $class;
   no strict 'refs';
   return ${"${class}::_parse_data"};
}

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
   my $parse= $class->_parse_data;
   # Make sure each attr is the correct type of ref, for the params.
   for (keys %attrs) {
      if (my $p= $parse->{template_parameter}{$_}) {
         if ($p eq '@') { ref $attrs{$_} eq 'ARRAY' or croak("Expected ARRAY for parameter $_"); }
         elsif ($p eq '%') { ref $attrs{$_} eq 'HASH' or croak("Expected HASH for parameter $_"); }
      }
      else {
         croak("Unknown parameter '$_' to template $parse->{filename}")
            unless $class->can($_);
      }
   }

   my $self= bless {
      autocomma => 1,
      autostatementline => 1,
      (map +($_ => $parse->{$_}), qw(
         autoindent autocolumn
         context
      )),
      output => CodeGen::Cpppp::Output->new,
      current_output_section => 'private',
      %attrs,
   }, $class;
   Scalar::Util::weaken($self->{context})
      if $self->{context};
   $self->BUILD(\%attrs);
   $self->flush;
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
   my $parse= $class->_parse_data;
   for my $k (keys $parse->{template_parameter}->%*) {
      my $p= $parse->{template_parameter}{$k};
      my $v= $params->{$p.$k} // $params->{$k};
      next unless defined $v;
      if ($p eq '@') {
         $v= ref $v eq 'HASH'? [ keys %$v ] : [ $v ]
            unless ref $v eq 'ARRAY';
      } elsif ($p eq '%') {
         # If it isn't a hash, treat it like a list that needs added to a set
         $v= { map +($_ => 1), ref $v eq 'ARRAY'? @$v : ($v) }
            unless ref $v eq 'HASH';
      }
      $ret{$k}= $v;
   }
   \%ret;
}

sub _init_param($self, $name, $ref, @initial_value) {
   if (exists $self->{$name}) {
      # Assign the value received from constructor to the variable in the template
        ref $ref eq 'SCALAR'? ($$ref= $self->{$name})
      : ref $ref eq 'ARRAY' ? (@$ref= @{$self->{$name} || []})
      : ref $ref eq 'HASH'  ? (%$ref= %{$self->{$name} || {}})
      : croak "Unhandled ref type ".ref($ref);
   } else {
        ref $ref eq 'SCALAR'? ($$ref= $initial_value[0])
      : ref $ref eq 'ARRAY' ? (@$ref= @initial_value)
      : ref $ref eq 'HASH'  ? (%$ref= @initial_value)
      : croak "Unhandled ref type ".ref($ref);
   }
   
   # Now store the variable of the template directly into this hash
   ref $ref eq 'SCALAR'? Hash::Util::hv_store(%$self, $name, $$ref)
   : ($self->{$name}= $ref);
   $ref;
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

=head2 define_template_method

This is called during the template constructor to bind the lexical subs of the
template to methods of the object being created.

=cut

sub define_template_method($self, $name, $code) {
   $self->{template_method}{$name}= $code;
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
   my $block= $self->_parse_data->{code_block_templates}[$i];
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
         # Avoid using $_ up to this point so that $_ pases through
         # from the surrounding code into the evals
         my @out= $fn->($self, $out);
         # Expand arrayref and coderefs in the returned list
         @out= @{$out[0]} if @out == 1 && ref $out[0] eq 'ARRAY';
         ref eq 'CODE' && ($_= $_->($self, $out)) for @out;
         @out= grep defined, @out;
         # Now decide what to join them with.
         my $join_sep= $";
         my $indent= '';
         my ($last_char)= ($$out =~ /(\S) (\s*) \Z/x);
         my $cur_line= substr($$out, rindex($$out, "\n")+1);
         my $inline= $cur_line =~ /\S/;
         if ($self->{autoindent}) {
            ($indent= $cur_line) =~ s/\S/ /g;
         }
         # Special handling if the user requested a list substitution
         if (ord $s->{eval} == ord '@') {
            $last_char= '' unless defined $last_char;
            if ($self->{autostatementline} && ($last_char eq '{' || $last_char eq ';')
               && substr($text, $s->{pos}+$s->{len}, 1) eq ';'
            ) {
               @out= grep /\S/, @out; # remove items that are only whitespace
               if (!$inline && substr($text, $s->{pos}+$s->{len}, 2) eq ";\n") {
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
               $join_sep= $inline? ', ' : ",\n";
               # If no items, or the first nonwhitespace character is a comma,
               # remove the previous comma
               if (!@out || $out[0] =~ /^\s*,/) {
                  $$out =~ s/,(\s*)\Z/$1/;
               }
            }
            elsif ($self->{autoindent} && !$inline && $join_sep !~ /\n/) {
               $join_sep .= "\n";
            }
         }
         if (@out) {
            # 'join' doesn't respect concat magic on AntiCharacter :-(
            my $str= shift @out;
            $str .= $join_sep . $_ for @out;
            # Autoindent: if new text contains newline, add current indent to start of each line.
            if ($self->{autoindent} && $indent) {
               $str =~ s/\n/\n$indent/g;
            }
            $$out .= $str;
         }
      }
   }
   $$out .= substr($text, $at);
}

1;
