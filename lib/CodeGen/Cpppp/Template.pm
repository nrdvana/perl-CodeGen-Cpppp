package CodeGen::Cpppp::Template;

our $VERSION= 0; # VERSION
# ABSTRACT: Base class for template classes created by compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'lexical_subs', 'postderef';
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

Exports symbols C<PUBLIC>, C<PROTECTED>, C<PRIVATE>, C<compile_cpppp>

=back

=cut

package CodeGen::Cpppp::Template::Exports {
   use constant {
      PUBLIC     => 'public',
      PROTECTED  => 'protected',
      PRIVATE    => 'private',
   };
   our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE compile_cpppp format_commandline
     format_timestamp
   );
   our %EXPORT_TAGS= (
      'v0' => [qw( PUBLIC PROTECTED PRIVATE compile_cpppp )],
   );
   #sub util {
   #   return bless [ caller ], __PACKAGE__;
   #}
   #sub _caller { ref $_[0] eq __PACKAGE__? @{+shift} : caller(1) }
   sub compile_cpppp {
      my ($pkg, $filename, $line)= caller;
      my $cpppp;
      if (@_ == 1) {
         # If the argument has any line terminator, assume it is cpppp code
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
      Carp::croak("compile_cppp argument should either be '__DATA__' or lines of cpppp code ending with '\\n'")
         unless defined $cpppp;
      Carp::croak("cpppp source cannot be empty")
         unless length $cpppp;

      my $parse= CodeGen::Cpppp::Template::Parser->new->parse(\$cpppp, $filename, $line);
      $pkg->_init_parse_data($parse);
      $pkg->_build_BUILD_method(
         $pkg->cpppp_version, $parse->{code}, $filename, $line);
   }
   sub format_commandline {
      require CodeGen::Cpppp::Platform;
      CodeGen::Cpppp::Platform::format_commandline(@_);
   }
   sub format_timestamp {
      my @t= gmtime;
      sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ", $t[5]+1900, @t[4,3,2,1,0]
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
   {
      no strict 'refs';
      croak "${class}::BUILD is already defined" if defined &{$class.'::BUILD'};
   }
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
      (map +($_ => $parse->{$_}||0), qw(
         autoindent autocolumn convert_linecomment_to_c89
      )),
      indent => $parse->{indent},
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

sub _render_pod_block($self, $i) {
   if ($self->emit_POD) {
      $self->_finish_render;
      $self->{output}->append($self->{current_output_section} => $self->_parse_data->{pod_blocks}[$i]);
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
         # Identify the indent settings at this point so that other modules can
         # automatically generate matching code.
         my ($last_char)= ($$out =~ /(\S) (\s*) \Z/x);
         my $cur_line= substr($$out, rindex($$out, "\n")+1);
         (my $indent_prefix= $cur_line) =~ s/\S/ /g;
         local $CodeGen::Cpppp::CURRENT_INDENT_PREFIX= $indent_prefix;
         local $CodeGen::Cpppp::INDENT= $self->indent if defined $self->indent;
         # it is "inline" context if non-whitespace occurs on this line already
         my $is_inline= !!($cur_line =~ /\S/);
         local $CodeGen::Cpppp::CURRENT_IS_INLINE= $is_inline;

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

=head1 Parser

You probably don't need to access the parser directly, but it does exist as a
separate object.  Beware the API might still change significantly.

=cut

package CodeGen::Cpppp::Template::Parser {
   use v5.20;
   use warnings;
   use Carp;
   use experimental 'signatures', 'lexical_subs', 'postderef';
   use Scalar::Util 'looks_like_number','blessed';

   our $next_pkg= 1;
   our $VERSION= $CodeGen::Cpppp::Template::VERSION;

=over

=item new

New instance of the parser.  You might choose to supply attributes C<autoindent>
or C<autocolumn>. (see attributes of same name in C<CodeGen::Cpppp>)

=item parse

  $parser->parse($input, $filename, $line=1);

Input is a file handle or scalar ref.  C<$filename> and C<$line> are used for
diagnostic messages.

=item compile

  $class= $parser->compile;

Generate perl code for the template, eval it, and return the newly created
package name.

=back

=cut

   sub new($class, %attrs) {
      $attrs{autoindent} //= 0;
      $attrs{autocolumn} //= 0;
      bless \%attrs, $class;
   }

   sub parse($self, $in, $filename=undef, $line=undef) {
      my @lines;
      if (ref $in eq 'SCALAR') {
         @lines= split /^/m, $$in;
      }
      else {
         my $fh;
         if (ref $in eq 'GLOB' || (blessed($in) && $in->can('getline'))) {
            $fh= $in;
         } else {
            open($fh, '<', $in) or croak "open($in): $!";
         }
         local $/= undef;
         my $text= <$fh>;
         $filename //= "$in";
         utf8::decode($text) or warn "$filename is not encoded as utf-8\n";
         @lines= split /^/m, $text;
      }
      $line //= 1;
      $self->{filename}= $filename;
      $self->{coltrack}= {};
      $self->{colmarker}= {};

      my ($perl, $perl_line, $block_group, $tpl_start_line, $cur_tpl, $pod_start, @pod)
         = ('', 0, 1);
      my sub end_tpl {
         if (defined $cur_tpl && $cur_tpl =~ /\S/) {
            my $parsed= $self->_parse_code_block($cur_tpl, $filename, $tpl_start_line);
            my $current_indent= $perl =~ /\n([ \t]*).*\n\Z/? $1 : '';
            $current_indent .= '  ' if $perl =~ /\{ *\n\Z/;
            $perl .= $self->_gen_perl_call_code_block($parsed, $current_indent);
         }
         $cur_tpl= undef;
      };
      for (@lines) {
         if (/^=(\w+)/) {
            if (!defined $pod_start) {
               $pod_start= $line;
               if (defined $cur_tpl) {
                  # trim off any blank line that occurs right before the pod.
                  chomp $cur_tpl;
                  end_tpl();
               }
            }
            push @pod, $_;
            if (@pod > 1 && $1 eq 'cut') {
               my $current_indent= $perl =~ /\n([ \t]*).*\n\Z/? $1 : '';
               $current_indent .= '  ' if $perl =~ /\{ *\n\Z/;
               $perl .= $self->_gen_perl_emit_pod_block(join('', @pod), $filename, $pod_start, $current_indent);
               @pod= ();
               $pod_start= undef;
            }
         }
         elsif (defined $pod_start) {
            push @pod, $_;
         }
         elsif (/^#!/) { # ignore #!
         }
         elsif (/^##/) { # full-line of perl code
            end_tpl() if defined $cur_tpl;
            $perl .= qq{# line $line "$filename"\n} unless $perl_line == $line;
            (my $pl= $_) =~ s/^##\s?//;
            $perl .= $self->_transform_template_perl($pl, $line);
            $perl_line= $line+1;
         }
         elsif (/^(.*?) ## ?((?:if|unless|for|while|unless) .*)/) { # perl conditional suffix, half tpl/half perl
            my ($tpl, $pl)= ($1, $2);
            end_tpl() if defined $cur_tpl;
            $tpl_start_line= $line;
            $cur_tpl= $tpl;
            end_tpl();
            $perl =~ s/;\s*$//; # remove semicolon
            $pl .= ';' unless $pl =~ /;\s*$/; # re-add it if user didn't
            $perl .= qq{\n# line $line "$filename"\n    $pl\n};
            $perl_line= $line + 1;
         }
         else { # default is to assume a line of template
            if (!defined $cur_tpl) {
               $tpl_start_line= $line;
               $cur_tpl= '';
            }
            $cur_tpl .= $_;
         }
      } continue { ++$line }
      end_tpl() if defined $cur_tpl;

      # Resolve final bits of column tracking
      my $ct= delete $self->{coltrack};
      _finish_coltrack($ct, $_) for grep looks_like_number($_), keys %$ct;

      # Finish detecting indent, if not specified
      if (!defined $self->{indent}) {
         $self->{indent}
            = $self->_guess_indent(delete $self->{indent_seen} || []);
      }

      $self->{code}= $perl;
      $self;
   }

   sub _guess_indent($self, $indent_seen) {
      my %evidence;
      my $prev;
      for (@$indent_seen) {
         if (!defined $prev || length($_) <= length($prev)) {
            $evidence{/^\t+$/? "\t" : /\t/? 'mixed_tabs' : $_}++;
         }
         elsif (length($prev) < length($_)) {
            if ($prev =~ /\t/ || $_ =~ /\t/) {
               if ($prev =~ /^\t+$/ && $_ =~ /^\t+$/) {
                  $evidence{"\t"}++;
               } else {
                  $evidence{mixed_tabs}++;
               }
            } else {
               my $step= length($_) - length($prev);
               if (0 == length($prev) % $step) {
                  $evidence{' 'x$step}++;
               }
            }
         }
         $prev= $_;
      }
      my $guess;
      for (keys %evidence) {
         $guess= $_ if !defined $guess
            || $evidence{$_} > $evidence{$guess}
            || ($evidence{$_} == $evidence{$guess} && $_ lt $guess);
      }
      return defined $guess && $guess eq 'mixed_tabs'? undef : $guess;
   }

   sub _transform_template_perl($self, $pl, $line) {
      # If user declares "sub NAME(", convert that to "my sub NAME" so that it can
      # capture refs to the variables of new template instances.
      if ($pl =~ /^ \s* (my \s+)? sub \s* ([\w_]+) \b \s* /x) {
         my $name= $2;
         $self->{template_method}{$name}= { line => $line };
         my $ofs= $-[0];
         my $ofs2= defined $1? $+[1] : $ofs;
         substr($pl, $ofs, $ofs2-$ofs, "my sub $name; \$self->define_template_method($name => \\&$name);");
      }
      # If user declares 'param $foo = $x' adjust that to 'param my $foo = $x'
      if ($pl =~ /^ \s* (param) \b /xgc) {
         my $ofs= $-[1];
         # It's an error if the thing following isn't a variable name
         $pl =~ /\G \s* ( [\$\@\%] [\w_]+ ) /xgc
            or croak("Expected variable name (including sigil) after 'param'");
         my $var_name= $1;
         $pl =~ /\G \s* ([;=]) /xgc
            or croak("Parameter declaration $var_name must be followed by '=' or ';'");
         my $term= $1;
         my $name= substr($var_name, 1);
         substr($pl, $ofs, $+[0]-$ofs, qq{param '$name', \\my $var_name }.($term eq ';'? ';' : ','));
         $self->{template_parameter}{$name}= substr($var_name,0,1);
      }
      # If user declares "define name(", convert that to both a method and a define
      elsif ($pl =~ /^ \s* (define) \s+ ([\w_]+) (\s*) \(/x) {
         my $name= $2;
         $self->{template_macro}{$name}= 'CODE';
         substr($pl, $-[1], $-[2]-$-[1], qq{my sub $name; \$self->define_template_macro($name => \\&$name); sub });
      }
      $pl;
   }

   sub _gen_perl_call_code_block($self, $parsed, $indent='') {
      my $codeblocks= $self->{code_block_templates} ||= [];
      push @$codeblocks, $parsed;
      my $code= $indent.'$self->_render_code_block('.$#$codeblocks;
      my %cache;
      my $i= 0;
      my $cur_line= 0;
      for my $s (@{$parsed->{subst}}) {
         if (defined $s->{eval}) {
            # No need to create more than one anonsub for the same expression
            if (defined $cache{$s->{eval}}) {
               $s->{eval_idx}= $cache{$s->{eval}};
               next;
            }
            $cache{$s->{eval}}= $s->{eval_idx}= $i++;
            my $sig= $s->{eval} =~ /self|output/? '($self, $output)' : '';
            if ($s->{line} == $cur_line) {
               $code .= qq{, sub${sig}{ $s->{eval} }};
            } elsif ($s->{line} == $cur_line+1) {
               $cur_line++;
               $code .= qq{,\n$indent  sub${sig}{ $s->{eval} }};
            } else {
               $code .= qq{,\n# line $s->{line} "$parsed->{file}"\n$indent  sub${sig}{ $s->{eval} }};
               $cur_line= $s->{line};
               $cur_line++ for $s->{eval} =~ /\n/g;
            }
         }
      }
      $code .= "\n$indent" if index($code, "\n") >= 0;
      $code . ");\n";
   }

   sub _gen_perl_emit_pod_block($self, $pod, $file, $line, $indent='') {
      my $pod_blocks= $self->{pod_blocks} ||= [];
      push @$pod_blocks, { pod => $pod, file => $file, line => $line };
      return $indent.'$self->_render_pod_block('.$#$pod_blocks.");\n";
   }

   sub _finish_coltrack($coltrack, $col) {
      # did it eventually have an eval to the left?
      if (grep $_->{follows_eval}, $coltrack->{$col}{members}->@*) {
         $coltrack->{$col}{members}[-1]{last}= 1;
      } else {
         # invalidate them all, they won't become unaligned anyway.
         $_->{colgroup}= undef for $coltrack->{$col}{members}->@*;
      }
      delete $coltrack->{$col};
   }

   sub _parse_code_block($self, $text, $file=undef, $orig_line=undef) {
      $text .= "\n" unless substr($text,-1) eq "\n";
      if ($text =~ /^# line (\d+) "([^"]+)"/) {
         $orig_line= $1-1;
         $file= $2;
      }
      # Check if we can auto-detect the indent
      unless (defined $self->{indent}) {
         # Find all total indents used in this code, but only count lines that
         # were preceeded by ';' or '{' or ')' followed by lines starting with a
         # word or variable substitution.
         push @{$self->{indent_seen}}, $1 while $text =~ /[;{)]\s*\n([ \t]+)[\w\$\@]/g;
      }
      local our $line= $orig_line || 1;
      # Everything in coltrack that survived the last _parse_code_block call
      # ended on the final line of the template.  Set the line numbers to
      # continue into this template.
      for my $c (grep looks_like_number($_), keys $self->{coltrack}->%*) {
         $self->{coltrack}{$c}{line}= $line;
      }
      local our $start;
      local our @subst;
      local $_= $text;
      # Parse and record the locations of the embedded perl statements
      ()= m{
         # Rough approximation of continuation of perl expressions in quoted strings
         (?(DEFINE)
            (?<BALANCED_EXPR> (?>
                 \{ (?&BALANCED_EXPR) \}
               | \[ (?&BALANCED_EXPR) \]
               | \( (?&BALANCED_EXPR) \)
               | [^[\](){}\n]+
               | \n (?{ $line++ })
            )* )
         )
         
         # Start of a perl expression in a quoted string
         [\$\@] (?{ $start= -1+pos }) 
            (?:
              \{ (?&BALANCED_EXPR) \}           # 
              | [\w_]+                          # plain variable
               (?:                              # maybe followed by ->[] or similar
                  (?: -> )?
                  (?: \{ (?&BALANCED_EXPR) \} | \[ (?&BALANCED_EXPR) \] )
               ) *                       
            ) (?{ push @subst, { pos => $start, len => -$start+pos, line => $line }; })
         
         # Track what line we're on
         | \n     (?{ $line++ })
         
         # Column alignment detection for the autocolumn feature
         | (?{ $start= pos; }) [ \t]{2,}+ (?{
               push @subst, { pos => pos, len => 0, line => $line, colgroup => undef };
           })
      }xg;
      
      my $prev_eval;
      for my $s (@subst) {
         if (exists $s->{colgroup}) {
            my $linestart= (rindex($text, "\n", $s->{pos})+1);
            my $col= $s->{pos} - $linestart;
            $s->{follows_eval}= $prev_eval && $prev_eval->{line} == $s->{line};
            # If same column as previous line, continue the coltracking.
            if ($self->{coltrack}{$col}) {
               if ($self->{coltrack}{$col}{members}[-1]{line} == $s->{line} - 1) {
                  push @{ $self->{coltrack}{$col}{members} }, $s;
                  $s->{colgroup}= $self->{coltrack}{$col}{id};
                  $self->{coltrack}{$col}{line}= $s->{line};
                  next;
               }
               # column ended prior to this
               _finish_coltrack($self->{coltrack}, $col);
            }
            # There's no need to create a column unless nonspace to the left
            # Otherwise it would just be normal indent.
            if (substr($text, $linestart, $s->{pos} - $linestart) =~ /\S/) {
               # new column begins
               $s->{colgroup}= $col*10000 + ++$self->{coltrack}{next_id}{$col};
               $s->{first}= 1;
               $self->{coltrack}{$col}= {
                  id => $s->{colgroup},
                  line => $s->{line},
                  members => [ $s ],
               };
            }
         }
         else { # Perl expression
            my $expr= substr($text, $s->{pos}, $s->{len});
            # Special case: ${{  }} notation is a shortcut for @{[do{ ... }]}
            $expr =~ s/^ \$\{\{ (.*) \}\} $/$1/x;
            # When not inside a string, ${foo} becomes ambiguous with ${foo()}
            $expr =~ s/^ ([\$\@]) \{ ([\w_]+) \} /$1$2/x;
            $s->{eval}= $expr;
            $prev_eval= $s;
         }
      }
      # Clean up any tracked column that ended before the final line of the template
      for my $c (grep looks_like_number($_), keys $self->{coltrack}->%*) {
         _finish_coltrack($self->{coltrack}, $c)
            if $self->{coltrack}{$c}{line} < $line-1;
      }
      @subst= grep defined $_->{eval} || defined $_->{colgroup}, @subst;
      
      { text => $text, subst => \@subst, file => $file }
   }

   sub compile($self, @input_args) {
      $self->parse(@input_args) if @input_args;
      croak "Call ->parse first" unless defined $self->{code};
      my $perl= $self->_gen_perl_template_package;
      unless (eval $perl) {
         croak "$perl\n\nException: $@\n";
      }
      return $self->{package};
   }

   sub _gen_perl_template_package($self, %opts) {
      my $perl= $self->{code} // '';
      my ($src_lineno, $src_filename, @global, $perl_ver, $cpppp_ver, $tpl_use_line)= (1);
      # Extract all initial 'use' and 'no' statements from the script.
      # If they refer to perl or CodeGen:::Cpppp, make a note of it.
      while ($perl =~ s/^ ( [ \t]+ | [#] .* | use [^;]+ ; | no [^;]+ ; \s* ) \n//gx) {
         my $line= $1;
         push @global, $line;
         $perl_ver= version->parse($1)
            if $line =~ /use \s+ ( v.* | ["']? [0-9.]+ ["']? ) \s* ; /x;
         $cpppp_ver= version->parse($1)
            if $line =~ /use \s+ CodeGen::Cpppp \s* ( v.* | ["']? [0-9.]+ ["']? ) \s* ; /x;
         $tpl_use_line= 1
            if $line =~ /use \s+ CodeGen::Cpppp::Template \s+/;
         if ($line =~ /^# line (\d+) "([^"]+)"/) {
            $src_lineno= $1;
            $src_filename= $2;
         } else {
            $src_lineno+= 1 + (()= $line =~ /\n/g);
         }
      }
      if ($opts{with_data}) {
         require Data::Dumper;
         my $dumper= Data::Dumper->new([ { %$self, code => '...' } ], [ '$_parse_data' ])
            ->Indent(1)->Sortkeys(1);
         push @global,
            'our $_parse_data; '.$dumper->Dump;
      }

      # Build the boilerplate for the template eval
      my $pkg= CodeGen::Cpppp::Template->_create_derived_package($cpppp_ver, $self);
      $self->{package}= $pkg;
      $cpppp_ver //= $CodeGen::Cpppp::Template::VERSION;
      $src_filename //= $self->{filename};
      join '', map "$_\n",
         "package $pkg;",
         # Inject a minimum perl version unless user-provided
         ("use v5.20;")x!(defined $perl_ver),
         # Inject a Template -setup unless user-provided
         ("use CodeGen::Cpppp::Template -setup => $cpppp_ver;")x!($tpl_use_line),
         # All the rest of the user's use/no statements
         @global,
         # Everything after that goes into a BUILD method
         $pkg->_gen_BUILD_method($cpppp_ver, $perl, $src_filename, $src_lineno),
         "1";
   }

}

1;
