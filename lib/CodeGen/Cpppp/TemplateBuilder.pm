package CodeGen::Cpppp::TemplateBuilder;

# utility function defined before anything is in scope
sub _clean_scope_eval { eval $_[0] }

our $VERSION= 0; # VERSION
# ABSTRACT: Object which builds Template classes from cpppp source code

use v5.20;
use warnings;
use experimental qw/ signatures lexical_subs postderef /;
use Scalar::Util qw/ looks_like_number blessed /;
use Carp ();
my sub croak { goto \&Carp::croak }
my sub carp  { goto \&Carp::carp }

=head1 DESCRIPTION

This object builds classes for templates, and also translates cpppp syntax into
perl.  The process of building a template class for a cppp source file is
roughly:

  CodeGen::Cpppp::TemplateBuilder
    ->new
    ->parse($file, $filename)
    ->compile;

Each template class retains a reference to its builder, so you can also call
methods to build the template programmatically.

  package MyTemplate;
  use CodeGen::Cpppp::Template -setup => 0;
  template_builder->parse(<<CPPPP, __FILE__, __LINE__);
  Test 1 2 3
  CPPPP

=head1 CONSTRUCTOR

=head2 new

Accepts a hash or key/value list of attributes.  If you do not specify the
C<template_class> attribute, a generic package name will be chosen for you, such
as 'CodeGen::Cpppp::Template::_1'.

=head1 ATTRIBUTES

=head2 template_class

Must be a safe name matching C<< /^\w+(::\w+)*$/ >>.  It must not already be
initialized (as determined by '@ISA' or method 'new').

=head2 cpppp_source

The total source code that has been passed to C<parse> (which can be called
multiple times).  If you pass this to the constructor, it immediately parses
the string, but this prevents you from specifying the filename and line number.

=head2 autoindent

Whether to attempt automatic indentation of C code generated by the template.

=head2 autocolumn

Whether to attempt automatic column alignment of C code generated by the template.

=head2 perl_source

The total source code which was eval'd to produce the template's
C<_template_init> method, which runs during the constructor.

=head2 params

The public parameters defined (so far) by the cpppp source.

=head2 subs

The public subroutines defined (so far) by the cpppp source.  Every occurrence
of C<sub NAME> which was not declared as C<my sub NAME> and where NAME does not
begin with an underscore will automatically become a public method of the
template.

=head2 macros

The C macros defined (so far) by the cpppp source.

=cut

our $next_pkg= 0;
sub new($class, %attrs) {
   _assert_valid_template_class(
      $attrs{template_class} //= 'CodeGen::Cpppp::Template::_'.++$next_pkg
   );
   my $src= delete $attrs{cpppp_source};
   $attrs{cpppp_source}= '';
   $attrs{autoindent} //= 0;
   $attrs{autocolumn} //= 0;
   $attrs{perl_source}= '';
   $attrs{params}= {};
   $attrs{subs}= {};
   $attrs{macros}= {};
   $attrs{compile_todo}= [];
   my $self= bless \%attrs, $class;
   $self->parse($src) if defined $src;
   $self;
}

sub _assert_valid_template_class($class_name) {
   croak "Invalid template_class name: '$class_name'"
      unless $class_name =~ /^\w+(::\w+)*\Z/; # make sure it's safe for eval
   no strict 'refs';
   croak "Class $class_name is already initialized"
      if $class_name->can('new') || scalar @{$class_name.'::ISA'};
}

=head1 METHODS

=head2 parse

$builder->parse($input, $filename, $line=1);

Input is a file handle or scalar ref.  C<$filename> and C<$line> are passed
through to the generated perl so that the perl diagnostic messages reference
the original cpppp source.

=head2 compile

$class= $builder->compile;

Finish installing any missing methods of the new class.  This gets called
lazily the first time the template class constructor runs if any changes
have been made to the template since the last compilation.  You might want to
call it directly to receive any errors before assuming the template is ready
for use.

=cut

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
   # If the class is not initialized yet, set up the basics
   $self->_setup_class unless $self->template_class->can('template_builder');
   # For each new param or sub, create an accessor.  These need processed in
   # the order they were seen in the source, to resolve name clashes
   # consistently.
   if (my $todo= delete $self->{compile_todo}) {
      for (@$todo) {
         my ($type, $name)= @$_;
         if ($type eq 'param') {
            $self->_install_param_accessor($name);
         } elsif ($type eq 'sub') {
            $self->_install_sub_accessor($name);
         } else { croak "BUG, invalid todo item type '$type'" }
      }
   }
   # re-eval the _template_init function.
   $self->_install_template_init;
   return $self->template_class;
}

sub _setup_class($self) {
   my ($parent, $derived)= ('CodeGen::Cpppp::Template', $self->template_class);
   _assert_valid_template_class($derived);
   no strict 'refs';
   push @{"${derived}::ISA"}, $parent;
   *{"${derived}::template_parameters"}= $self->params;
   ${"${derived}::template_builder"}= $self;
   # Declare package accessors
   _clean_scope_eval(<<END) or croak $@;
      sub ${derived}::template_parameters { \%${derived}::template_parameters }
      1;
END
}

sub _import_template_perl_flags($class) {
   feature->import(':5.20');
   warnings->import;
   utf8->import;
   experimental->import(qw( lexical_subs signatures postderef ));
}

sub _declare_param($self, $name_with_sigil) {
   $name_with_sigil =~ /^[@%\$]\w+\Z/ or croak "Invalid parameter name '$name_with_sigil'";
   my $name= substr($name_with_sigil, 1);
   croak "template parameter '$name' already exists"
      if defined $self->params->{$name};
   $self->params->{$name}= { var => $name_with_sigil };
   push @{$self->{compile_todo}}, [ param => $name_with_sigil ];
}

sub _install_param_accessor($self, $name_with_sigil) {
   $name_with_sigil =~ /^[@%\$]\w+\Z/ or croak "Invalid parameter name '$name_with_sigil'";
   my $name= substr($name_with_sigil, 1);
   # If an attribute of this name does not exist yet, create an accessor for it
   if ($self->template_class->can($name)) {
      carp "template parameter '$name' is masked by an existing attribute";
   } else {
      _clean_scope_eval("sub ".$self->template_class."::${name} { \$_[0]{'$name_with_sigil'} }")
         or croak $@;
   }
}

sub _declare_sub($self, $name) {
   if ($name =~ /^\w+\Z/) {
      if ($class->_template_subs->{$name}) {
         carp "Template 'sub $name' declared twice";
      } else {
         push @{$self->{compile_todo}}, [ sub => $name ];
      }
   }
}

sub _install_sub_accessor($self, $name) {
   my $class= $self->template_class;
   _clean_scope_eval(<<PERL) or croak $@;
   sub ${class}::${name} {
      my $m= shift->{'&$name'}
         or Carp::croak(q{Template execution did not define 'sub $name'});
      goto $m;
   }
PERL
}

sub _install_template_init($self) {
   my $class= $self->template_class;
   no strict 'refs';
   undef *{$class.'::_template_init'};
   _clean_scope_eval($self->_generate_template_init_src)
      or croak "Failed to compile template: $@";
}

sub _generate_template_init_src($self) {
   my $class= $self->template_class;
   '# line '. (__LINE__+1) . ' "' . __FILE__ . '"',
   . "package $class;\n"
   . "BEGIN { CodeGen::Cpppp::TemplateBuilder::_import_template_perl_flags() }\n"
   . 'sub _template_init($self, $constructor_parameters=undef) {'."\n"
   . '   Scalar::Util::weaken($self);'."\n"
   # Inject all the lexical functions that need to be in scope
   . join('', map "   $_\n", $self->_template_scope_functions)
   . $self->{perl_source}
   . "}\n"
}

sub _template_scope_functions($self) {
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

#   # Extract all initial 'use' and 'no' statements from the script.
#   # If they refer to perl or CodeGen:::Cpppp, make a note of it.
#   while ($perl =~ s/^ ( [ \t]+ | [#] .* | use [^;]+ ; | no [^;]+ ; \s* ) \n//gx) {
#      my $line= $1;
#      push @global, $line;
#      $perl_ver= version->parse($1)
#         if $line =~ /use \s+ ( v.* | ["']? [0-9.]+ ["']? ) \s* ; /x;
#      $cpppp_ver= version->parse($1)
#         if $line =~ /use \s+ CodeGen::Cpppp \s* ( v.* | ["']? [0-9.]+ ["']? ) \s* ; /x;
#      $tpl_use_line= 1
#         if $line =~ /use \s+ CodeGen::Cpppp::Template \s+/;
#      if ($line =~ /^# line (\d+) "([^"]+)"/) {
#         $src_lineno= $1;
#         $src_filename= $2;
#      } else {
#         $src_lineno+= 1 + (()= $line =~ /\n/g);
#      }
#   }

undef *looks_like_number;
undef *blessed;
1;