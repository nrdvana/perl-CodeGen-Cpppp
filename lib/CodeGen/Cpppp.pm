package CodeGen::Cpppp;
use v5.20;
use warnings;
use Carp;
use experimental 'signatures';
use version;
use Cwd 'abs_path';
use Scalar::Util 'blessed', 'looks_like_number';
use CodeGen::Cpppp::Template;

our $VERSION= 0; # VERSION
# ABSTRACT: The C Perl-Powered Pre-Processor

=head1 SYNOPSIS

I<Does that mean it's more powerful?  ...Is it more powerful?>

I<Well, it's one layer of abstraction higher, isn't it?  It's not C<m4>.  You see, most
blokes gonna be templating with C<cpp> or C<m4>, you're on C<m4> here all the way up,
all the way up, Where can you go from there? Where?>

I<Nowhere!  Exactly.>

I<What we do is if we need that extra, push, over the cliff, you know what we do?>

I<C<perl>, exactly.>

I<These go to C<perl>.>

B<Input:>

  #! /usr/bin/env cpppp
  ## param $min_bits = 8;
  ## param $max_bits = 16;
  ## param $feature_parent = 0;
  ## param $feature_count = 0;
  ##
  ## for (my $bits= $min_bits; $bits <= $max_bits; $bits <<= 1) {
  struct tree_node_$bits {
    uint${bits}_t  left :  ${{$bits-1}},
                   color:  1,
                   right:  ${{$bits-1}},
                   parent,   ## if $feature_parent;
                   count,    ## if $feature_count;
                   $trim_comma $trim_ws;
  };
  ## }

B<Output:>

  struct tree_node_8 {
    uint8_t  left :  7,
             color:  1,
             right:  7;
  };
  struct tree_node_16 {
    uint16_t left : 15,
             color:  1,
             right: 15;
  };

B<Input:>

  ## my @extra_args;
  extern int fn( char *format, @extra_args );
  ## for ('int a', 'int b') {
  ##   push @extra_args, $_;
  extern int fn_$_( char *format, @extra_args );
  ## }

B<Output:>

  extern int fn( char *format  );
  extern int fn_a( char *format, int a );
  extern int fn_b( char *format, int a, int b );

=head1 DESCRIPTION

B<WARNING: this API is completely and totally unstable>.

This module is a preprocessor for C, or maybe more like a perl template engine
that specializes in generating C code.  Each input file gets translated to Perl
in a way that declares a new OO class, and then you can create instances of that
class with various parameters to generate your C output, or call methods on it
like automatically generating headers or function prototypes.

For the end-user, there is a 'cpppp' command line tool that behaves much like
the 'cpp' tool.

If you have an interest in this, contact me, because I could use help
brainstorming ideas about how to accommodate the most possibilities, here.

B<Possible Future Features:>

=over

=item *

Scan existing headers to discover available macros, structs, and functions on the host.

=item *

Pass a list of headers through the real cpp and analyze the macro output.

=item *

Shell out to a compiler to find 'sizeof' information for structs.

=item *

Directly perform the work of inlining one function into another.

=back

=head1 ATTRIBUTES

=head2 output

An instance of L<CodeGen::Cpppp::Output> that is used as the default C<output>
parameter for all automatically-created templates, thus collecting all their
output.

=cut

sub output { $_[0]{output} }

=head1 CONSTRUCTOR

=head2 new

Bare-bones for now, it accepts whatever hash values you hand to it.

=cut

sub new($class, @attrs) {
   bless {
      output => CodeGen::Cpppp::Output->new,
      @attrs == 1 && ref $attrs[0]? %{$attrs[0]}
      : !(@attrs&1)? @attrs
      : croak "Expected even-length list or hashref"
   }, $class;
}

=head1 METHODS

=head2 process

  $cpppp->process($filename);
  $cpppp->process($handle, $filename, $line_offset);

Process one template, according to L</action>.  The parameters are the same as
for L</parse_cpppp>.

=cut

sub process($self, @input_args) {
   my $m= $self->can('_process__'.$self->action)
      or croak "Undefined action '".$self->action."'";
   $self->$m(@input_args);
}
sub _process__render($self, @input_args) {
   my $pkg= $self->compile_cpppp(@input_args);
   my $params= $pkg->coerce_parameters($self->params);
   $params->{output}= $self->output;
   my $tpl= $pkg->new($params);
   # tpl already wrote to ->output
}
sub _process__dump_template_perl($self, @input_args) {
   my $parse= $self->parse_cpppp(@input_args);
   $self->output->declare_section('template_perl');
   my $code= $self->_gen_perl_template_package($parse);
   require Data::Dumper;
   my $dumper= Data::Dumper->new([ { %$parse, code => '...' } ], [ '$_parse_data' ])
      ->Indent(1)->Sortkeys(1);
   my $ofs= index($code, "\nsub BUILD")+1;
   $self->output->append(template_perl => substr($code, 0, $ofs) . "our \$_parse_data;\n");
   $self->output->append(template_perl => $dumper->Dump);
   $self->output->append(template_perl => substr($code, $ofs));
}

=head2 require_template

  $cpppp->require_template
  
=cut

sub require_template($self, $filename) {
   my $abs_path= abs_path($filename);
   $self->{templates}{$abs_path} ||= $self->compile_template($filename);
}

=head2 compile_cpppp

  $cpppp->compile_cpppp($filename);
  $cpppp->compile_cpppp($input_fh, $filename);
  $cpppp->compile_cpppp(\$scalar_tpl, $filename, $line_offset);

This reads the input file handle (or scalar-ref) and builds a new perl template
class out of it (and dies if there are syntax errors in the template).

Yes, this 'eval's the input, and no, there are not any guards against
malicious templates.  But you run the same risk any time you run someone's
'./configure' script.

=cut

our $next_pkg= 1;
sub compile_cpppp($self, @input_args) {
   my $parse= $self->parse_cpppp(@input_args);
   my $perl= $self->_gen_perl_template_package($parse);
   unless (eval $perl) {
      die "$perl\n\nException: $@\n";
   }
   return $parse->{package};
}

sub _gen_perl_template_package($self, $parse, %opts) {
   my $perl= $parse->{code} // '';
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
      my $dumper= Data::Dumper->new([ { %$parse, code => '...' } ], [ '$_parse_data' ])
         ->Indent(1)->Sortkeys(1);
      push @global,
         'our $_parse_data; '.$dumper->Dump;
   }

   # Build the boilerplate for the template eval
   my $pkg= CodeGen::Cpppp::Template->_create_derived_package($cpppp_ver, $parse);
   $parse->{package}= $pkg;
   $cpppp_ver //= $VERSION;
   $src_filename //= $parse->{filename};
   join '', map "$_\n",
      "package $pkg;",
      # Inject a minimum perl version unless user-provided
      ("use v5.20;")x!(defined $perl_ver),
      # Inject a Template -setup unless user-provided
      ("use CodeGen::Cpppp::Template -setup => $cpppp_ver;")x!($tpl_use_line),
      # All the rest of the user's use/no statements
      @global,
      # Everything after that goes into a sub
      "sub BUILD(\$self, \$constructor_parameters=undef) {",
      "  Scalar::Util::weaken(\$self);",
      # Inject all the lexical functions that need to be in scope
      $pkg->_gen_perl_scope_functions($cpppp_ver),
      qq{# line $src_lineno "$src_filename"},
      $perl,
      "}",
      "1";
}

sub parse_cpppp($self, $in, $filename=undef, $line=undef) {
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
   $self->{cpppp_parse}= {
      autocomma => 1,
      autostatementline => 1,
      autoindent => 1,
      autocolumn => 1,
      filename => $filename,
      colmarker => {},
      coltrack => { },
   };
   my ($perl, $block_group, $tpl_start_line, $cur_tpl)= ('', 1);
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
      if (/^#!/) { # ignore #!
      }
      elsif (/^##/) { # full-line of perl code
         if (defined $cur_tpl || !length $perl) {
            end_tpl();
            $perl .= qq{# line $line "$filename"\n};
         }
         (my $pl= $_) =~ s/^##\s?//;
         $perl .= $self->_transform_template_perl($pl, $line);
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
   my $ct= delete $self->{cpppp_parse}{coltrack};
   _finish_coltrack($ct, $_) for grep looks_like_number($_), keys %$ct;

   $self->{cpppp_parse}{code}= $perl;
   delete $self->{cpppp_parse};
}

sub _transform_template_perl($self, $pl, $line) {
   # If user declares "sub NAME(", convert that to "my sub NAME" so that it can
   # capture refs to the variables of new template instances.
   if ($pl =~ /(my)? \s* \b sub \s* ([\w_]+) \b \s* /x) {
      my $name= $2;
      $self->{cpppp_parse}{template_method}{$name}= { line => $line };
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
      $self->{cpppp_parse}{template_parameter}{$name}= substr($var_name,0,1);
   }
   # If user declares "define name(", convert that to both a method and a define
   elsif ($pl =~ /^ \s* (define) \s* ([\w_]+) (\s*) \(/x) {
      my $name= $2;
      $self->{cpppp_parse}{template_macro}{$name}= 'CODE';
      substr($pl, $-[1], $-[2]-$-[1], qq{my sub $name; \$self->define_template_macro($name => \\&$name); sub });
   }
   $pl;
}

sub _gen_perl_call_code_block($self, $parsed, $indent='') {
   my $codeblocks= $self->{cpppp_parse}{code_block_templates} ||= [];
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
   local our $line= $orig_line || 1;
   local our $parse= $self->{cpppp_parse};
   local our $start;
   local our @subst;
   # Everything in coltrack that survived the last _parse_code_block call
   # ended on the final line of the template.  Set the line numbers to
   # continue into this template.
   for my $c (grep looks_like_number($_), keys $parse->{coltrack}->%*) {
      $parse->{coltrack}{$c}{line}= $line;
   }
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
   for (0..$#subst) {
      my $s= $subst[$_];
      if (exists $s->{colgroup}) {
         my $linestart= (rindex($text, "\n", $s->{pos})+1);
         my $col= $s->{pos} - $linestart;
         $s->{follows_eval}= $prev_eval && $prev_eval->{line} == $s->{line};
         # If same column as previous line, continue the coltracking.
         if ($parse->{coltrack}{$col}) {
            if ($parse->{coltrack}{$col}{members}[-1]{line} == $s->{line} - 1) {
               push @{ $parse->{coltrack}{$col}{members} }, $s;
               $s->{colgroup}= $parse->{coltrack}{$col}{id};
               $parse->{coltrack}{$col}{line}= $s->{line};
               next;
            }
            # column ended prior to this
            _finish_coltrack($parse->{coltrack}, $col);
         }
         # There's no need to create a column unless nonspace to the left
         # Otherwise it would just be normal indent.
         if (substr($text, $linestart, $s->{pos} - $linestart) =~ /\S/) {
            # new column begins
            $s->{colgroup}= $col*10000 + ++$parse->{coltrack}{next_id}{$col};
            $s->{first}= 1;
            $parse->{coltrack}{$col}= {
               id => $s->{colgroup},
               line => $s->{line},
               members => [ $s ],
            };
         }
      }
      else { # Perl expression
         my $expr= substr($text, $s->{pos}, $s->{len});
         # Special case: ${{  }} notation is a shortcut for @{[do{ ... }]}
         if ($expr =~ /^ \$\{\{ (.*) \}\} $/x) {
            $s->{eval}= $1;
         } else {
            $s->{eval}= $expr; # Will need to be filled in with a coderef
         }
         $prev_eval= $s;
      }
   }
   # cleanup
   for my $c (grep looks_like_number($_), keys $parse->{coltrack}->%*) {
      if ($parse->{coltrack}{$c}{line} < $line-1) {
         _finish_coltrack($parse->{coltrack}, $c);
      }
   }
   @subst= grep defined $_->{eval} || defined $_->{colgroup}, @subst;
   
   { text => $text, subst => \@subst, file => $file }
}

=head2 patch_file

  $cpppp->patch_file($filename, $marker, $new_content);

Reads C<$filename>, looking for lines containing C<"BEGIN $merker"> and
C<"END $marker">.  If not found, it dies.  It then replaces all the lines
between those two lines with C<$new_content>, and writes it back to the same
file handle.

Example:

  my $tpl= $cpppp->require_template("example.cp");
  my $out= $tpl->new->output;
  $cpppp->patch_file("project.h", "example.cp", $out->get('public'));
  $cpppp->patch_file("internal.h", "example.cp", $out->get('protected'));

=cut

sub patch_file($self, $fname, $patch_markers, $new_content) {
   $new_content .= "\n" unless $new_content =~ /\n\Z/;
   utf8::encode($new_content);
   open my $fh, '+<', $fname or die "open($fname): $!";
   my $content= do { local $/= undef; <$fh> };
   $content =~ s{(BEGIN \Q$patch_markers\E[^\n]*\n).*?(^[^\n]+?END \Q$patch_markers\E)}
      {$1$new_content$2}sm
      or croak "Can't find $patch_markers in $fname";
   $fh->seek(0,0) or die "seek: $!";
   $fh->print($content) or die "write: $!";
   $fh->truncate($fh->tell) or die "truncate: $!";
   $fh->close or die "close: $!";
   $self;
}

sub overwrite_file_with_backup($self, $fname, $new_content) {
   $new_content .= "\n" unless $new_content =~ /\n\Z/;
   utf8::encode($new_content);
   if (-e $fname) {
      my $n= 0;
      ++$n while -e "$fname.$n";
      require File::Copy;
      File::Copy::copy($fname, "$fname.$n") or die "copy($fname, $fname.$n): $!";
   }
   open my $fh, '>', $fname or die "open($fname): $!";
   $fh->print($new_content) or die "write: $!";
   $fh->close or die "close: $!";
   $self;
}

sub write_sections_to_file($self, $sections, $fname, $patch_markers=undef) {
   my $content= $self->output->get($sections);
   if (defined $patch_markers) {
      $self->patch_file($fname, $patch_markers, $content);
   } else {
      $self->overwrite_file_with_backup($fname, $content);
   }
}

sub _slurp_file($self, $fname) {
   open my $fh, '<', $fname or die "open($fname): $!";
   my $content= do { local $/= undef; <$fh> };
   $fh->close or die "close: $!";
   $content;
}

1;
