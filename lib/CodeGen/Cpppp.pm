package CodeGen::Cpppp;
use v5.20;
use warnings;
use Carp;
no warnings 'experimental::signatures';
use feature 'signatures';
use version;
use Cwd 'abs_path';
use Scalar::Util 'blessed';
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
  ## for (my $bits= 8; $bits <= 16; $bits <<= 1) {
  struct tree_node_$bits {
    uint${bits}_t  left:  ${{$bits-1}},
                   color: 1,
                   parent,   ## if $feature_parent;
                   right: ${{$bits-1}};
  };
  ## }

B<Output:>

  struct tree_node_8 {
    uint8_t left:  7,
            color: 1,
            right: 7;
  };
  struct tree_node_16 {
    uint16_t left:  15,
             color: 1,
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

=head2 action

The main action to perform in 'process_template':

=over

=item C<'render'>

Render the template.

=item C<'dump_template_perl'>

Render the perl code that would get eval'd for each template.

=back

=cut

sub action { $_[0]{action} }

=head1 CONSTRUCTOR

=head2 new

Bare-bones for now, it accepts whatever hash values you hand to it.

=cut

sub new($class, @attrs) {
   bless {
      out => CodeGen::Cpppp::Output->new,
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
   print $pkg->new->render->get;
}
sub _process__dump_template_perl($self, @input_args) {
   my $parse= $self->parse_cpppp(@input_args);
   my $code= $self->_gen_perl_template_package($parse);
   require Data::Dumper;
   my $ofs= index($code, "\nsub process")+1;
   print substr($code, 0, $ofs);
   print "our \$_parse_data;\n";
   print Data::Dumper->new([ { %$parse, code => '...' } ], [ '$_parse_data' ])
      ->Indent(1)->Sortkeys(1)->Dump;
   print substr($code, $ofs);
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

sub _gen_perl_template_package($self, $parse) {
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
      "sub process(\$self) {",
      # Inject all the lexical functions that need to be in scope
      CodeGen::Cpppp::Template->_generate_template_scope_functions($cpppp_ver),
      qq{# line $src_lineno "$src_filename"},
      $perl,
      "}",
      "1";
}

sub parse_cpppp($self, $in, $filename=undef, $line=undef) {
   if (ref $in eq 'SCALAR') {
      my $tmp= $in;
      # Opening scalarref fails if it has utf8 flag set
      utf8::encode($tmp) if utf8::is_utf8($tmp);
      undef $in;
      open($in, '<', $tmp) or croak "open($tmp): $!";
      defined $in or die "bug";
   }
   elsif (ref $in ne 'GLOB' && !(blessed($in) && $in->can('getline'))) {
      open(my $fh, '<', $in) or croak "open($in): $!";
      $filename //= "$in";
      $in= $fh;
   }
   my $line_ofs= $line? $line - 1 : 0;
   $self->{cpppp_parse}= {
      autocomma => 1,
      autostatementline => 1,
      autoindent => 1,
      filename => $filename,
   };
   my ($perl, $tpl_start_line, $cur_tpl);
   my $end_tpl= sub {
      if (defined $cur_tpl && $cur_tpl =~ /\S/) {
         my $parsed= $self->_parse_code_block($cur_tpl, $filename, $tpl_start_line);
         my $current_indent= $perl =~ /\n([ \t]*).*\n\Z/? $1 : '';
         $current_indent .= '  ' if $perl =~ /\{ *\n\Z/;
         $perl .= $self->_gen_perl_call_code_block($parsed, $current_indent);
      }
      $cur_tpl= undef;
   };
   while (<$in>) {
      if (/^#!/) { # ignore #!
      }
      elsif (/^##/) { # full-line of perl code
         if (defined $cur_tpl || !defined $perl) {
            &$end_tpl;
            $perl .= '# line '.($.+$line_ofs).qq{ "$filename"\n};
         }
         s/^##\s?//;
         my $pl= $_;
         $perl .= $self->_transform_template_perl($pl);
      }
      elsif (/^(.*?) ## ?((?:if|unless|for) .*)/) { # perl conditional suffix, half tpl/half perl
         my ($tpl, $pl)= ($1, $2);
         &$end_tpl if defined $cur_tpl;
         $tpl_start_line= $. + $line_ofs;
         $cur_tpl= $tpl;
         &$end_tpl;
         $perl =~ s/;\s*$//; # remove semicolon
         $pl .= ';' unless $pl =~ /;\s*$/; # re-add it if user didn't
         $perl .= qq{\n# line }.($.+$line_ofs).qq{ "$filename"\n    $pl\n};
      }
      else { # default is to assume a line of template
         if (!defined $cur_tpl) {
            $tpl_start_line= $. + $line_ofs;
            $cur_tpl= '';
         }
         $cur_tpl .= $_;
      }
   }
   &$end_tpl if defined $cur_tpl;
   $self->{cpppp_parse}{code}= $perl;
   delete $self->{cpppp_parse};
}

sub _transform_template_perl($self, $pl) {
   # If user declares "sub NAME(", convert that to "my sub NAME" so that we
   # can grab a ref to it later.
   if ($pl =~ /\b sub \s* (\w+) \s* \(/x) {
      push @{$self->{cpppp_parse}{named_subs}}, $1;
      # look backward and see if it already started with 'my'
      my $pos= rindex($pl, "my", $-[0]);
      if ($pos == -1) {
         substr($pl, $-[0], 0, 'my ');
      }
   }
   # If user declares "##define name(", convert that to both a method and a define
   if ($pl =~ /\b define \s* (\w+) (\s*) \(/x) {
      push @{$self->{cpppp_parse}{named_subs}}, $1;
      substr($pl, $-[2], $+[2]-$-[2], '=> \$self->{define}{'.$1.'}; my sub '.$1);
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

sub _parse_code_block($self, $text, $file, $orig_line) {
   $text .= "\n" unless substr($text,-1) eq "\n";
   if ($text =~ /^# line (\d+) "([^"]+)"/) {
      $orig_line= $1-1;
      $file= $2;
   }
   local our $line= $orig_line || 1;
   local our $start;
   local our @subst;
   local $_= $text;
   # Parse and record the locations of the embedded perl statements
   ()= m{
      (?(DEFINE)
         (?<BALANCED_EXPR> (?>
              \{ (?&BALANCED_EXPR) \}
            | \[ (?&BALANCED_EXPR) \]
            | \( (?&BALANCED_EXPR) \)
            | [^[\](){}\n]+
            | \n (?{ $line++ })
         )* )
      )
      [\$\@] (?{ $start= -1+pos }) 
      (?:
        \{ (?&BALANCED_EXPR) \}           # 
        | [\w_]+                          # plain variable
         (?:                              # maybe followed by ->[] or similar
            (?: -> )?
            (?: \{ (?&BALANCED_EXPR) \} | \[ (?&BALANCED_EXPR) \] )
         ) *                       
      ) (?{ push @subst, { pos => $start, len => -$start+pos, line => $line };
            
          })
      | \n     (?{ $line++ })
   }xg;
   
   for (0..$#subst) {
      my $s= $subst[$_];
      # Special cases
      my $expr= substr($text, $s->{pos}, $s->{len});
      if ($expr eq '$trim_comma') {
         # Modify the text being created to remove the final comma
         $s->{fn}= sub { ${$_[1]} =~ s/,(\s*)$/$1/; '' };
      } elsif ($expr =~ /^ \$\{\{ (.*) \}\} $/x) {
         # Notation ${{ ... }} is a shortcut for @{[do{ ... }]}
         $s->{eval}= $1;
      } else {
         $s->{eval}= $expr; # Will need to be filled in with a coderef
      }
   }
   # Detect columns.  Look for any location where two spaces occur.
   local our %cols;
   local our $linestart= 0;
   $line= $orig_line || 1;
   pos= 0;
   while (m{\G(?>
        \n (?{ ++$line; $linestart= pos })
      | [ ][ ]+ (?{ push @{$cols{-$linestart + pos}}, { pos => pos, len => 0, line => $line  } })
      | .
   )}xcg) {}
   warn "BUG: failed to parse columns" unless pos == length($text);
   # Delete all column markers that occur inside of code substitutions
   for my $s (@subst) {
      for my $col (grep $_ > $s->{pos} && $_ < $s->{pos} + $s->{len}, keys %cols) {
         my $markers= $cols{$col};
         @$markers= grep $_->{pos} > $s->{pos}+$s->{len} || $_->{pos} < $s->{pos},
            @$markers;
      }
   }
   # Detect the actual columns from the remaining markers
   my $colgroup= 0;
   for my $col (sort { $a <=> $b } keys %cols) {
      # Find out which column markers are from adjacent lines
      my $lines= $cols{$col};
      my @adjacent= [ $lines->[0] ];
      for (1..$#$lines) {
         if ($adjacent[-1][-1]{line} + 1 == $lines->[$_]{line}) {
            push @{ $adjacent[-1] }, $lines->[$_];
         } else {
            push @adjacent, [ $lines->[$_] ];
         }
      }
      # Need at least 2 adjacent lines to count as a colum.
      for (grep @$_ > 1, @adjacent) {
         # At least one of the lines must have text to the left of it
         my $has_left= 0;
         for (@$_) {
            my $linestart= rindex($text, "\n", $_->{pos})+1;
            if (substr($text, $linestart, $_->{pos}-$linestart) =~ /\S/) {
               $has_left= 1;
               last;
            }
         }
         next unless $has_left;
         # this is a new linked column group
         ++$colgroup;
         # add one column marker per line in this group
         push @subst, map +{ colgroup => $colgroup, pos => $_->{pos}, len => 0, line => $_->{line} }, @$_;
      }
   }
   # Now merge the column markers into the substitutions in string order
   @subst= sort { $a->{pos} <=> $b->{pos} or $a->{len} <=> $b->{len} } @subst;
   
   { text => $text, subst => \@subst, file => $file }
}

1;

__END__
sub patch_header($self, $fname, $patch_markers=undef) {
   $patch_markers //= "GENERATED ".uc($self->namespace)." HEADERS";
   $self->_patch_file($fname, $patch_markers,
      join '', map { chomp; "$_\n" } $self->public_decl->@*, $self->public_type->@*, $self->public_impl->@*);
}

sub patch_source($self, $fname, $patch_markers=undef) {
   $patch_markers //= "GENERATED ".uc($self->namespace)." IMPLEMENTATION";
   $self->_patch_file($fname, $patch_markers,
      join '', map { chomp; "$_\n" } $self->private_decl->@*, $self->private_type->@*, $self->private_impl->@*);
}

sub patch_xs_boot($self, $fname, $patch_markers=undef) {
   $patch_markers //= "GENERATED ".uc($self->namespace)." XS BOOT";
   $self->_patch_file($fname, $patch_markers,
      join '', map { chomp; "$_\n" } $self->xs_boot->@*);
}

sub _slurp_file($self, $fname) {
   open my $fh, '<', $fname or die "open($fname): $!";
   my $content= do { local $/= undef; <$fh> };
   $fh->close or die "close: $!";
   $content;
}

sub _patch_file($self, $fname, $patch_markers, $new_content) {
   open my $fh, '+<', $fname or die "open($fname): $!";
   my $content= do { local $/= undef; <$fh> };
   $content =~ s{(BEGIN \Q$patch_markers\E[^\n]*\n).*?(^[^\n]+?END \Q$patch_markers\E)}
      {$1$new_content$2}sm
      or croak "Can't find $patch_markers in $fname";
   $fh->seek(0,0) or die "seek: $!";
   $fh->print($content) or die "write: $!";
   $fh->truncate($fh->tell) or die "truncate: $!";
   $fh->close or die "close: $!";
}

1;
