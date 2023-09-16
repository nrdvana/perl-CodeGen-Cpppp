package CodeGen::Cpppp;
use v5.20;
use warnings;
use Carp;

# VERSION
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
  ## for (my $bits= 8; $bits <= 32; $bits <<= 1) {
  struct tree_node_$bits {
    uint${bits}_t  left:  ${{$bits-1}},
                   color: 1,
                   right: ${{$bits-1}};
  };
  ## }

B<Output:>

  struct tree_node_8 {
    uint8_t left:  7,
            right: 7,
            color: 1;
  };
  struct tree_node_16 {
    uint16_t left:  15,
             right: 15,
             color: 1;
  };
  struct tree_node_32 {
    uint32_t left:  31,
             right: 31,
             color: 1;
  };

=head1 DESCRIPTION

B<WARNING: this API is completely and totally unstable>.

This module is a preprocessor for C, 

If you have an interest in this, contact me, because I could use help brainstorming ideas
about how to accommodate the most possibilities, here.

Possibilities:

=over

=item *

Scan existing headers to discover available macros, structs, and functions on the host.

=item *

Pass a list of headers through the real cpp and analyze the macro output.

=item *

Shell out to a compiler to find 'sizeof' information for structs.

=back

=head1 CONSTRUCTOR

Bare-bones for now, it accepts whatever hash values you hand to it.

=cut

sub new {
   my ($class, %attrs)= @_;
   bless \%attrs, $class;
}

=head1 METHODS

=head2 compile_template

  $cpppp->compile_template($input_fh, $filename);
  $cpppp->compile_template(\$scalar_tpl, $filename, $line_offset);

This reads the input file handle (or scalar-ref) and builds a new perl template
class out of it (and dies if there are syntax errors in the template).

=cut

our $next_pkg= 1;
sub compile_template {
   my ($self, $in, $filename, $line)= @_;
   my $parse= $self->_parse_cpppp($in, $filename, $line);
   my $pkg= 'CodeGen::Cpppp::_Template'.$next_pkg++;
   my $perl= "package $pkg;\n"
      ."use v5.20;\n"
      ."use warnings;\n"
      ."no warnings 'experimental::lexical_subs', 'experimental::signatures';\n"
      ."use feature 'lexical_subs', 'signatures';\n"
      ."use CodeGen::Cpppp::Template -setup;\n"
      ."sub process(\$self) {\n"
      ."$parse->{code};\n"
      ."}\n"
      ."1\n";
   unless (eval $perl) {
      my $err= "$@";
      STDERR->print($perl);
      die $err;
   }
   $pkg->_set_parse_data($parse);
   return $pkg;
}

sub _parse_cpppp {
   my ($self, $in, $filename, $line)= @_;
   my $line_ofs= $line? $line - 1 : 0;
   if (ref $in eq 'SCALAR') {
      my $tmp= $in;
      utf8::encode($tmp) if utf8::is_utf8($tmp);
      undef $in;
      open($in, '<', $tmp) or die;
      defined $in or die;
   }
   $self->{cpppp_parse}= {};
   my ($perl, $tpl_start_line, $cur_tpl);
   my $end_tpl= sub {
      if ($cur_tpl =~ /\S/) {
         my $parsed= $self->_parse_code_block($cur_tpl, $filename, $tpl_start_line);
         $perl .= $self->_gen_perl_call_code_block($parsed);
      }
      $cur_tpl= undef;
   };
   while (<$in>) {
      if (/^#!/) { # ignore #!
      }
      elsif (/^##(?!#)/) { # full-line of perl code
         if (defined $cur_tpl) {
            &$end_tpl;
            $perl .= '# line '.($.+$line_ofs).qq{ "$filename"\n};
         }
         elsif (!defined $perl) {
            $perl= '# line '.($.+$line_ofs).qq{ "$filename"\n};
         }
         s/^##\s?//;
         my $pl= $_;
         $perl .= $self->_process_template_perl($pl);
      }
      elsif (/^(.*?) ## ?((?:if|unless) .*)/) { # perl conditional suffix, half tpl/half perl
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

sub _process_template_perl {
   my ($self, $pl)= @_;
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

sub _gen_perl_call_code_block {
   my ($self, $parsed)= @_;
   my $codeblocks= $self->{cpppp_parse}{code_block_templates} ||= [];
   push @$codeblocks, $parsed;
   my $code= '$self->_render_code_block('.$#$codeblocks;
   my $i= 0;
   for my $s (@{$parsed->{subst}}) {
      if (defined $s->{eval}) {
         $s->{eval_idx}= $i++;
         my $sig= $s->{eval} =~ /self|output/? '($self, $output)' : '';
         $code .= qq{,\n# line $s->{line} "$parsed->{file}"\n  sub${sig}{ $s->{eval} }};
      }
   }
   $code .");\n";
}

sub _parse_code_block {
   my ($self, $text, $file, $orig_line)= @_;
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
   @subst= sort { $a->{pos} <=> $b->{pos} } @subst;
   
   { text => $text, subst => \@subst, file => $file }
}

package CodeGen::Cpppp::Template;
$INC{'CodeGen/Cpppp/Template.pm'}= 1;
use v5.20;
use warnings;
use Carp;

sub import {
   my $class= shift;
   my $caller= caller;
   for (@_) {
      if ($_ eq '-setup') {
         no strict 'refs';
         push @{$caller.'::ISA'}, $class;
      } else { croak "$class does not export $_" }
   }
}

sub _set_parse_data {
   my ($class, $parse)= @_;
   no strict 'refs';
   ${$class.'::_parse_data'}= $parse;
}

sub new {
   my $class= shift;
   no strict 'refs';
   bless {
      %{${$class.'::_parse_data'}},
      out => {
         public => '',
         protected => '',
         private => '',
         decl => '',
         impl => '',
      }
   }, $class;
}

sub render {
   my $self= shift;
   $self->{process_result} //= $self->process;
   return $self->{out}{impl};
}

sub _render_code_block {
   my ($self, $i, @expr_subs)= @_;
   my $block= $self->{code_block_templates}[$i];
   my $text= $block->{text};
   my $newtext= '';
   my $at= 0;
   my %colpos;
   # First pass, perform substitutions and record new column markers
   for my $s (@{$block->{subst}}) {
      $newtext .= substr($text, $at, $s->{pos} - $at);
      if ($s->{colgroup}) {
         push @{$colpos{$s->{colgroup}}}, length($newtext);
      }
      elsif (defined $s->{fn}) {
         $newtext .= $s->{fn}->($self, \$newtext);
      }
      elsif (defined $s->{eval_idx}) {
         my $fn= $expr_subs[$s->{eval_idx}]
            or die;
         # Avoid using $_ so that $_ pases through from the surrounding code into the evals
         my @out= $fn->($self, \$newtext);
         for (my $i= 0; $i < @out; $i++) {
            if (ref $out[$i]) {
               if (ref $out[$i] eq 'ARRAY') { splice(@out, $i, 1, @{$out[$i]}) }
               elsif (ref $out[$i] eq 'CODE') { $out[$i]= $out[$i]->($self, \$newtext) }
            }
         }
         my $out= join $", @out;
         if (index($out, "\n") >= 0) {
            my $col= length($newtext) - (rindex($newtext, "\n")+1);
            my $indent= ' 'x$col;
            $out =~ s/^/$indent/mg;
         }
         $newtext .= $out;
      }
      $at= $s->{pos} + $s->{len};
   }
   $text= $newtext . substr($text, $at);
   # Second pass, adjust whitespace of all column markers so they line up.
   # Iterate from leftmost column rightward.
   for my $group_i (sort { $a <=> $b } keys %colpos) {
      my $group= $colpos{$group_i};
      # Find the longest prefix (excluding trailing whitespace)
      my $newcol= 0;
      for (@$group) {
         my $linestart= rindex($text, "\n", $_)+1;
         substr($text, $linestart, $_-$linestart) =~ /(.*? ) *$/;
         my $l= length($1);
         $newcol= $l if $l > $newcol;
      }
      # Now update them all to that common length, but after each update
      # need to update all other positions by the amount changed because the
      # source string is changing.
      @$group= sort { $a <=> $b } @$group;
      for (my $i= 0; $i < @$group; $i++) {
         my $linestart= rindex($text, "\n", $group->[$i])+1;
         my $oldcol= $group->[$i] - $linestart;
         my $diff= $newcol - $oldcol;
         if ($diff < 0) {
            substr($text, $linestart + $newcol, -$diff, '');
         } elsif($diff > 0) {
            substr($text, $linestart + $oldcol, 0, ' 'x$diff);
         }
         # update all positions beyond this one
         for (values %colpos) {
            for (@$_) {
               $_ += $diff if $_ > $group->[$i];
            }
         }
      }
   }
   $self->{out}{impl} .= $text;
}

package CodeGen::Cpppp::Template::Imports;
use Exporter;
our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE );
our %EXPORT_TAGS= ( all => \@EXPORT_OK );

sub PUBLIC {}
sub PROTECTED {}
sub PRIVATE {}

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
