package CodeGen::Cpppp;
use strict;
use warnings;
use Carp;

# VERSION
# ABSTRACT: The C Perl-Powered Pre-Processor

=head1 SYNOPSIS

I<Does that mean it's more powerful?  ...Is it more powerful?>

I<Well, it's one layer of abstraction higher, isn't it?  It's not C<m4>.  You see, most
blokes gonna be templating with C<cpp> or C<m4>, you're on C<m4> here all the way up,
all the way up, Where can you go from there? Where?  Nowhere!  Exactly.>

I<What we do is if we need that extra, push, over the cliff, you know what we do?>

I<C<perl>, exactly. These go to C<perl>.>

  #! /usr/bin/env cpppp
  ## for (my $bits= 8; $bits <= 32; $bits <<= 1) {
  struct tree_node_$bits {
    uint${bits}_t left:  ${{$bits-1}},
                  color: 1,
                  right: ${{$bits-1}};
  }
  ## }

Output:

  struct tree_node_8 {
    uint8_t left:  7,
            right: 7,
            color: 1;
  }
  struct tree_node_16 {
    uint16_t left:  15,
             right: 15,
             color: 1;
  }
  struct tree_node_32 {
    uint32_t left:  31,
             right: 31,
             color: 1;
  }

=head1 DESCRIPTION

B<WARNING: this API is complete and totally unstable>.

If you have an interest in this, contact me, because I could use help brainstorming ideas
about how to accommodate the most possibilities, here.

=head1 CONSTRUCTOR

Bare-bones for now, it accepts whatever hash values you hand to it.

=cut

sub new {
   my ($class, %attrs)= @_;
   $attrs{code_block_templates}= [];
   bless \%attrs, $class;
}

=head1 METHODS

=head2 compile_template

  $cpppp->compile_template($input_fh, $filename);
  $cpppp->compile_template(\$scalar_tpl, $filename, $line_offset);

This reads the input file handle (or scalar-ref) and builds a perl subroutine out of it, then
evals that subroutine so it is ready to run (and spits out any compile errors in the template).

=cut

sub compile_template {
   my ($self, $in, $filename, $line)= @_;
   my $perl= $self->_translate_cpppp($in, $filename, $line);
   $perl= "package CodeGen::Cpppp::Tmp::0;\n"
      ."use strict; use warnings;\n"
      ."sub { my \$self= shift;\n"
      ."$perl;\n"
      ."}\n";
   unless ($self->{fn}= eval $perl) {
      print $perl;
      die $@;
   }
   return $self;
}

=head1 METHODS

=head2 render

  $cpppp->render();

Execute the template previously compiled.  Passing arguments to this template is a TODO item.

=cut

sub render {
   my $self= shift;
   $self->{out}= '';
   $self->{fn}->($self);
   return $self->{out};
}

sub _translate_cpppp {
   my ($self, $in, $filename, $line)= @_;
   my $line_ofs= $line? $line - 1 : 0;
   if (ref $in eq 'SCALAR') {
      my $tmp= $in;
      utf8::encode($tmp) if utf8::is_utf8($tmp);
      undef $in;
      open($in, '<', $tmp) or die;
      defined $in or die;
   }
   my ($perl, $tpl_start_line, $cur_tpl);
   my $end_tpl= sub {
      my $parsed= $self->_parse_code_block($cur_tpl, $filename, $tpl_start_line);
      $perl .= '$self->_render_code_block('.$self->_generate_code_block_perl($parsed).");\n";
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
         if (!defined $perl) {
            $perl= '# line '.($.+$line_ofs).qq{ "$filename"\n};
         }
         s/^##\s*//;
         my $pl= $_;
         if ($pl =~ /sub \w+ \s* \( ( [^,)\n]* )/x) {
            if ($1 ne '$self') {
               substr($pl, $-[1], $+[1]-$-[1], '$self'.(length $1? ', '.$1 : ''));
            }
         }
         $perl .= $pl;
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
   $perl;
}

sub _generate_code_block_perl {
   my ($self, $parsed)= @_;
   my $n= @{$self->{code_block_templates}};
   push @{$self->{code_block_templates}}, $parsed;
   my $code= 'do { my @expr_subs;'."\n";
   for (0 .. $#{$parsed->{subst}}) {
      my $s= $parsed->{subst}[$_];
      if ($s->{len}) {
         my $expr= substr($parsed->{text}, $s->{pos}, $s->{len});
         if ($expr eq '$anticomma') {
            # Special case: trim out the previous comma, skipping over whitespace
            $s->{fn}= sub { ${$_[1]} =~ s/,(\s*)/$1/; '' };
         }
         else {
            # Notation ${{ ... }} is an extension that means "run this literal perl"
            if ($expr =~ /\$\{\{(.*)\}\}$/) {
               $expr= $1;
            }
            $code .= join "\n",
               '    $expr_subs['.$_.']= sub { my $self= shift;',
               '# line '.$s->{line}.' "'.$parsed->{file}.'"',
               '      '.$expr,
               "    };\n";
         }
      }
   }
   return $code.'($self->{code_block_templates}['.$n."], \\\@expr_subs)\n}";
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

sub _render_code_block {
   my ($self, $block, $expr_subs)= @_;
   my $text= $block->{text};
   my $newtext= '';
   my $at= 0;
   my %colpos;
   # First pass, perform substitutions and record new column markers
   for my $i (0..$#{$block->{subst}}) {
      my $s= $block->{subst}[$i];
      if ($s->{colgroup}) {
         push @{$colpos{$s->{colgroup}}}, length($newtext) + $s->{pos} - $at;
      }
      else {
         $newtext .= substr($text, $at, $s->{pos} - $at);
         my $fn= $expr_subs->[$i];
         if ($fn) {
            $newtext .= $fn->($self, \$newtext);
         }
         $at= $s->{pos} + $s->{len};
      }
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
            print "subtract $diff characters at $linestart+$newcol\n";
            substr($text, $linestart + $newcol, -$diff, '');
         } elsif($diff > 0) {
            print "insert $diff spaces at $linestart+$oldcol\n";
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
   $self->{out} .= $text;
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
