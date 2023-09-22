package CodeGen::Cpppp::Template;

# VERSION
# ABSTRACT: Template objects created by parsing and compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'postderef';
use Scalar::Util 'looks_like_number';
use CodeGen::Cpppp::Output;
use Exporter ();
require version;

use constant {
   public    => 'PUBLIC',
   protected => 'PROTECTED',
   private   => 'PRIVATE',
   impl      => 'IMPL',
};
our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE IMPL );
our %EXPORT_TAGS= (
   'v0' => \@EXPORT_OK,
);
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
   goto \&Exporter::import;
}

our $_next_pkg= 1;
sub _create_derived_package($class, $version, $parse_data) {
   my $pkg= 'CodeGen::Cpppp::Template::_'.$_next_pkg++;
   no strict 'refs';
   @{"${pkg}::ISA"}= ( $class );
   ${"${pkg}::_parse_data"}= $parse_data;
   $pkg;
}

sub _setup_derived_package($class, $pkg, $version) {
   strict->import;
   warnings->import;
   utf8->import;
   experimental->import(qw( lexical_subs signatures postderef ));

   no strict 'refs';
   @{"${pkg}::ISA"}= ( $class ) unless @{"${pkg}::ISA"};
}

sub _generate_template_scope_functions($class, $version) {
   return (
      '# line '. __LINE__ . ' "' . __FILE__ . '"',
      'my sub section($name){ $self->set_current_section($name) }',
      'my sub template($name){ $self->require_template($name) }',
   );
}

sub _parse_data($class) {
   no strict 'refs';
   return ${"${class}::_parse_data"};
}

sub new($class, @attrs) {
   no strict 'refs';
   bless {
      %{${$class.'::_parse_data'}},
      output => CodeGen::Cpppp::Output->new,
      @attrs == 1 && ref $attrs[0]? %{ $attrs[0] }
      : !(@attrs&1)? @attrs
      : croak "Expected even-length list or hashref"
   }, $class;
}

sub output($self) {
   $self->{output};
}

sub render {
   my $self= shift;
   $self->{process_result} //= $self->process;
   return $self->output;
}

sub _render_code_block {
   my ($self, $i, @expr_subs)= @_;
   my $block= $self->{code_block_templates}[$i];
   my $text= $block->{text};
   my $newtext= '';
   my $at= 0;
   my %colmarker;
   my $prev_colmark;
   # First pass, perform substitutions and record new column markers
   my sub str_esc{ join '', map +(ord($_) > 0x7e || ord($_) < 0x21? sprintf("\\x{%X}",ord) : $_), split //, $_[0] }
   for my $s (@{$block->{subst}}) {
      $newtext .= substr($text, $at, $s->{pos} - $at);
      if ($s->{colgroup}) {
         my $mark= $colmarker{$s->{colgroup}} //= join '', "\x{200A}", map chr(0x2000+$_), split //, $s->{colgroup};
         $newtext .= $mark;
         $prev_colmark= $s;
      }
      elsif (defined $s->{fn}) {
         $newtext .= $s->{fn}->($self, \$newtext);
      }
      elsif (defined $s->{eval_idx}) {
         my $fn= $expr_subs[$s->{eval_idx}]
            or die;
         # Avoid using $_ up to this point so that $_ pases through
         # from the surrounding code into the evals
         my @out= $fn->($self, \$newtext);
         # Expand arrayref and coderefs in the returned list
         @out= @{$out[0]} if @out == 1 && ref $out[0] eq 'ARRAY';
         ref eq 'CODE' && ($_= $_->($self, \$newtext)) for @out;
         # Now decide what to join them with.
         my $join_sep= $";
         my $indent= '';
         my ($last_char)= ($newtext =~ /(\S) (\s*) \Z/x);
         my $cur_line= substr($newtext, rindex($newtext, "\n")+1);
         my $inline= $cur_line =~ /\S/;
         if ($self->{autoindent}) {
            ($indent= $cur_line) =~ s/\S/ /g;
         }
         # Special handling if the user requested a list substitution
         if (ord $s->{eval} == ord '@') {
            $last_char= '' unless defined $last_char;
            if ($self->{autocomma} && ($last_char eq ',' || $last_char eq '(')) {
               if (@out) {
                  $join_sep= $inline? ', ' : ",\n";
                  @out= grep /\S/, @out; # remove items that are only whitespace
               }
               # If no items, or the first nonwhitespace character is a comma,
               # remove the previous comma
               if (!@out || $out[0] =~ /^\s*,/) {
                  $newtext =~ s/,(\s*)\Z/$1/;
               }
            } elsif ($self->{autostatementline} && ($last_char eq '{' || $last_char eq ';')) {
               @out= grep /\S/, @out; # remove items that are only whitespace
               $join_sep= $inline? "; " : ";\n";
            } elsif ($self->{autoindent} && !$inline && $join_sep !~ /\n/) {
               $join_sep .= "\n";
            }
         }
         my $out= join $join_sep, @out;
         # Autoindent: if new text contains newline, add current indent to start of each line.
         if ($self->{autoindent} && $indent) {
            $out =~ s/\n/\n$indent/g;
         }
         $newtext .= $out;
      }
      $at= $s->{pos} + $s->{len};
   }
   $text= $newtext . substr($text, $at);
   # Second pass, adjust whitespace of all column markers so they line up.
   # Iterate from leftmost column rightward.
   autoindent: for my $group_i (sort { $a <=> $b } keys %colmarker) {
      my $token= $colmarker{$group_i};
      # Find the longest prefix (excluding trailing whitespace)
      my $maxcol= 0;
      my ($linestart, $col);
      while ($text =~ /[ ]*$token/mg) {
         $linestart= rindex($text, "\n", $-[0])+1;
         $col= $-[0] - $linestart;
         $maxcol= $col if $col > $maxcol;
      }
      $text =~ s/[ ]*$token/
         $linestart= rindex($text, "\n", $-[0])+1;
         " "x(1 + $maxcol - ($-[0] - $linestart))
         /ge;
   }
   $self->{output}->append(impl => $text);
}

1;
