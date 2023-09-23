package CodeGen::Cpppp::Template;

# VERSION
# ABSTRACT: Template objects created by parsing and compiling cpppp

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'postderef';
use Scalar::Util 'looks_like_number';
use Hash::Util;
use CodeGen::Cpppp::Output;
use Exporter ();
require version;

use constant {
   PUBLIC     => 'public',
   PROTECTED  => 'protected',
   PRIVATE    => 'private',
};
our @EXPORT_OK= qw( PUBLIC PROTECTED PRIVATE );
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
   # Create accessors for all of the attributes declared in the template.
   for (keys $parse_data->{template_parameter}->%*) {
      my $name= $_;
      *{"${pkg}::$name"}= sub { $_[0]{$name} };
   }
   # Expose all of the functions declared in the template
   for (keys $parse_data->{template_method}->%*) {
      my $name= $_;
      *{"${pkg}::$name"}= sub {
         my $m= shift->{template_method}{$name}
            or croak "Template execution did not define method '$name'";
         goto $m;
      };
   }
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

sub _gen_perl_scope_functions($class, $version) {
   return (
      '# line '. __LINE__ . ' "' . __FILE__ . '"',
      'my sub param { $self->_init_param(@_) }',
      'my sub define($name, $replacement){ $self->define_template_macro($name, $replacement) }',
      'my sub section($name){ $self->current_output_section($name) }',
      'my sub template($name){ $self->require_template($name) }',
   );
}

sub _parse_data($class) {
   no strict 'refs';
   $class = ref $class if ref $class;
   return ${"${class}::_parse_data"};
}

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
         croak("Unknown parameter '$_' to template $parse->{filename}");
      }
   }

   my $self= bless {
      (map +($_ => $parse->{$_}), qw( autocomma autoindent autostatementline )),
      output => CodeGen::Cpppp::Output->new,
      current_output_section => 'private',
      %attrs,
   }, $class;
   $self->BUILD(\%attrs);
   $self;
}

sub current_output_section($self, $new=undef) {
   if (defined $new) {
      $self->output->has_section($new)
         or croak "No defined output section '$new'";
      $self->{current_output_section}= $new;
   }
   $self->{current_output_section};
}

sub output($self) {
   $self->{output};
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

sub define_template_macro($self, $name, $code) {
   $self->{template_macro}{$name}= $code;
}

sub define_template_method($self, $name, $code) {
   $self->{template_method}{$name}= $code;
}

sub _render_code_block {
   my ($self, $i, @expr_subs)= @_;
   my $block= $self->_parse_data->{code_block_templates}[$i];
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
         @out= grep defined, @out;
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
         my $str= join $join_sep, @out;
         # Autoindent: if new text contains newline, add current indent to start of each line.
         if ($self->{autoindent} && $indent) {
            $str =~ s/\n/\n$indent/g;
         }
         $newtext .= $str;
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
   $self->{output}->append($self->{current_output_section} => $text);
}

1;
