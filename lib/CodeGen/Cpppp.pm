package CodeGen::Cpppp;
use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'lexical_subs', 'postderef';
use version;
use Cwd 'abs_path';
use Scalar::Util 'blessed', 'looks_like_number';
use CodeGen::Cpppp::Template;
use CodeGen::Cpppp::TemplateBuilder;
use CodeGen::Cpppp::Output;

our $VERSION= 0; # VERSION
# ABSTRACT: The C Perl-Powered Pre-Processor

# These can be inspected by code generators to find out the current
# context the code is being inserted into.  They are localized by
# the template engine.
our $current_indent_prefix= '';
our $current_is_inline= 0;
our @template_stack;
our $indent= '   ';

=head1 RATIONALE

I<It's very special, because, if you can see, the preprocessor, goes up, to
C<perl>.  Look, right across the directory, C<perl>, C<perl>, C<perl>.>

=over

I<And most distributions go up to C<m4> >

=back

I<Exactly>

=over

I<Does that mean it's more powerful?  ...Is it more powerful?>

=back

I<Well, it's one layer of abstraction higher, isn't it?  It's not C<m4>.
You see, most blokes gonna be templating with C<cpp> or C<m4>, you're on C<m4>
here all the way up, all the way up, aaaall the way up, you're at C<m4> for your
pre-processing, Where can you go from there? Where?  Nowhere!  Exactly.>

I<What we do is if we need that extra, push over the cliff, you know what we do?>

=over

I<put it up to C<perl> >

=back

I< C<perl>, exactly. One higher. >

=over

I<Why don't you just download the C<cpp> source, and enhance it with the
abstractions you need?  Make C<cpp> more powerful, and make C<cpp> be the
preprocessor?>

=back

I<...>

I<... These go to B<perl>.>

=head1 SYNOPSIS

  # Cpppp object is a template factory
  my $cpppp= CodeGen::Cpppp->new(%options);
  
  # Simple templates immediately generate their output during
  # construction, which goes to the output accumulator of $cpppp
  # by default.
  $cpppp->new_template($filename, %params);
  
  # Complex templates can define custom methods
  my $tpl= $cpppp->new_template($otherfile, %params);
  $tpl->generate_more_stuff(...);
  
  # Inspect or print the accumulated output
  say $cpppp->output;
  $cpppp->write_sections_to_file(public  => 'project.h');
  $cpppp->write_sections_to_file(private => 'project.c');

B<Input:>

  #! /usr/bin/env cpppp
  ## param $min_bits = 8;
  ## param $max_bits = 16;
  ## param $feature_parent = 0;
  ## param $feature_count = 0;
  ## param @extra_node_fields;
  ##
  ## for (my $bits= $min_bits; $bits <= $max_bits; $bits <<= 1) {
  struct tree_node_$bits {
    uint${bits}_t  left :  ${{$bits-1}},
                   color:  1,
                   right:  ${{$bits-1}},
                   parent,   ## if $feature_parent;
                   count,    ## if $feature_count;
                   $trim_comma $trim_ws;
    @extra_node_fields;
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

=head1 SECURITY

B<Templates are equivalent to perl scripts>.  Use the same caution when
using cpppp templates that you would use when running perl scripts.
Do not load, compile, or render templates from un-trusted authors.

=head1 DESCRIPTION

This module is a preprocessor for C, or maybe more like a perl template engine
that specializes in generating C code.  Each input file gets translated to Perl
in a way that declares a new OO class, and then you can create instances of that
class with various parameters to generate your C output, or call methods on it
like automatically generating headers or function prototypes.

For the end-user, there is a 'cpppp' command line tool that behaves much like
the 'cpp' tool.

B<WARNING: this API is not stable>.  It would be unwise to use C<cpppp>
as part of a distribution's build scripts yet, but it is perfectly safe to use
it to generate sources and then add those generated files to a project.

If you have an interest in this topic, contact me, because I could use help
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

=head2 autoindent

Default value for new templates; determines whether embedded newlines inside
variables that expand in the source code will automatically have indent applied.

=head2 autocolumn

Default value for new templates; enables the feature that detects column layout
in the source template, and attempts to line up those same elements in the
output after variables have been expanded.

=head2 convert_linecomment_to_c89

If true, rewrite the output to convert newer '//' comments into traditional
'/*' comments.

=cut

sub autoindent($self, $newval=undef) {
   $self->{autoindent}= $newval if defined $newval;
   $self->{autoindent} // 1;
}
sub autocolumn($self, $newval=undef) {
   $self->{autocolumn}= $newval if defined $newval;
   $self->{autocolumn} // 1;
}

sub convert_linecomment_to_c89($self, $newval=undef) {
   $self->{convert_linecomment_to_c89}= $newval if defined $newval;
   $self->{convert_linecomment_to_c89} // 0;
}

=head2 include_path

An arrayref of directories to search for template files during
C<require_template>.  Make sure no un-trusted users have control over any
directory in this path, the same as you would do for Perl's C<@INC> paths.

=head2 output

An instance of L<CodeGen::Cpppp::Output> that is used as the default C<output>
parameter for all automatically-created templates, thus collecting all their
output.

=cut

sub include_path { $_[0]{include_path} //= [] }
sub output { $_[0]{output} //= CodeGen::Cpppp::Output->new }

=head1 CONSTRUCTOR

=head2 new

Bare-bones for now, it accepts whatever hash values you hand to it.

=cut

sub new($class, @attrs) {
   my $self= bless {
      @attrs == 1 && ref $attrs[0]? %{$attrs[0]}
      : !(@attrs&1)? @attrs
      : croak "Expected even-length list or hashref"
   }, $class;
   $self->{include_path}= [ $self->{include_path} ]
      if defined $self->{include_path} && ref $self->{include_path} ne 'ARRAY';
   $self;
}

=head1 METHODS

=head2 require_template

  $tpl_class= $cpppp->require_template($filename);

Load a template from a file, and die if not found or if it fails to compile.
Subsequent loads of the same file return the same class.
  
=cut

sub require_template($self, $filename) {
   $self->{templates}{$filename} ||= do {
      my $path= $self->find_template($filename)
         or croak("No template '$filename' found");
      $self->{templates}{$path} ||= $self->compile_cpppp($path);
   }
}

=head2 find_template

  $abs_path= $cpppp->find_template($filename);

Check the filename itself, and relative to all paths in L</include_path>,
and return the absolute path to the first match.

=cut

sub find_template($self, $filename) {
   return abs_path($filename) if $filename =~ m,/, and -e $filename;
   # /foo ./foo and ../foo do not trigger a path search
   return undef if $filename =~ m,^\.?\.?/,;
   for ($self->include_path->@*) {
      my $p= "$_/$filename";
      $p =~ s,//,/,g; # in case include-path ends with '/'
      return abs_path($p) if -e $p;
   }
   return undef;
}

=head2 new_template

  $tpl_instance= $cpppp->new_template($class_or_filename, %params);

Load a template by filename (or use an already-loaded class) and construct a
new instance using C<%params> but also with the context and output defaulting
to this C<$cpppp> instance, and return the template object.

=cut

sub new_template($self, $class_or_filename, @params) {
   $class_or_filename= "$class_or_filename";
   my $class= $class_or_filename->isa('CodeGen::Cpppp::Template')
      ? $class_or_filename
      : $self->require_template($class_or_filename);
   my %params= (
      context => $self,
      output => $self->output,
      !(@params&1)? @params
      : 1 == @params && ref $params[0] eq 'HASH'? %{$params[0]}
      : croak("Expected even-length key/val list, or hashref"),
   );
   $class->new(\%params);
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

sub compile_cpppp {
   my $self= shift;
   my $builder= CodeGen::Cpppp::TemplateBuilder->new(
      autoindent => $self->autoindent,
      autocolumn => $self->autocolumn,
   );
   local $CodeGen::Cpppp::TemplateBuilder::DATA_CALLER_LEVEL=
      $CodeGen::Cpppp::TemplateBuilder::DATA_CALLER_LEVEL+1;
   $builder->compile(@_);
}

=head2 patch_file

  $cpppp->patch_file($filename, $marker, $new_content);

Reads C<$filename>, looking for lines containing C<"BEGIN $marker"> and
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
   $new_content .= "\n" unless $new_content =~ /\n\Z/ or !length $new_content;
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

=head2 backup_and_overwrite_file

  $cpppp->backup_and_overwrite_file($filename, $new_content);

Create a backup of $filename if it already exists, and then write a new file
containing C<$new_content>.  The backup is created by appending a ".N" to the
filename, choosing the first available "N" counting upward from 0.

=cut

sub backup_and_overwrite_file($self, $fname, $new_content) {
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

=head2 get_filtered_output

  my $text= $cpppp->get_filtered_output(@sections);

Like C<< $cpppp->output->get >>, but also apply filters to the output, like
L</convert_linecomment_to_c89>.

=cut

sub get_filtered_output($self, @sections) {
   @sections= grep defined, @sections; # allow a single undef to mean 'all'
   my $content= $self->output->get(@sections);
   if ($self->convert_linecomment_to_c89) {
      # rewrite '//' comments as '/*' comments
      require CodeGen::Cpppp::CParser;
      my @tokens= CodeGen::Cpppp::CParser->tokenize($content);
      my $ofs= 0;
      for (@tokens) {
         $_->[2] += $ofs;
         if ($_->type eq 'comment') {
            if (substr($content, $_->src_pos, 2) eq '//') {
               substr($content, $_->src_pos, $_->src_len, '/*'.$_->value.' */');
               $ofs += 3;
            }
         }
      }
   }
   $content;
}

=head2 write_sections_to_file

  $cpppp->write_sections_to_file($section_spec, $filename);
  $cpppp->write_sections_to_file($section_spec, $filename, $patch_markers);

This is a simple wrapper around L<CodeGen::Cpppp::Output/get> and either
L</backup_and_overwrite_file> or L</patch_file>, depending on whether you
supply C<$patch_markers>.

=cut

sub write_sections_to_file($self, $sections, $fname, $patch_markers=undef) {
   my $content= $self->get_filtered_output($sections);
   if (defined $patch_markers) {
      $self->patch_file($fname, $patch_markers, $content);
   } else {
      $self->backup_and_overwrite_file($fname, $content);
   }
   $self
}

sub _slurp_file($self, $fname) {
   open my $fh, '<', $fname or die "open($fname): $!";
   my $content= do { local $/= undef; <$fh> };
   $fh->close or die "close: $!";
   $content;
}

1;
