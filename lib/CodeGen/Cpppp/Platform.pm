package CodeGen::Cpppp::Platform;

# VERSION
# ABSTRACT: Utility functions for abstracting the host OS

use v5.20;
use warnings;
use Carp;
use experimental 'signatures', 'lexical_subs', 'postderef';
use Exporter 'import';

our @EXPORT_OK= qw( format_commandline );

=head1 EXPORTS

=head2 format_commandline

Return a shell command representation of the current running perl script.
This is meant to be used by templates to show how to re-generate the output.

=cut

# could use String::ShellQuote, but aiming for fewer deps
# TODO: move this to a platform-specific utility module
sub _unix_shellquote {
   return "''" unless length $_[0];
   return $_[0] unless $_[0] =~ m|[^-\w!%+,./:@^]|;
   (my $x= $_[0]) =~ s/'/'\\''/g;
   $x= "'$x'";
   $x =~ s/^''//;
   $x =~ s/''$//;
   $x;
}

# This utility function returns a string which could be passed to the
# shell to re-run the current command.
sub _unix_format_commandline {
   # Make a map of which options have arguments
   my %have_arg;
   # This is defined by bin/cpppp.  Use it, if available, else we don't know
   # which options have arguments and everything goes to its own line.
   for (keys %main::option_spec) {
      /^([^=]+)=/ or next;
      $have_arg{$_}= 1 for split '|', $1;
   }
   my @lines;
   for (@main::original_argv? @main::original_argv : @ARGV) {
      my $escaped= _unix_shellquote($_);
      if (@lines && $lines[-1] =~ /^-+(.*)/ && $have_arg{$1}) {
         $lines[-1] .= ' ' . $escaped;
      } else {
         push @lines, $escaped;
      }
   }
   return $0 . ' ' . join(" \\\n    ", @lines);
}

sub _win32_format_commandline {
   require Win32::ShellQuote;
   # Not sure if Win32 can wrap a command, so just drop the whole thing on one line.
   my @argv= ( $0, @main::original_argv? @main::original_argv : @ARGV );
   return Win32::ShellQuote::quote_native(@argv);
}

if ($^O eq 'Win32') {
   *format_commandline= \&_win32_format_commandline;
} else {
   *format_commandline= \&_unix_format_commandline;
}

1;
