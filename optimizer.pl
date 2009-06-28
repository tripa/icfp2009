#! /usr/bin/env perl

use 5.010;
use strict;
use warnings;

use Scalar::Util qw(looks_like_number);
use File::Basename 'basename';
use Template;

my $asm_file = shift or die "Source file needed\n";
open my $asm, '<', $asm_file or die $!;

my @instructions;
my @data;
my (%read, %write);
my (%input, @output, %state);
my (%flag, %read_flag);
my $status;

while ( <$asm> ) {
  my ($addr, $instr, $data) = /^\s*(\d+)\s+(\w+(?:\s+\w+)*)\s+\[(.*)\]$/ or die $_;
  my @instr = split /\s+/, $instr;
  $instructions[$addr] = \@instr;
  $data[$addr] = $data;

  my @vars = grep { looks_like_number $_ } @instr;
  $input{$vars[0]}++ if $instr[0] eq 'Input';
  shift @vars if $instr[0] =~ /put$/;
  $read{$_} = 1 for @vars;
  $_ >= $addr and $state{$_}++ for @vars;

  given ($instr[0]) {
    when ('Output') { push @output, @vars }
    when ('Cmpz') { $status = $addr; $flag{$addr} = 1; }
    when ('Noop') {}
    default { $write{$addr} = 1 }
  }

  if ($instr[0] eq 'Phi') {
    die "Phi with undefined status at $addr\n" unless $status;
    $read_flag{$addr} = $status;
  }

}

# read not written: constants
my %constant;
$write{$_} or $constant{$_} = 1 for keys %read;

# btw: remove constants from state
delete @state{keys %constant};

# written not read: useless
$read{$_} or warn "Unused memory at $_\n" for keys %write;

my (%assign, %output);

sub rvar {
  my ($addr, $num) = @_;
  return $data[$num] if $constant{$num};
  return $num >= $addr ? "o$num" : "n$num" if $state{$num};
  die "f$num" if $flag{$num};
  return "i$num";
}

sub wvar {
  my ($addr) = @_;
  die "can't write a constant at $addr" if $constant{$addr};
  return "n$addr" if $state{$addr};
  return "f$addr" if $flag{$addr};
  return "i$addr";
}

sub unary {
  my ($addr, $r) = @_;
  $assign{wvar($addr)} = $r;
}

sub binary {
  my ($addr, $f, $o1, $o2) = @_;
  unary $addr, $f->(rvar($addr, $o1), rvar($addr, $o2));
}

sub simpleBinary {
  my ($addr, $op, $o1, $o2) = @_;
  binary $addr, sub { "$_[0] $op $_[1]" }, $o1, $o2;
}

sub div { "if $_[1]==0.0 then 0.0 else $_[0]/$_[1]" }

for my $addr (0..$#instructions) {
  my ($op, $o1, $o2) = @{ $instructions[$addr] };
  given ($op) {
    when ('Add') { simpleBinary $addr, '+', $o1, $o2 }
    when ('Sub') { simpleBinary $addr, '-', $o1, $o2 }
    when ('Mult') { simpleBinary $addr, '*', $o1, $o2 }
    when ('Div') { binary $addr, \&div, $o1, $o2 }
    when ('Output') { $output{$o1} = rvar($addr, $o2) }
    when ('Phi') { unary $addr, "if f$read_flag{$addr} then " . rvar($addr, $o1)
                     . " else " . rvar($addr, $o2) }
    when ('Noop') {}
    when ('Cmpz') {
      my %cmp = (
        LTZ => '<',
        LEZ => '<=',
        EQZ => '==',
        GEZ => '>=',
        GTZ => '>',
       );
      unary $addr, rvar($addr, $o2) . $cmp{$o1} . "0.0"
    }
    when ('Sqrt') { unary $addr, 'sqrt ' . rvar($addr, $o1) }
    when ('Copy') { unary $addr, rvar($addr, $o1) }
    when ('Input') { unary $addr, "input$o1" }
    default { die "unknown opcode $_" }
  }
}

# while (my ($addr, $val) = each %assign) { say "$addr = $val" }
# while (my ($addr, $val) = each %output) { say "output $addr $val" }

my $state = 0;
$state{$_} = $state++ foreach keys %state;

my $hs = ucfirst(basename $asm_file, '.asm');
say "Generating $hs.hs";
my $tt = Template->new();
$tt->process('optimizer.tt', {
  module => $hs,
  data => \@data,
  state => \%state,
  input => \%input,
  assign => \%assign,
  output => \%output
 }, "$hs.hs")
  or die $tt->error();
