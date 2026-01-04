#!/usr/bin/perl
# Perl Test File for UAST-Grep
# Tests: functions, packages, variables, control flow, error handling

use strict;
use warnings;
use v5.20;
use feature qw(say signatures);
no warnings 'experimental::signatures';

# Constants
use constant MAX_ITEMS => 100;
use constant DEFAULT_NAME => 'UAST-Grep';

# Package-level variables
our $VERSION = '1.0.0';

# Package definition (OOP)
package Person {
    sub new {
        my ($class, %args) = @_;
        my $self = {
            name  => $args{name} // 'Unknown',
            age   => $args{age} // 0,
            email => $args{email},
        };
        return bless $self, $class;
    }

    sub name { $_[0]->{name} }
    sub age { $_[0]->{age} }
    sub email { $_[0]->{email} }

    sub is_adult {
        my ($self) = @_;
        return $self->{age} >= 18;
    }
}

# Main package
package main;

# Scalar variables
my $name = 'UAST-Grep';
my $interpolated = "Testing $name parser";
my $complex = "Length: " . length($name);

# Array variables
my @items = (1, 2, 3, 'hello', 'world');
my @empty = ();

# Hash variables
my %cache = (
    key1 => 'value1',
    key2 => 'value2',
);

# Reference variables
my $array_ref = \@items;
my $hash_ref = \%cache;
my $code_ref = sub { say "Anonymous sub" };

# Here document
my $here_doc = <<'END_DOC';
This is a here document.
It can span multiple lines.
No interpolation in single-quoted delimiter.
END_DOC

# Subroutine definitions
sub log_message {
    my ($level, $message) = @_;
    say "[$level] $message";
}

# Subroutine with signatures (5.20+)
sub calculate_sum($a, $b = 0) {
    return $a + $b;
}

# Transform function
sub transform {
    my ($item) = @_;

    # Pattern matching with given/when (deprecated, using if-elsif)
    if (ref($item) eq 'ARRAY') {
        return [map { $_ * 2 } @$item];
    }
    elsif (!ref($item) && $item =~ /^\d+$/) {
        return $item * 2;
    }
    elsif (!ref($item)) {
        return uc($item);
    }
    else {
        return $item;
    }
}

# Process items
sub process_items {
    my (@input_items) = @_;
    my @results;

    # C-style for loop
    for (my $i = 0; $i < scalar(@input_items); $i++) {
        push @results, transform($input_items[$i]);
    }

    # Foreach loop
    foreach my $item (@input_items) {
        $cache{$item} = $item;
    }

    # While loop
    my $counter = 0;
    while ($counter < 10) {
        $counter++;
    }

    # Until loop
    until ($counter >= 20) {
        $counter++;
    }

    # Do-while
    do {
        $counter++;
    } while ($counter < 25);

    return @results;
}

# Error handling
sub risky_operation {
    my ($filename) = @_;

    # eval block for error handling
    my $content = eval {
        open my $fh, '<', $filename
            or die "Cannot open file: $!";
        local $/;  # Slurp mode
        my $data = <$fh>;
        close $fh;
        return $data;
    };

    if ($@) {
        log_message('ERROR', "Error: $@");
        return undef;
    }

    log_message('INFO', "Read " . length($content) . " characters");
    return $content;
}

# Try::Tiny style (without the module)
sub safe_operation {
    my ($code_block) = @_;
    my $result = eval { $code_block->() };
    return ($@) ? (undef, $@) : ($result, undef);
}

# Get status message
sub get_status_message {
    my ($code) = @_;

    # Hash-based dispatch
    my %messages = (
        200 => 'OK',
        404 => 'Not Found',
        500 => 'Server Error',
    );

    return $messages{$code} // 'Unknown';
}

# Higher-order function (map equivalent)
sub map_items {
    my ($func, @items) = @_;
    return map { $func->($_) } @items;
}

# Higher-order function (filter equivalent)
sub filter_items {
    my ($func, @items) = @_;
    return grep { $func->($_) } @items;
}

# Higher-order function (reduce equivalent)
sub reduce_items {
    my ($func, $initial, @items) = @_;
    my $acc = $initial;
    for my $item (@items) {
        $acc = $func->($acc, $item);
    }
    return $acc;
}

# Closure
sub make_counter {
    my $count = 0;
    return sub {
        return ++$count;
    };
}

# Regular expression operations
sub regex_operations {
    my ($text) = @_;

    # Match
    if ($text =~ /pattern/i) {
        say "Matched!";
    }

    # Capture groups
    if ($text =~ /(\w+)\s+(\w+)/) {
        my ($first, $second) = ($1, $2);
        say "First: $first, Second: $second";
    }

    # Substitution
    my $modified = $text;
    $modified =~ s/old/new/g;

    # Split
    my @words = split /\s+/, $text;

    # Join
    my $joined = join ', ', @words;

    return ($modified, @words);
}

# Hash operations
sub hash_operations {
    my %hash = (a => 1, b => 2, c => 3);

    # Keys and values
    my @keys = keys %hash;
    my @values = values %hash;

    # Each
    while (my ($key, $value) = each %hash) {
        say "$key: $value";
    }

    # Exists and delete
    if (exists $hash{a}) {
        delete $hash{a};
    }

    # Slice
    my @subset = @hash{qw(b c)};

    return %hash;
}

# Array operations
sub array_operations {
    my @arr = (1, 2, 3, 4, 5);

    # Push/pop
    push @arr, 6;
    my $last = pop @arr;

    # Shift/unshift
    unshift @arr, 0;
    my $first = shift @arr;

    # Splice
    my @removed = splice @arr, 1, 2;

    # Sort
    my @sorted = sort { $a <=> $b } @arr;

    # Reverse
    my @reversed = reverse @arr;

    # Slice
    my @slice = @arr[0..2];

    return @sorted;
}

# String operations
sub string_operations {
    my $str = 'Hello, World!';

    # Length
    my $len = length($str);

    # Substring
    my $sub = substr($str, 0, 5);

    # Index
    my $pos = index($str, 'World');

    # Case
    my $upper = uc($str);
    my $lower = lc($str);

    # Trim (no built-in, using regex)
    my $trimmed = $str;
    $trimmed =~ s/^\s+|\s+$//g;

    # Sprintf
    my $formatted = sprintf("Length: %d", $len);

    return ($sub, $upper, $formatted);
}

# Ternary operator
sub check_positive {
    my ($n) = @_;
    return $n > 0 ? 'Positive' : 'Not positive';
}

# Chained comparison (using unless)
sub validate_range {
    my ($n, $min, $max) = @_;
    return 0 unless defined $n;
    return 0 unless $n >= $min;
    return 0 unless $n <= $max;
    return 1;
}

# Main execution
sub main {
    my @data = (1, 2, 3, 'hello', [4, 5]);
    my @results = process_items(@data);

    log_message('INFO', "Processing complete");
    say "Results: ", join(', ', map { ref($_) eq 'ARRAY' ? "[@$_]" : $_ } @results);

    # Test functions
    say "Sum: ", calculate_sum(5, 3);
    say "Status: ", get_status_message(200);

    # Higher-order functions
    my @doubled = map_items(sub { $_[0] * 2 }, 1, 2, 3, 4, 5);
    say "Doubled: ", join(', ', @doubled);

    my @filtered = filter_items(sub { $_[0] > 2 }, 1, 2, 3, 4, 5);
    say "Filtered: ", join(', ', @filtered);

    my $sum = reduce_items(sub { $_[0] + $_[1] }, 0, 1, 2, 3, 4, 5);
    say "Sum: $sum";

    # Closure
    my $counter = make_counter();
    say "Count: ", $counter->(), ", ", $counter->(), ", ", $counter->();

    # Person object
    my $person = Person->new(name => 'Alice', age => 30, email => 'alice@example.com');
    say $person->name, " is adult: ", ($person->is_adult ? 'yes' : 'no');
}

main();

1;  # Return true for module loading
