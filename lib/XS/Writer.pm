package XS::Writer;

use strict;
use warnings;

our $VERSION = 0.01;

use File::Basename;
use File::Path;
use Carp;
use Moose;
use Moose::Autobox;

{
    package StringWithWhitespace;
    use Moose::Role;

    sub strip_ws {
        $_[0] =~ s/^\s+//;
        $_[0] =~ s/\s+$//;
        $_[0];
    }

    sub squeeze_ws {
        $_[0] =~ s/\s+/ /g;
        $_[0];
    }
}
Moose::Autobox->mixin_additional_role("SCALAR", "StringWithWhitespace");


=head1 NAME

XS::Writer - Module to write some XS for you

=head1 SYNOPSIS

    # As part of your build process...
    use XS::Writer;

    my $writer = XS::Writer->new(
        package   => 'Some::Employee'
    );

    $writer->struct(<<'END');
        typedef struct employee {
            char *      name;
            double      salary;
            int         id;
        };
    END

    # This will generate lib/Some/Employee_struct.xs
    $writer->write_file;


    # Then in lib/Some/Employee.xs
    INCLUDE: Employee_struct.xs

    ...any other XS you like...


    # You must add this to lib/Some/typemap
    TYPEMAP
    Some::Employee          T_PTROBJ


    # And finally in lib/Some/Employee.pm
    package Some::Employee;

    our $VERSION = 1.23;

    use XSLoader;
    XSLoader::load __PACKAGE__, $VERSION;


    # And you will be able to do
    use Some::Employee;

    my $employee = Some::Employee->new;
    $employee->name("Yarrow Hock");


=head1 DESCRIPTION

I went nuts trying to figure out how to map structs into perl.  I finally
figured it out and I never want anyone else to have to go through that.
I also wanted the process to remain transparent, many of the XS writing
modules are themselves almost as complicated as XS itself.

This module helps you write XS by taking care of some of the rote things
for you.  Right now it just makes structs available as objects, writing a
constructor, destructor and accessors.

The instructions are meant for Module::Build.  Adapt as necessary for
MakeMaker.

=head1 Methods

=head3 new

    my $writer = XS::Writer->new( %args );

Setup a new writer.  Arguments are...

    package         (required) The package to write your XS into.
    xs_file         (optional) Where to write the XS file.  Defaults to
                    lib/Your/Package_struct.xs
    include         (optional) Any extra code to include

=cut

has 'package',
    is          => 'rw',
    required    => 1
;
has 'xs_type',
    is          => 'rw',
    lazy        => 1,
    default     => sub {
        my $self = shift;
        my $type = $self->package;
        $type =~ s/::/__/;
        return $type;
    }
;
has 'xs_prefix',
    is          => 'rw',
    lazy        => 1,
    default     => sub {
        my $self = shift;
        return $self->xs_type . "_";
    }
;
has 'xs_file',
    is          => 'rw',
    lazy        => 1,
    default     => sub {
        my $self = shift;
        my $file = $self->package;
        $file =~ s{::}{/};
        return "lib/${file}_struct.xs";
    }
;
has 'include',
    is          => 'rw',
    default     => '',
;
has 'struct_type',
    is          => 'rw'
;
has 'struct_elements' =>
    is          => 'rw',
    isa         => 'HashRef'
;
has 'struct_constructor' =>
    is          => 'rw',
    lazy        => 1,
    default     => sub {
        my $self = shift;
        return "(malloc(sizeof(@{[ $self->struct_type ]})))";
    },
;
has 'type_accessors' =>
    is          => 'rw',
    isa         => 'HashRef',
    default     => sub { {} },
;


sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    $self->type_accessor(int => <<'END');
$type
$accessor( $class self, ... )
    CODE:
        if( items > 1 )
            self->$key = SvIV(ST(1));
        RETVAL = self->$key;
    OUTPUT:
        RETVAL
END

    $self->type_accessor("char *" => <<'END');
$type
$accessor( $class self, ... )
    CODE:
        if( items > 1 )
            self->$key = SvPV_nolen(ST(1));
        RETVAL = self->$key;
    OUTPUT:
        RETVAL
END

    $self->type_accessor(double => <<'END');
$type
$accessor( $class self, ... )
    CODE:
        if( items > 1 )
            self->$key = SvNV(ST(1));
        RETVAL = self->$key;
    OUTPUT:
        RETVAL
END

    return $self;
}


=head3 struct

    $writer->struct($typedef);

The typedef for the struct you'd like to write a class around.

The C parser isn't too sophisticated.

=cut

sub struct {
    my $self = shift;
    my $typedef = shift;

    # Cleanup
    $typedef =~ s{/\* .* \*/}{};    # strip C comments
    $typedef =~ s{//.*}{};          # strip C++ comments
    $typedef->strip_ws;

    $typedef =~ s/^typedef\s+//;        # optional "typedef"
    $typedef =~ s/^struct\s+(\w+)//;    # struct type
    my $type = $1;

    croak "Can't figure out the type" unless $type;

    # Get the braces out of the way.
    $typedef =~ s/.*?{\s+//;
    $typedef =~ s/\s+}.*?//;

    # All we should have left is "type key;"
    my %elements = map {
                       /^(.*)\s+(\w+)$/ ?
                           ($2 => $1) : ()
                   }
                   map { $_->strip_ws;  $_->squeeze_ws }
                       split /;/, $typedef;

    croak "Didn't see any elements in $type" unless keys %elements;

    $self->struct_type($type);
    $self->struct_elements(\%elements);
}


=head3 type_accessor

    $writer->type_accessor($type, $xs);

XS::Writer will deal with simple types, but you will have to supply
code for anything beyond that.

Here's an example for an accessor to elements with the 'double' type.

    $writer->type_accessor('double', <<'END_XS');
        $type
        $accessor( $class self, ... )
            CODE:
                if( items > 1 )  /* setting */
                    self->$key = SvNV(ST(1));

                RETVAL = self->$key;
            OUTPUT:
                RETVAL
    END_XS

Variables should be used in place of hard coding.

    $type       same as the $type you gave
    $accessor   name of the accessor function
    $class      type of the struct
    $key        the element on the struct being accessed

=cut

sub type_accessor {
    my $self = shift;
    my($type, $xs) = @_;

    my $package = $self->package;

    $xs =~ s{\$type} {$type}g;
    $xs =~ s{\$class}{$package}g;

    $self->type_accessors->{$type} = $xs;
}

=head3 make_xs

    my $xs = $self->make_xs;

Generates the XS code.

=cut

sub make_xs_header {
    my $self = shift;

    my $xs = <<END;
/* Generated by XS::Writer $VERSION */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

@{[ $self->include ]}

typedef @{[ $self->struct_type ]} *     @{[ $self->xs_type ]};

MODULE = @{[ $self->package ]}  PACKAGE = @{[ $self->package ]}  PREFIX = @{[ $self->xs_prefix ]}

@{[ $self->package ]}
@{[ $self->xs_type ]}_new(char* CLASS)
    CODE:
       RETVAL = (@{[ $self->struct_constructor ]});
        if( RETVAL == NULL ) {
           warn( "unable to create new @{[ $self->package ]}" );
       }
    OUTPUT:
       RETVAL
END

    return $xs;
}


sub make_xs_accessors {
    my $self = shift;

    my $xs = '';

    my $elements  = $self->struct_elements;
    my $accessors = $self->type_accessors;
    my $xs_type   = $self->xs_type;
    for my $key (sort { lc $a cmp lc $b } keys %$elements) {
        my $type = $elements->{$key};

        my $accessor = $accessors->{$type}
            or croak "No accessor for type $type";
        $accessor =~ s/\$accessor/${xs_type}_${key}/g;
        $accessor =~ s/\$key/$key/g;

        $xs .= $accessor;
        $xs .= "\n\n";
    }

    return $xs;
}


sub make_xs {
    my $self = shift;

    return    $self->make_xs_header
            . "\n\n"
            . $self->make_xs_accessors;
}


=head3 write_xs

    $writer->write_xs;

Writes the XS to $writer->xs_file.

=cut

sub write_xs {
    my $self = shift;
    
    my $dir = dirname($self->xs_file);
    mkpath $dir unless -d $dir;

    open my $fh, '>', $self->xs_file
        or die "Can't write to @{[ $self->xs_file ]}: $!";
    print $fh $self->make_xs;
}


=head1 SEE ALSO

Inline::Struct, ExtUtils::XSBuilder

=cut

1;
