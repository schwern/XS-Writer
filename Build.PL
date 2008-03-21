#!/usr/bin/perl -w

use Module::Build;

my $build = Module::Build->new(
    module_name     => 'XS::Writer',

    license         => 'perl',
    
    requires        => {
        'Moose'             => 0.40,
        'Moose::Autobox'    => 0.06,
        'Carp'              => 0,
    },
    build_requires  => {
        'Test::More'        => 0.62,
        'Test::Differences' => 0,
    },
    configure_requires  => {
        'Module::Build'     => 0.2808,
    }
);
$build->create_build_script;
