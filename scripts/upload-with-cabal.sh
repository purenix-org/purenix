#!/usr/bin/env bash
#
# This script should be run to upload a new release of PureNix to Hackage
# using cabal.  This script also uploads documentation.
#
# First, get into a Nix shell:
#
# $ cd purenix/
# $ nix-shell
#
# Then you can just run this script:
#
# $ ./scripts/upload-with-cabal.sh

set -e

# Change to the top-level PureNix directory.
this_script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$this_script_dir/"
cd ..

sdist_dir=$(mktemp -d dist-sdist.XXXXXX)
trap 'rm -rf "$sdist_dir"' EXIT

# Create the source distribution tarball.
cabal sdist --builddir="$sdist_dir"

output_tar_file=$(find "$sdist_dir/sdist/" -type f -name "purenix*.tar.gz")

# Upload the source distribution tarball to Hackage.
cabal upload --publish "$output_tar_file"

# Generate and upload the hackage documentation as well.
# Note that we are currently in the top-level directory, so we have to
# call hackage-docs.sh in this current scripts directory.
./scripts/hackage-docs.sh
