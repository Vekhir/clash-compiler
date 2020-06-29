#!/bin/bash
set -xou pipefail

egrep ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

# Check that clash-dev works
if [ "$RUN_CLASHDEV" = "yes" ]; then
  echo "" > clash-dev.result
  echo 'genVHDL "./examples/FIR.hs" >> writeFile "clash-dev.result" "success"' | ./clash-dev
  if [[ "$(cat clash-dev.result)" != "success" ]]; then
    echo "clash-dev test failed"
    exit 1
  fi
fi

# Check whether version numbers in snap / clash-{prelude,lib,ghc} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-lib/clash-lib.cabal clash-ghc/clash-ghc.cabal clash-cores/clash-cores.cabal"
snapcraft_file="bindist/linux/snap/snap/snapcraft.yaml"
versions=$(grep "^[vV]ersion" $cabal_files $snapcraft_file | grep -Eo '[0-9]+(\.[0-9]+)+')

if [[ $(echo $versions | tr ' ' '\n' | wc -l) == 5 ]]; then
    if [[ $(echo $versions | tr ' ' '\n' | uniq | wc -l) != 1 ]]; then
        echo "Expected all distributions to have the same version number. Found: $versions"
        exit 1;
    fi
else
    echo "Expected to find version number in all distributions. Found: $versions";
    exit 1;
fi

# You'd think comparing v${version} with ${CI_COMMIT_TAG} would do the
# trick, but no..
CI_COMMIT_TAG=${CI_COMMIT_TAG:-}
version=$(echo $versions | tr ' ' '\n' | head -n 1)
tag_version=${CI_COMMIT_TAG:1:${#CI_COMMIT_TAG}-1}  # Strip first character (v0.99 -> 0.99)

if [[ ${tag_version} != "" && ${version} != ${tag_version} ]]; then
    if [[ "${CI_COMMIT_TAG:0:1}" == "v" ]]; then
        echo "Tag name and distribution's release number should match:"
        echo "  Tag version:          ${CI_COMMIT_TAG}"
        echo "  Distribution version: v${version}"
        exit 1;
    else
        echo "\$CI_COMMIT_TAG should start with a 'v'. Found: ${CI_COMMIT_TAG}"
        exit 1;
    fi
fi

# Run unittests, doctests
if [ "$RUN_LIBTESTS" = "yes" ]; then
  # Create a ghc environment file needed for the doctests
  # On cabal>=2.4.1.0 and ghc<8.4.4 this isn't done automatically
  cabal --write-ghc-environment-files=always new-build all
  cabal new-test clash-cores clash-cosim clash-prelude clash-lib
fi

# Run HDL generation / simulation
if [ "$RUN_TESTSUITE" = "yes" ]; then
  cabal new-run -- clash-testsuite -j$THREADS --hide-successes -p "/.VHDL./ || /.Verilog./"
fi
