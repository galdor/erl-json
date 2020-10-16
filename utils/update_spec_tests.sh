#!/bin/sh

set -eu
set -o pipefail

root=$(realpath $(dirname $0)/..)

repository="https://github.com/json-schema-org/JSON-Schema-Test-Suite.git"
version="draft2019-09"
repository_dir="$root/tmp/spec-repository"
test_data_dir="$root/priv/test/spec-tests"

rm -rf $root/tmp
mkdir $root/tmp

rm -rf $test_data_dir
mkdir -p $test_data_dir

echo "cloning $repository"
git clone -q $repository $repository_dir

cp -r $repository_dir/tests/$version/* $test_data_dir
