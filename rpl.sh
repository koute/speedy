#!/usr/bin/env bash

DIR=$(dirname $(realpath "$0"))
cd $DIR
set -ex

sd 'read_from_buffer' 'load' $(fd -e rs -e toml -e md)
