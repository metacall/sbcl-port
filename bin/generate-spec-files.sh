#!/bin/sh

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`
PROJECT_HOME=`readlink -f ${SCRIPT_DIR}/..`

macrofile=$(mktemp)

gcc_version=$(gcc -dumpversion)

# c2ffi -i /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/include $PROJECT_HOME/c2ffi-spec/metacall.h --driver null --macro-file $macrofile --arch x86_64-pc-linux-gnu
# c2ffi -i /usr/lib/gcc/x86_64-linux-gnu/${gcc_version}/include $PROJECT_HOME/c2ffi-spec/metacall.h --driver json --output $PROJECT_HOME/c2ffi-spec/metacall.x86_64-pc-linux-gnu.spec --arch x86_64-pc-linux-gnu

c2ffi $PROJECT_HOME/c2ffi-spec/metacall.h --driver null --macro-file $macrofile --arch x86_64-pc-linux-gnu
c2ffi $PROJECT_HOME/c2ffi-spec/metacall.h --driver json --output $PROJECT_HOME/c2ffi-spec/metacall.x86_64-pc-linux-gnu.spec --arch x86_64-pc-linux-gnu

rm $macrofile
