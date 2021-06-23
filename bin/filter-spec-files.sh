#!/bin/bash

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`
PROJECT_HOME=`readlink -f ${SCRIPT_DIR}/..`

patterns=(
    "$PROJECT_HOME/c2ffi-spec/metacall.*.spec"
)

queries=(
    # it's tempting to do [sort_by(.name) | .[] |... for nice
    # diff'ability, but the lisp generator works in a streaming
    # fashion, i.e. it is sensitive to the order of the definitions
    # (e.g. it filters out struct fields whose type hasn't been seen
    # yet).
    '.[] '
)

for i in "${!patterns[@]}"; do
    for file in ${patterns[$i]}; do
        names=$(for line in "$(grep long-double $file )" ; do echo "$line" | sed -e 's/,$//g' | jq '.name' ; done)

        query='.[] | select ( .tag != "typedef" ) '
        for n in $names ; do
            query="${query} | select ( .name != $n )"
        done

        echo "query: ${query}"

        echo "Running jq '${query}' ${file}"
        jq "${query}" ${file} > ${file}.filtered
        mv ${file}.filtered ${file}
    done
done
