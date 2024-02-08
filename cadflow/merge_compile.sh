#!/bin/sh
set -x
issues="$*"
for issue in $issues ; do
    echo "Fetching and merging.. issue  $issue"
done
exit 0
