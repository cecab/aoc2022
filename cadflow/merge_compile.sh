#!/bin/sh
set -x
issues="$*"
branches=""
for issue in $issues ; do
    echo "Fetching and merging.. issue  $issue"
    issue_branch=`git branch -a | grep -- "-issue-${issue}-" | tr -d " "`
    branches="${branches} $issue_branch"
done
echo "Mering branches: $branches"
exit 0
