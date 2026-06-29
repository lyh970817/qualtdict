# TASK

Merge the following branches into the current branch:

{{BRANCHES}}

For each branch:

1. Run `git merge <branch> --no-edit`
2. If there are merge conflicts, resolve them intelligently by reading both sides and choosing the correct resolution
3. After resolving conflicts, run `Rscript -e 'devtools::test()'` and the strongest practical R package checks for the merged changes
4. If tests fail, fix the issues before proceeding to the next branch

After all branches are merged, make a single commit summarizing the merge.

# CLOSE ISSUES

For each branch that was merged, close its issue using the following command:

`gh issue close <ID> --comment "Completed by Sandcastle"`

If closing those child issues completes a parent PRD, close the parent PRD too.
Only close a parent PRD after checking that all linked implementation issues
for that PRD are closed or were merged in this run. Do not close an unfinished
parent PRD merely because one child issue completed.

Here are all the issues:

{{ISSUES}}

Once you've merged everything you can, output <promise>COMPLETE</promise>.
