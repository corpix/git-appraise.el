# Local git-appraise reviews

Install `git-appraise` so that `git appraise` works in your shell. Create a
review with the CLI, for example on a feature branch:

```sh
git appraise request -m "Review this change" -target refs/heads/main
```

You can also create one from Emacs with `M-x git-appraise-backend-request`: it
prompts for the source branch, target branch, message, and reviewers, runs
`git appraise request`, and opens the new review. The source defaults to the
current branch, the target to the remote's default branch, and an empty message
to the reviewed commit's message. A review already open for the same commit is
reported instead of creating a duplicate.

From a buffer in that Git repository, run `M-x git-appraise-start` and choose a
review. The list includes open and completed local reviews. When the repository
has no reviews, `git-appraise-start` offers to run the same creation flow instead
of failing. The local `appraise` backend is the only supported backend in this
checkout.

In the review buffer, `RET` on an added or context diff line opens an inline
comment editor; `C-c C-c` saves and sends the comment immediately. `C-c RET`
sends an inline comment immediately. Use `r c` to send older saved drafts.
`RET` on a file heading sends a file comment after editing. `RET`
on a previous comment opens a reply, which is sent on `C-c C-c`. Use `r s c` for
a general comment, `r a` to accept, or `r r` to reject. Rejection prompts for
feedback if none was entered. `G` reloads the review from local Git and keeps
unfinished comments when the review head has not changed. If it has changed,
Emacs asks before discarding those drafts.

git-appraise stores requests and comments in Git notes. This backend invokes
its CLI and never pushes or pulls; share review notes separately when needed.
It does not merge or submit changes to the target branch. Comments
on an older commit appear folded under their file with their original path,
line, and commit. The file heading shows a comment count while folded.
Comments without a line appear in Conversation. Files and hunks start folded;
`TAB` and the `r` menu save your fold choices in a personal Git note. The CLI
cannot create a comment on a removed line of
the current diff because that line is absent from the review head file.

`TAB` on a listed commit folds the commits group and returns to its heading.

File headings show status, classification (such as GEN or DOC), added and
deleted line counts, then the filename. The columns stay aligned while a large
file waits for its diff to load. Set `git-appraise-show-file-change-summary` to
`nil` to hide the counts.
