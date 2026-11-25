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

Keep the same review while the source branch gains fix commits. Its anchor is
the first reviewed commit; the current branch tip supplies the evolving diff.
Create another request for a separate logical change, with its own first
commit. A normal review round does not create a second request.

From a buffer in that Git repository, run `M-x git-appraise-start` and choose a
review. The list includes open and completed local reviews. When the repository
has no reviews, `git-appraise-start` offers to run the same creation flow instead
of failing. The local `appraise` backend is the only supported backend in this
checkout.

In the review buffer, `RET` on an added or context diff line opens an inline
comment editor; `C-c C-c` saves a draft. `C-c RET` sends one inline comment
immediately. Use `r c` to publish staged drafts as FYI, or `r r` to publish
them as needs-work findings. `C-c C-f` on a draft exempts it as FYI in a
needs-work batch. With no actionable draft, `r r` asks for a summary and
publishes that as a needs-work comment. `RET` on a previous comment opens an
ordinary reply; `C-c C-r` publishes a reply that marks that finding addressed.
Use `r a` to accept and `r R` for explicit rejection. `G` reloads from local
Git while retaining unsent drafts, even when the branch has gained commits.
An inline draft on an old head must be re-anchored before it can be sent.
Git-appraise derives the review's status from its comment threads: an
unaddressed `-nmw` finding makes the CLI report the review as rejected. The
explicit `r R` action adds a review-wide rejection event; `r a` adds an
acceptance event. A resolving reply addresses its parent finding without
silently accepting the whole review.

git-appraise stores requests and published comments in its shared Git notes.
Unsent drafts and progress marks live in separate personal notes refs. A local
write stays local until `r p` pushes git-appraise notes; `r P` pulls teammates'
review notes. The explicit `r L` command lands a review into its target. Comments
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
