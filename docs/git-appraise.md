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
Before asking for branches or a message, Emacs stops if tracked changes are
uncommitted. With only untracked files, it asks whether to continue; those
files are not part of the review.

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
Use `r a` to accept and `r R` for explicit rejection. `g` or `G` reloads from local
Git while retaining unsent drafts, even when the branch has gained commits.
An inline draft on an old head must be re-anchored before it can be sent.
Git-appraise derives the review's status from its comment threads: an
unaddressed `-nmw` finding makes the CLI report the review as rejected. The
explicit `r R` action adds a review-wide rejection event; `r a` adds an
acceptance event. A resolving reply addresses its parent finding without
silently accepting the whole review.
The header shows the review decision and lifecycle separately. Decision is
pending, approved, or rejected; lifecycle is open, abandoned, or landed.

Use `r E` to edit the request's description, reviewer list, or target branch.
The source branch and original review anchor stay fixed. A comment's original
author can edit it with `e`; `C-c C-s` changes their root comment between
FYI and needs-work. `C-c C-u` reopens an addressed finding with a needs-work
reply. Published edits append new discussion events, leaving a foldable history
under the comment. The CLI lacks an edit command, so the package writes the
schema-supported edit event directly to the Git discussion note.

git-appraise stores requests and published comments in its shared Git notes.
Unsent drafts and progress marks live in the personal SQLite store
(`git-appraise-store.el`), local to the machine. `r p` pushes git-appraise
notes; `r P` pulls teammates' review notes. The explicit `r L` command lands a
review into its target. Comments
on an older commit appear folded under their file with their original path,
line, and commit. The file heading shows a comment count while folded.
Comments without a line appear in Discussion. Its folded headings show compact
status badges and counts; timestamps appear inside opened sections. Files start folded; hunks
without saved choices open when you open their file. `TAB`, `S-TAB`, and the
`r` menu save fold choices in the personal SQLite store. The CLI
cannot create a comment on a removed line of
the current diff because that line is absent from the review head file.

Use `r D` in a review buffer, or `? R D` from Magit, to remove a local review.
Before confirmation, Emacs checks configured remotes and warns when it finds
the request note there or cannot verify a remote. After confirmation, removal
clears the request and discussion notes for its
commit and clears its local drafts, viewed marks, and folds. The commit is not
deleted, and no garbage collection runs. Removing only the request note with
`git notes` leaves the separate discussion note; requesting the same commit
again then brings back its old comments and decision.
These note changes push normally. If another person updated the remote notes,
pull and merge them first. A merge can restore a removed note, so repeat the
removal if it reappears before pushing. Do not force-push the notes ref.

`TAB` on a listed commit folds the commits group and returns to its heading.

File headings show status, classification (such as GEN or DOC), added and
deleted line counts, then the filename. The columns stay aligned while a large
file waits for its diff to load. Set `git-appraise-show-file-change-summary` to
`nil` to hide the counts.
