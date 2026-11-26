# Viewed marks (personal review progress)

Files and hunks can be marked **viewed** while reviewing, so you can
put a review down and pick it up later without re-reading everything
you already went through.  Marks are personal: they are stored in the
local SQLite review store (`git-appraise-store.el`) and are never
published by ordinary git-appraise commands.

## Using it

- `v` toggles the viewed mark at point: the hunk under the cursor, or
  the whole file when point is on a file heading.
- `V` toggles the whole file from anywhere inside it (a file with
  hunks applies the action to every current hunk in one batch; a file
  without hunks — binary or metadata-only changes — gets its own
  mark).
- `N` moves forward to the next file that is not fully viewed.

The UI shows:

- a viewed indicator on a file heading once **all** of its current hunks
  (or, for a hunk-less file, the file itself) are viewed;
- `  [CHANGED]` on a file heading that was viewed before but whose
  content has changed since — a soft signal that never affects the
  mark state;
- a dimmed hunk heading for each viewed hunk;
- an orange left-margin bar on diff lines that changed after the file
  was last marked fully viewed. The existing comment marker stays in
  the fringe. For a change without a surviving diff line, the cue is
  placed on the file heading;
- `N/M viewed` below the Files changed heading.  Hunks and
  hunk-less files are the countable items; a file with hunks is not
  counted separately (its mark is the rollup).

Marks survive rebuilds, `G` reloads and Emacs restarts. The diff baseline
is saved when the whole file becomes viewed, including when the final hunk
is marked individually. Existing marks from older store versions gain a
line baseline the next time the file is fully marked viewed.

## Where it lives

Every event is one row in the `viewed_events` table of the store
database (`git-appraise-store-file`, by default
`~/.emacs.d/git-appraise/state.sqlite`), keyed by the repository and
the review's original revision (`Review.Revision`). The latest full-file
baseline is in `viewed_snapshots` under the same repository, revision,
reviewer and path. Mark events and baselines are committed together.
Inspect marks with:

```sh
sqlite3 ~/.emacs.d/git-appraise/state.sqlite \
  'SELECT * FROM viewed_events WHERE revision = "<review-revision>"'
```

The store is local to the machine and never travels with
`git-appraise push` / `git-appraise pull` (those carry only
`refs/notes/devtools/*`).  Repository identity is the canonical
common git dir, so linked worktrees share one set of marks.  After
upgrading from the git-notes backend, run `M-x
git-appraise-import-git-notes` once per repository: it moves the old
notes into the store and deletes the `refs/notes/git-appraise-el/*`
refs, recording each ref's pre-delete tip (in `legacy_imports`) so a
ref can be restored with `git update-ref` until gc.

## What resets a mark

A mark is tied to content, not position: a hunk's identity is the
canonical path plus its diff body with the `@@` range numbers
normalized away, and a hunk-less file's identity is its whole diff
block (mode lines, rename headers, binary markers included).
Therefore:

- a pure line-number shift (e.g. an unrelated commit added lines
  above) keeps the mark;
- any content change to the hunk — added/removed lines, context,
  whitespace, a trailing-newline marker — makes it unviewed again and
  lights `[CHANGED]` on the file;
- a new hunk starts unviewed; a changed hunk-less block loses its
  file mark;
- two byte-identical hunks in one file are only matched by a mark
  whose stored position still agrees — ambiguous marks are never
  guessed onto a location.

Viewed event rows are not deleted: an unmark appends a `viewed:false`
record, and if a hunk's exact content returns later (a revert), its
earlier mark can apply again.

## Reduction rules

Events are reduced per reviewer (`git config user.email` in the
reviewed repository — set it, or marks cannot be stored), item, path
and fingerprint, with last write wins by the event timestamp
(microsecond ISO-8601 UTC) and the record ID as tie-break.  Reviews
and reviewers are isolated: marks are stored per review revision and
tagged with the reviewer, so two reviews in one repository and two
reviewers on one review never share state.

If a store file is copied between devices with skewed clocks, the
later *recorded* timestamp wins for opposing events regardless of
which action happened last.

## Implementation notes

- `git-appraise-viewed-core.el` computes diff identities, inventory and
  event projection.
- `git-appraise-viewed.el` handles viewed commands and display state;
  `git-appraise-viewed-changes.el` compares full-file baselines and paints
  margin bars.
- `git-appraise-store.el` owns durable personal marks and baselines.
