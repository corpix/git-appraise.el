# AGENTS.md

Guidance for agents and humans working in this repository.

## Project and repository hygiene

`git-appraise` is an Emacs package for local git-appraise reviews. Git notes
hold published requests and discussions; an EmacSQL SQLite store holds
personal drafts, folds, and viewed progress. There is no forge backend.
This checkout is used by the user's Emacs, so changes can affect a live session.

- Never write facts about untracked or ignored content into tracked files.
  This includes AGENTS.md, documentation, tests, and commit messages. Base
  tracked statements on tracked source or explicit user requirements. Keep
  private workspace observations in the conversation unless the user asks
  to publish them.
- Work on disposable Git repositories and in-memory or `/tmp` SQLite stores
  in tests. Do not mutate a user's review notes or personal database while
  investigating a reported problem.
- Keep commits focused and use `type(scope): imperative summary` or
  `type: imperative summary`. Do not add AI attribution or tool credit.

## Layout

| Area | Files |
| --- | --- |
| Entry points and commands | `git-appraise.el`, `git-appraise-actions.el`, `git-appraise-request-edit.el`, `git-appraise-remove.el` |
| Local backend | `git-appraise-backend*.el`, `git-appraise-comment*.el` |
| Rendering | `git-appraise-section.el` and `git-appraise-section-*.el`, `git-appraise-render.el`, `git-appraise-lazy.el` |
| Diff and appearance | `git-appraise-diff.el`, `git-appraise-file-types.el`, `git-appraise-badges.el`, `git-appraise-faces.el` |
| Personal state | `git-appraise-db.el`, `git-appraise-store*.el`, `git-appraise-pr-state.el`, `git-appraise-viewed*.el`, `git-appraise-fold-state.el` |
| Tests | `test/unit`, `test/integration`, `test/workflow`, `test/support` |

Prefer dash.el for list processing. Use a local predicate for one-off logic;
extract cohesive helpers when reused. Split files beyond about 800 lines and
functions beyond about 80 lines by concern. The section facade shows how to
keep existing `require` sites while splitting an implementation.

## Verification

```sh
just compile       # byte-compile package files
just test          # batch ERT suite
just bench         # 200-file viewed-action timing
just profile       # CPU and memory profile in ignored tmp/performance/
just bench-submit  # 200-file review-submission timing
just profile-submit # submission CPU and memory profile
```

- Run `check-parens` in `emacs-lisp-mode` on each changed Elisp file before
  tests. A balanced file can still have nested forms in the wrong function;
  inspect the byte-compiler output and disassemble a function that appears
  to run but has no effect.
- Redirect the complete compile log. Exit 0 alone is insufficient: search
  it for `Error` and ensure touched `.elc` files are newer than their `.el`
  sources. A failed compile can leave stale bytecode serving old behavior.
- Set `native-comp-jit-compilation` to nil as the first batch probe eval,
  and use the full package load path. Keep probes disposable and bounded.
- Run the focused tests, then the full suite before committing. Run `bench`
  and `profile` when changing render or bulk-action work; run `bench-submit`
  and `profile-submit` when changing submission or discussion refresh.
- Respect the user's live-session constraints. If live probing is authorized,
  load each touched file rather than using `require`, remove defuns that no
  longer exist, and check a fresh render plus re-render stability. Do not
  drive interactive prompts through `emacsclient --eval`.
- Commit verified work in coherent increments. Do not commit a tree that
  fails compile or test.

## Test conventions

- Tests run through `test/run-tests.el` in batch Emacs. Store tests use an
  in-memory database or a disposable file for restart cases; never use the
  real personal store.
- Use real commands or keys for workflow tests, real Git and git-appraise
  for integration tests, and pure projections for unit tests. A render test
  must kill its buffer in `unwind-protect` and disable unrelated background
  history work.
- Asynchronous renders need a new-render marker, not a poll that can match
  old buffer content. Check section visibility through overlays, not the
  `hidden` slot alone.
- Cover large-review submission with a finding whose file has left the diff.
  Check that the finding stays visible in Discussion and that resolution and
  approval keep the cursor on the active comment.
- Use absolute fixture paths and deterministic commit dates when test
  expectations depend on time or commit order. `tabulated-list` entries are
  `(list ID VECTOR)`; `header-line-format` is not in `buffer-string`.
- Test files are loaded, not required, and have no `provide` form. Test
  names use `file-name/description`.

## Elisp, Magit, and Git invariants

- A function used as a plain `cl-letf` target may need `fset` with
  `unwind-protect`. A lambda that refers to sibling bindings belongs in
  `let*`, not plain `let` on Emacs 30.2.
- Match data is global. Save `match-string` results before a nested regexp,
  and pass the source string explicitly. `delete-dups` mutates lists; copy
  shared values first. `apply` requires a list as its final argument.
- `magit-wash-sequence` stops when a washer returns nil. Magit section
  accessors use `oref`; `magit-map-sections` collects through side effects.
  Use the section tree to collect staged items instead of scanning every
  rendered line. `magit-insert-heading` may change point and append a child
  count.
- Magit section folding needs a heading, a hidden section spec, and
  `git-appraise-section--hide-if-hidden` on every render. Keep Magit's
  section identity and keymap properties when editing headings in place.
  A `defvar` keymap is shared across reloads; patch it with `define-key`.
- Use `magit-git-output` for multi-line diffs. `magit-git-string` returns
  one line. Preserve trailing newlines in diff blocks so file headers do not
  join. Use canonical paths from diff headers.
- Keep full-review reloads out of comment-only actions when an in-place
  update suffices. Preserve point and window position unless an action
  explicitly navigates. Test both rendered state and cursor location after
  interactive actions. When replacing Discussion, preserve child group folds.
  Anchor a published-comment cursor to its stable ID; preserve other positions
  with a marker.
- A published comment edit appends a discussion event with `original` set
  to the stable root hash. A reply resolving a finding uses native `-lgtm`;
  a needs-work reply can reopen it. Requests and discussions live in separate
  Git notes refs, so removing a review requires clearing both.
- Classify files by stable type, not their user-facing label. Read the five
  Linguist attributes in one NUL-delimited `git check-attr --stdin -z` call.
  An explicit attribute unset differs from an unspecified attribute. Keep
  file-type labels and faces customizable without changing DOC statistics,
  focus hiding, or sort behavior.
- On promisor or partial clones, avoid blob-reading Git calls that can
  fetch from the network. Cap lines before regexp scans of diff text.

<!-- git-appraise-conventions v4 -->

## Code review conventions

- A requirement's EARS definition is the first line of its primary ERT
  docstring: `WHEN ... THE SYSTEM SHALL ...`. State a user-visible outcome,
  not a keybinding, function, section shape, or face.
- Give each requirement a stable date-numbered ID (`YYYY-MM-DD-NNN`). Put
  that ID in the ERT name and in the `;;; req-<id>:` header above its guard.
  The header contains machine keys only: `covers:`, `covers-fn:`, and
  `enforcement:` (`test`, `runtime`, or `convention`).
- Treat changes to an EARS definition or requirement block as a spec change.
  Keep local priorities and notes out of tracked requirement source.
- Agents author the requirement block and guard test as part of a change.

<!-- end git-appraise-conventions v4 -->
