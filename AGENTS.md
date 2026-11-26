# AGENTS.md

Guidance for AI agents (and humans) working in this repository.

## What this is

`git-appraise` is an Emacs package for reviewing local
[git-appraise](https://github.com/google/git-appraise) reviews in a
magit-section based read/write buffer.  It works with git and
git-appraise only; no forge/remote backends.  It is *installed and actively
used* from this checkout (`~/.emacs.d/elpa/git-appraise`), so changes here
affect the user's live Emacs immediately.

## Layout

| File | Role |
|---|---|
| `git-appraise.el` | entrypoints, `git-appraise-mode`, transients |
| `git-appraise-section.el` | render FACADE + build orchestration (`--trigger-hooks`, `--internal-build`, `--build-buffer`) |
| `-section-shared/-header/-analysis/-comment/-wash/-criteria.el` | the render parts: primitives, PR header, analysis + review order, comments, the owned diff wash, criteria checklist |
| `git-appraise-diff.el` | diff classification (SPEC/DOC/GEN, file order, noise rules) |
| `git-appraise-render.el`, `-lazy.el` | render driver, lazy section expansion |
| `git-appraise-reactions.el` | reaction toggles (description/conversation/code comment) |
| `git-appraise-analysis.el` | phase 5 heuristics + phase 15 hunk delicacy |
| `git-appraise-history.el` | history harvest, review heat, coupling matrix source |
| `git-appraise-coupling.el` | co-change completeness findings |
| `git-appraise-dossier.el` | on-demand hunk context (`C-c C-h`) |
| `git-appraise-testimpact.el` | CI-gaming detector, test mapping, `T` runner |
| `git-appraise-hunkhighlight*.el` | treesit semantic faces, per-language queries, intra-line marks |
| `git-appraise-criteria*.el` | criteria engine, brief, grill, regression request, checklist |
| `git-appraise-budget*.el` | review budget queue + measured focus minutes |
| `git-appraise-local.el` | local working-tree review |
| `git-appraise-viewed.el` | viewed state and its projection onto the diff |
| `git-appraise-pr-state.el` | unsent feedback/draft persistence (store-backed) |
| `git-appraise-db.el` | in-memory review state; durable state lives in the store |
| `git-appraise-sql.el` | query builder returning `(SQL . PARAMS)` for native sqlite |
| `git-appraise-store.el` | SQLite store: connection, migrations runner, CRUD (`~/.emacs.d/git-appraise/state.sqlite`) |
| `git-appraise-store-migrations.el` | versioned schema registry (`PRAGMA user_version`) |
| `git-appraise-store-import.el` | one-shot legacy git-notes import (`M-x git-appraise-import-git-notes`); deletable later |
| `git-appraise-browse.el` | browse-url integration |
| `git-appraise-folding.el` | fold commands |
| `git-appraise-actions.el` | interactive/navigation commands |
| `git-appraise-{repo,comment,utils,faces,parse-hunk,interfaces,people,search,auth,appraise,agents-shell}.el` | support; `agents-shell` is the optional reference drafter (soft dependency) |
| `test/` | ERT tests (`just test`) |

## Elisp style

Process lists with **dash.el** (`--each`, `--map`, `-filter`, `--count`,
`->>`): prefer it over hand-rolled `dolist`/`push`/`nreverse` loops and
bare `cl-loop`.  Keep a one-off predicate in the surrounding `let*`; lift
it to a named defun only when reused.

## File size guideline

Keep source files **500–800 lines**.  Past 800 lines — or a single
function past ~80 — stop and refactor: extract cohesive helpers, then
split by concern (the `git-appraise-section-*` facade split is the
template: thin facade that `(require)`s its parts in dependency order, so
existing `(require 'git-appraise-section)` sites keep working; cross-part
calls use `declare-function`, classes are plain symbols at compile time).
This applies to files already over the limit (`-actions`,
`-analysis`, `-hunkhighlight`): shrink them when touched.  Do not pad or
merge files to hit the range.

## Build and test

Dependencies are ordinary package.el packages (in `~/.emacs.d/elpa`, or
`just deps` on a fresh machine). To locate a dependency source: `emacs -Q --batch --eval '(princ (locate-library "magit-section"))'`.

```sh
just compile   # byte-compile all package files
just test      # ERT suite in batch emacs
just deps      # install dependencies from MELPA
```

Tests are plain ERT run by `test/run-tests.el` in a batch `emacs -Q`
(no cask/buttercup).

### Test conventions

- DB-touching tests run inside `git-appraise-test--with-db` / a repo
  harness: the store file is pinned to a fresh `/tmp` sqlite for the
  whole batch by `test/run-tests.el`; the user's real database is
  never touched. `git-appraise-test--reset-db` also closes the store
  connection (that is how the "restart" tests simulate a reopen).
- Section assertions use `git-appraise-test--sections-match`.
- ERT names are `file-name/description` (slash form).

## Verification protocol

1. `just compile`.  **Exit 0 proves nothing** — grep the log for `Error`
   and check the `.elc` mtime is newer than the `.el`.  Also purge stale
   native-comp caches: `rm -f ~/.emacs.d/eln-cache/*/git-appraise-*.eln`.
2. `just test`.
3. In the live daemon, `(load "file")` every touched file (not
   `require`), then render a fresh buffer and check section counts plus
   re-render stability.  `fmakunbound` removed defuns; `setq` changed
   defcustom defaults explicitly.
4. Commit completed work as you go, per Commit style below.  Small,
   focused commits (a feature with its tests, a fix, a docs touch-up)
   beat one big pile at the end; never commit a tree that fails
   `just compile` / `just test`.

## Commit style

- Follow the repo's existing subject style: `type(scope): summary` or
  `type: summary`.  Common types: `feat`, `fix`, `test`, `docs`,
  `refactor`, `build`, `chore`, `ci`.
- Use an imperative, specific summary; include a scope when it names the
  affected area (`fix(section): ...`, `test(local): ...`).
- Keep the subject concise; add a body only when the why is not obvious.
- No AI attribution.  Do not add `Co-authored-by` trailers, "Generated
  by", "AI-assisted", or similar AI/tool credit lines to commits or PRs.

## Gotchas

### Compile and artifacts

- A failed compile leaves the STALE `.elc` serving old bytecode, so tests
  and probes pass green on code you did not write.  After every compile
  (and every interrupted one) verify log + mtime.  Never filter compile
  output — a `grep | head` once hid "End of file during parsing".
- `check-parens` every source/test edit before running the suite.  An
  unbalanced test file kills the whole batch at load time; the real error
  drowns above make's `-L` echo, so redirect output to a file.
- Balanced-but-mis-nested code passes `check-parens`.  When a function
  "runs but does nothing" (or silently returns nil), `(disassemble 'fn
  BUF)` first — output goes to BUF or `*Disassembly*`, not
  `standard-output`.
- Big block edits: splice by line range with boundary assertions, and
  check net opener/closer counts with awk.  Hand-counting closers was
  wrong repeatedly; pattern anchors for bottom-up splices can eat a block
  inserted by an earlier splice.
- A macro defined in a file compiled LATER in the same pass leaves
  `(invalid-function ...)` at runtime.  The justfile compiles
  macro-defining files first; outside rebuilds (package.el, Emacs
  upgrade) can reintroduce it — purge `eln-cache` and reload.
- `git-appraise-autoloads.el` is generated and gitignored.  Regenerate by
  deleting it, then `(package-generate-autoloads "git-appraise" PKG-DIR)`;
  a fresh file keeps the `add-to-list` load-path form that update mode
  silently drops.  Verify the form is present.
- Batch probes: `(setq native-comp-jit-compilation nil)` as the FIRST
  eval (shared eln-cache races the daemon), and use the full elpa
  `load-path`, not a hand-picked `-L` list.
- Pipe probe output through `grep -a` (byte-code prin1 makes grep call
  the stream binary).

### Daemon probing

- `(require 'foo)` is a no-op once loaded, and old defuns survive a
  reload.  `(load FILE)` returns `t`, not the file's value — have scripts
  store their report in a defvar and read that back.
- To rebind a changed defcustom default: `(setq VAR (eval (car (get 'VAR
  'standard-value))))`, then print the bound value.
- `(defvar X nil)` PRE-BINDS X and silently kills a later defcustom's
  default.  Forward-declare valueless: `(defvar X)`.
- A `defvar` keymap does not rebind on reload; patch the shared map in
  place with `define-key`.  Re-defining a derived mode does not update
  live buffers — patch their local map and hooks.
- After a daemon restart, code is autoloaded only: load `git-appraise.el`
  explicitly.  Emacs 31.1 refuses to re-load a previously loaded `.el`
  path ("End of file during parsing: #<killed buffer>") — verify a reload
  via `(documentation 'fn)` for a new marker word.
- Never run heavy or unbounded compute in the daemon (a `git grep` froze
  Emacs for two minutes; walking all sections of all review buffers hung
  it).  Measure in batch, keep probes single-pass and targeted.
- Never drive prompting commands via `emacsclient --eval` — the prompt
  pops in the user's frame.  Set state and call the machinery.
- A poll for an async render can be satisfied by the buffer's PREVIOUS
  content; poll on a marker only the new render produces.
- `check-parens` every `/tmp` script before loading it.  `search-forward`
  is literal (not regexp), sets no match data, and must search something
  unique to the target.  `how-many` counts from point — go to
  `point-min` and anchor the pattern.

### Elisp pitfalls

- Match data is global and clobbered by nested `string-match`: bind
  `match-string` results immediately, and pass the string explicitly
  (`(match-string 1 STR)`).
- `?` is a regexp quantifier (escape a literal `?`), and so is the `+` of
  `^@@ -1,3 +1,4` (escape it).
- `(string-prefix-p "\\" line)` guards the `\ No newline` marker;
  `"\\\\"` is two characters and guards nothing.
- `apply`'s last argument must be a list — `append` trailing scalars.
- `delete-dups` is destructive and `append` shares tails: use
  `(delete-dups (copy-sequence ...))` on cached/shared lists.
- `split-string` on text ending in a newline yields a trailing `""` — use
  OMIT-NULLS when empty lines are not data.
- On Emacs 30.2 a lambda in a plain `let` does NOT capture a sibling
  binding (silently dynamic).  Use `let*`; it is load-bearing.
- cl-loop: an `unless` before `for` does not filter; `_`-prefixed
  destructured vars raise void-variable.
- `(interactive "p")` passes 1, not nil — key no-prefix on
  `(member arg '(nil 1))`.
- An interactive spec's condition-case handler ending in `(message ...)`
  returns that string as the command's argument.  Signal `user-error`, and
  validate by the parser's RESULT (a nil-returning parser checks nothing).
- The `a` library has no `a-cons`; `a-get-in` keys are bare symbols.
- `advice-member-p` takes the advice SYMBOL, not the function object.

### magit sections and rendering

- `magit-wash-sequence` stops when a washer returns nil — inserters must
  return the section.
- magit 4.x accessors are oref-based; `magit-map-sections` takes
  (FN &optional SECTION), returns the walked section, so collect by side
  effect.  We paint hunk faces eagerly (`--paint-hunk`).
- The wash is owned (`git-appraise-wash-*`); never re-advice magit-diff
  internals.  Use only magit-SECTION (stable).  Transcribe magit
  internals exactly and prove the copy on real input — a dropped backref
  group silently matched nothing for days.
- `magit-git-string` returns the first line only; use `magit-git-output`
  for diffs.  Both set GIT_LITERAL_PATHSPECS=1, so glob pathspecs need
  plain `call-process`.
- `magit-insert-heading` appends the child count and leaves point at the
  first body line's bol; paint the heading by going `forward-line -1`.
  In tests match with `[[:space:]\n]*` between words, never a trailing
  colon.
- A section folds by default only with: a heading, an explicit
  `git-appraise-section--hide-if-hidden`, and HIDE in the `(CLASS VALUE
  HIDE)` spec.  A `hidden` slot is not visibility on a direct re-render —
  run the fold walk on EVERY render.  Tests assert the `invisible`
  overlay, not the slot.
- `magit-section-show-level` keys on nesting depth, which differs between
  real renders and the wash harness — fold by section TYPE.
- magit buffer names have no leading star; log sections carry short shas;
  `magit-log-head` needs the merged `"-n3"` form.
- Dual-role comment classes delegate `magit-section-ident-value` through
  `value`; their data lives on the value object.  `-p` predicates are
  exact-class, and the `local?` slot lies — classify by class.
  Hand-built instances in tests need `hidden` and `start`/`end` set.
- Semantic faces must be OVERLAYS (text-property face lists merge with
  earlier faces winning); mark them `cr-hh-face`, `evaporate`.
- The section `keymap` slot routes keys, but `replace-match` drops all
  text properties — capture `(text-properties-at BEG)` and restore.  A
  symbol-valued `keymap` property is inert (use the evaluated map), and
  ERT must press the real key.
- In-place heading edits: never rewrite a whole heading span (magit sets
  `start` insertion-type t).  Edit surgical spans, copy the neighbor's
  properties, and bind `inhibit-read-only`.
- `buffer-swap-text` re-points every marker into the backup buffer: never
  use `render--call` for a partial edit.
- Every buffer-local that must outlive the wash needs `permanent-local`
  (the deferred tail re-runs the mode body).

### Diff, git, analysis

- Never trim the trailing newline of stored diff text — file blocks glue
  onto the next header.
- Canonical diff paths come from the header (`git-appraise-path`), never
  from membership guesses.
- magit's `d`-on-region diffs endpoint trees; include the oldest commit
  with `OLD^..NEW` (the empty-tree sha for a root commit).
- On promisor/partial clones skip blob-reading git calls (`git blame`):
  they block Emacs on network fetches.
- `git grep -l` is ~25x faster than `-n -F -f` when you only need files.
- `--exclude=` is a `git log` SUBCOMMAND option; misplaced it errors, and
  `call-process` drops stderr (silently empty results).  Capture stderr
  before debugging such emptiness.
- Cap lines before any regexp over diff text ("Stack overflow in regexp
  matcher" on megabyte lines); use `string-search` for full-line scans.
  Keep `git-appraise-analysis-run` condition-cased — an engine error reads
  as "Got an error from your VC provider".
- `git ls-files --cached --others --exclude-standard` is unsorted — sort
  before comparing.
- Range/semantics questions: probe with real git before shipping.

### DB and persistence

- `git-appraise-db` is an in-memory hash table (`git-appraise-db--objects`); `git-appraise-db-update` stores in memory. Durable personal state is SQLite (`git-appraise-store.el`), not closql.
- Personal state (viewed/fold events, pr-state snapshots) lives in the store under the repository's canonical common git dir; the import job (`git-appraise-store-import.el`) moves legacy `refs/notes/git-appraise-el/*` notes in and deletes the refs. Hydrate on pullreq create; persist on feedback/draft mutations.
- Strings persisted to the store/JSON should be `substring-no-properties`'d.
- An untagged file has a PRESENT hash key with nil value — use a missing-key sentinel.  Likewise, a truthy cache-miss sentinel inside `or` short-circuits and leaks: use the two-step let/if.
- `slot-boundp` on nil signals; guard with `(and obj (slot-boundp obj 'slot))`.

### ERT tests

- Test files have no `provide` form — `load` them.
- `cl-letf` on a plain function needs `(setf fn)`; use `fset` +
  `unwind-protect`.
- Render-level tests must be hermetic (bind `git-appraise-history-enabled`
  nil) AND leak-proof (`unwind-protect` + `kill-buffer`), or a sentinel
  re-render deletes a later test's LOCAL row.  Sentinels must never
  signal.
- Render tests need `git-appraise-repo-enable` at its default `t`; db-only
  tests bind it nil.
- The deferred render is async even in batch: poll `(sit-for 0.05)` until
  `raw-diff` is set and `deferred:queue` is nil.
- `git-appraise-db-pullreq` is abstract — build rows with a concrete
  subclass.
- ERT `equal` does not glob — assert buffer names with `string-match-p`.
  `face-at` helpers read the last char of the match, so end the search
  string on the token.
- Date-numbered ids in expectations come from `(format-time-string
  "%Y-%m-%d")`.
- Fixture paths must be absolute (`make-temp-file`); a relative `:file`
  writes into the repo during `just test`.
- Deterministic fixture commits need `GIT_AUTHOR_DATE`/
  `GIT_COMMITTER_DATE`; fast-import needs `from` after `data`, fresh blob
  marks per commit, and `git reset --hard` afterwards.
- `tabulated-list` entries are `(list ID VECTOR)`, and
  `header-line-format` is not part of `buffer-string`.

### Feature invariants

- A build that replaces the stored diff snapshot must compute phase 13
  news first; the commit buffer's build overwrites the PR snapshot (one
  spurious NEW pass).
- The new-since-last-review defvar is PR-scoped — clear it for
  COMMIT-FOCUS renders.
- NEW is independent of classification: keep DOC/GEN/heat tags, focus
  filtering and folds, and share the collapse policy between the washer
  and the post-fetch pass.
- shr crashes kill the whole render; HTML insertion is condition-cased.
- Focus tracking: `buffer-list-update-hook` does not fire while you sit
  in a buffer — use `post-command-hook` with a generous (900s) discard
  threshold.

<!-- git-appraise-conventions v4 -->

## Code review conventions

### Criteria (requirements)

- The EARS DEFINITION is the FIRST LINE of the test's own
  DESCRIPTION: the docstring of the primary guard test in
  ERT (`WHEN ... THE SYSTEM SHALL ...`) — a `C-h f` bonus,
  readable with zero tooling; a standardized comment block
  above the test carries the MACHINE KEYS only: `covers:`
  (paths), `covers-fn:` (functions), `enforcement:`
  (test|runtime|convention).
- Every requirement binds to its guard tests: the req id
  appears in the ERT test name (`file/req-<id>-description`)
  and in the `;;; req-<id>:` block header above the primary
  test.
- The id is stable and date-numbered (`YYYY-MM-DD-NNN`);
  local tool state (priorities, notes) lives in the untracked
  `specs/index.md`, never in test source.
- A criterion states a USER-VISIBLE SYSTEM OUTCOME, never an
  interface detail — no keybindings, command names, section
  shapes or faces in a criterion (those are documentation;
  the WHEN names what the user does or the state of the
  world, the SHALL names what the system does about it).
  Editing the definition (the EARS docstring or the block)
  IS editing the requirement: review diffs that touch `;;;
  req-` blocks or EARS docstrings as SPEC CHANGES.

### If you are an AI agent working in this repository

- Author req blocks in tests yourself, in the format above,
  as part of the change.
<!-- end git-appraise-conventions v4 -->
