# Test suite

`just test` runs the ERT suite in batch Emacs. Pass an ERT name or regexp as
the selector, for example `just test 'review-journey/'`. The runner loads files in
the order `unit/`, `integration/`, `workflow/`, with names sorted inside each
directory.

| Layer | What it checks |
| --- | --- |
| `unit/` | Pure projections, formatting, store rows, and process behavior. |
| `integration/` | A feature with real Git or git-appraise, rendering, or SQLite. |
| `workflow/` | A sequence of review actions through commands or keys, checked in the buffer and backing state. |
| `support/` | Disposable repositories, shared fixtures, and render helpers; no ERT tests. |

Workflow tests cover review creation and selection, comment drafting and
publication, decisions and landing, viewed and fold progress, synchronization,
and concurrent review context. Prefer a real command or key in these tests;
stub prompts when batch Emacs cannot answer them. Keep direct calls to internal
helpers in unit and integration tests.

Every test that represents a user-visible requirement keeps its stable
`req-YYYY-MM-DD-NNN` ID, EARS definition in the first docstring line, and
`covers:` metadata above its primary guard. Test files have no `provide` form.
Temporary repositories and file-backed SQLite databases are deleted by their
fixtures. The suite never opens the user's review store.

## Bulk viewed performance

`just bench` creates a disposable 200-file review, selects the file headings,
and invokes the review buffer's `v` binding to mark and unmark them. It reports
elapsed time for each action and call counts/timings for its main phases. Fixture
creation and rendering are excluded from the action timings. The fixture uses an
in-memory SQLite store and never contacts the live Emacs daemon.

`just profile` runs the same case with Emacs's CPU and memory profilers. Expanded
text reports and loadable `.prof` files are written to ignored
`tmp/performance/`. Compare benchmark results on the same machine before and
after a change. The regular ERT suite checks bounded full-review work and
correct state for 200 files; elapsed time is diagnostic, not a CI threshold.

## Large review submission

`just bench-submit` times a needs-work submission on a disposable 200-file
review with a historical finding. It reports the metadata refresh and section
update phases, so an accidental full diff reload is visible in the call counts.
`just profile-submit` runs the same flow with CPU and memory profiling and
writes `submit-*.txt` and `submit-*.prof` to ignored `tmp/performance/`.
The workflow tests also cover needs-work feedback, resolving findings, approval,
and cursor location after each action.
