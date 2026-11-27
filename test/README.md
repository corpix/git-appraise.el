# Test suite

`just test` runs the ERT suite in batch Emacs. Pass an ERT name or regexp as
the selector, for example `just test 'workflow/'`. The runner loads files in
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
