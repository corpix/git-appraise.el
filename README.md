[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)

# Git Appraise

An Emacs interface for reviewing [git-appraise](https://github.com/google/git-appraise)
requests in a Git repository. Reviews, comments, and decisions are stored by
the `git-appraise` CLI in Git notes. The Emacs package renders the review with
Magit sections and provides commands to create requests and respond to them.

This package supports **local git and git-appraise reviews only**. No forge
or remote review backends.

## Requirements and installation

- Emacs 28.1 or newer, Git, and the `git-appraise` executable on `PATH`.
- The Emacs packages listed in `git-appraise.el` (`Package-Requires`).

On Emacs versions with `use-package :vc` support, install from this repository
with:

```emacs-lisp
(use-package git-appraise
  :vc (:url "https://github.com/corpix/git-appraise.el" :rev :newest))
```

From a buffer in the repository you want to review, run
`M-x git-appraise-start` to choose an open or completed review. If there are
none, Emacs offers to create one. You can also run
`M-x git-appraise-backend-request` to create a request directly. Its prompts
cover the source branch, target branch, message, and optional reviewers. An
empty message uses the reviewed commit's message. In Magit status or revision,
press `R`, then `o` to open a review or `n` to create one.

See [Local git-appraise reviews](docs/git-appraise.md) for the CLI setup and a
full local workflow.

## Reviewing

The review buffer shows the request, commits, conversation, and file diffs.
Existing inline comments appear next to their diff lines; comments on an
older commit or outside the current diff appear in Conversation. File headings
show change counts and tags such as `[DOC]` or `[GEN]`. Documentation,
lockfiles, and generated output are classified and folded by default. Files
marked `linguist-generated` or `linguist-vendored` in `.gitattributes` are
excluded from the rendered diff by default; the Files changed heading reports
the hidden count. Use `r x` to toggle that exclusion for the current buffer.
Comment and reply headings show the author, status, and local time. They show
only the time for today, add the day and month for older comments this year,
and add the year for comments from another year.

Press `r` for the command menu. The main actions are:

| Where | Key | Action |
| --- | --- | --- |
| Diff line | `RET` | Draft an inline comment |
| Diff hunk | `C-c RET` | Send one inline comment immediately |
| File heading | `RET` | Send a file comment |
| Existing comment | `RET` | Reply |
| Draft comment | `RET` | Edit the draft |
| Draft comment | `C-c C-k` | Delete the draft |
| Comment editor | `C-c C-c` | Save or send the comment |
| Comment editor | `C-c C-k` | Cancel editing |
| Review buffer | `C-c C-n` / `C-c C-p` | Move between diff hunks |
| Review buffer | `n` / `p` | Move between visible files and hunks, one heading at a time |
| Review buffer | `v` | Toggle the viewed mark on the hunk at point, or the file on a file heading |
| Review buffer | `V` | Toggle the viewed mark on the whole file |
| Review buffer | `G` | Reload the review |
| Review buffer | `r a` / `r r` / `r c` | Accept, reject, or submit comments |
| Review buffer | `r l` | Accept with the configured LGTM message |
| Review buffer | `r s c` | Send a general comment |
| Review buffer | `r A` | Abandon the current review |
| Review buffer | `r b` | Rebase the current review onto its target |
| Review buffer | `r p` / `r P` | Push or pull git-appraise notes |

Drafted inline comments survive a reload while the review head is unchanged.
If the head changed, reload asks before discarding them. A rejection needs
feedback; the command prompts for it when needed. The CLI cannot place a new
inline comment on a removed line because that line is absent from the review
head file. Use a file or general comment for that case.

Viewed marks record your review progress: `v` and `V` mark hunks and files
viewed, the Files changed heading tracks `N/M viewed`, and a `[CHANGED]` tag
flags files that were viewed before but changed since. Marks are content
addressed (a line shift keeps a mark, an edit resets it) and live in your
local git notes under `refs/notes/git-appraise-el/viewed`; they are never
published by `git-appraise push`. See `docs/viewed-state.md`.

## Configuration

```emacs-lisp
;; Open the review buffer in the current window instead of another window.
(setq git-appraise-new-buffer-window-strategy #'switch-to-buffer)

;; Set the width used to wrap displayed comments.
(setq git-appraise-fill-column 80)

;; Inline comments have thin top and bottom rules by default.
;; Use nil for background-only comments, or choose a custom rule color.
(setq git-appraise-inline-comment-border-style 'horizontal)
(setq git-appraise-inline-comment-border-color nil)

;; Keep files marked generated or vendored by .gitattributes visible.
(setq git-appraise-exclude-gitattributes-generated nil)

;; Stop folding hunks when a review opens.
(setq git-appraise-fold-hunks-by-default nil)

;; Folded hunks with comments show a marker. Customize its text or face.
(setq git-appraise-commented-hunk-indicator "  [comment]")
;; M-x customize-face RET git-appraise-commented-hunk-face RET
```

The `git-appraise-diff-file-order-rules` and
`git-appraise-diff-noise-rules` options control file order, tags, and automatic
folding. Use `M-x customize-group RET git-appraise RET` for the other options.
The background colors for your comments, the request author's comments, and
other reviewers' comments are customizable through the
`git-appraise-comment-self-bg`, `git-appraise-comment-author-bg`, and
`git-appraise-comment-other-bg` faces.

The backend invokes the local `git-appraise` CLI. Use `git-appraise-push-notes`
/ `git-appraise-pull-notes` (or the `R`/`r` manage menus) to sync git-appraise
notes with a remote. Viewed marks live in separate local notes and are not
pushed by `git-appraise push`.

## Development

Run `just compile` to byte-compile the package and `just test` for its ERT
suite. `just deps` installs Emacs dependencies on a fresh machine.
