# Review workflow and magit-hutch comparison

This package keeps git-appraise as the published review record. A review
follows a source branch as new commits are added. Its comments and replies
are append-only Git-note events. A reply with `-lgtm` can resolve a finding;
unsent drafts and personal progress stay in the local SQLite store.

The local `magit-hutch` checkout solves a different part of review. It uses
gptel to inspect staged, unpushed or branch changes, fetches code through
bounded tools, and can suggest patches. Its cache keys results by a hash of
the full diff, and its staged-review promotion checks the same content hash
at commit time. These are useful patterns for keeping review results tied to
exact code. Its agent interaction and patch application are outside this
package's current workflow.

The current package now uses a separate content comparison for its personal
"changed since viewed" baseline. The split between published review events
and local progress is intentional: `git-appraise push` shares conversation
and decisions, while local SQLite holds folds, drafts, marks and diff
snapshots. No magit-hutch refs, cache files or gptel dependency are needed.

An agent action launched from the review buffer remains a future feature.
It would need to receive the current review's base, head, diff and comment
context, and return a response without silently publishing comments or
changing git-appraise notes. The current work leaves that interface open.
