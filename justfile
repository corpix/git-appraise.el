set shell := ["bash", "-cu"]
set lazy := true

emacs := env_var_or_default('EMACS', 'emacs')
pkg_dir := env_var_or_default('PKG_DIR', env_var('HOME') + '/.emacs.d/elpa')
load_path := `printf -- '-L . -L test'; for d in '{{pkg_dir}}'/*; do [ -d "$d" ] && printf ' -L %s' "$d" || true; done`

# Install package dependencies from MELPA.
deps:
    {{emacs}} -Q --batch \
      --eval "(require 'package)" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
      --eval "(package-initialize)" \
      --eval "(package-refresh-contents)" \
      --eval "(dolist (p (quote (a magit transient uuidgen deferred markdown-mode emojify s dash))) \
               (unless (package-installed-p p) (package-install p)))" \
      --eval "(message \"deps ok\")"

# Byte-compile all package files.
compile:
    #!/usr/bin/env bash
    set -euo pipefail
    # Macro-defining files (git-appraise-section.el defines
    # git-appraise--stage-time, used by git-appraise-local.el;
    # git-appraise-actions.el defines git-appraise-set-obsolete-fns)
    # come FIRST: the loop is alphabetical, and a file compiled
    # before the section that defines a macro it calls ships the
    # macro call UNEXPANDED — (invalid-function ...) at render time.
    {{emacs}} -Q --batch {{load_path}} \
      --eval "(byte-compile-file \"git-appraise.el\")" \
      --eval "(byte-compile-file \"git-appraise-section.el\")" \
      --eval "(byte-compile-file \"git-appraise-actions.el\")" \
      --eval "(dolist (f (directory-files \".\" nil \"^git-appraise-.*\\\\.el\\\\'\")) \
               (unless (member f (quote (\"git-appraise-autoloads.el\" \"git-appraise-pkg.el\")) ) \
                 (byte-compile-file f)))"

# Run the ERT test suite in batch mode. Optional selector: just test pr-state/
test selector="t":
    ERT_SELECTOR='{{selector}}' {{emacs}} -Q --batch {{load_path}} -l test/run-tests.el

# Compile then test.
check: compile test

# Remove build artifacts.
clean:
    rm -f *.elc test/*.elc
