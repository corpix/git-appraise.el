;;; git-appraise-file-types.el --- Customizable review file types -*- lexical-binding: t; -*-

(require 'dash)
(require 'subr-x)
(require 'git-appraise-faces)

(defcustom git-appraise-file-types
  '((generated :paths ("\\`\\(package-lock\\.json\\|yarn\\.lock\\|pnpm-lock\\.yaml\\|Cargo\\.lock\\|flake\\.lock\\|poetry\\.lock\\|Gemfile\\.lock\\|composer\\.lock\\|mix\\.lock\\|Pipfile\\.lock\\|packages\\.lock\\.json\\)\\'"
                        "\\.lock\\'"
                        "\\`\\(dist\\|build\\|out\\)/\\|\\.min\\.[cm]?js\\'\\|\\.bundle\\.[cm]?js\\'\\|\\.map\\'")
      :attribute :generated :label "GEN"
      :face git-appraise-file-type-generated-face
      :collapse t :hide t :order 100)
    (vendored :paths ("\\`\\(vendor\\|third_party\\)/")
      :attribute :vendored :label "VEND"
      :face git-appraise-file-type-vendored-face
      :collapse t :hide t :order 90)
    (spec :paths ("\\`\\(spec\\|specs\\)/" "[_\\.]spec\\.[^/]+\\'")
      :label "SPEC" :face git-appraise-file-type-spec-face
      :collapse nil :hide nil :order 20)
    (test :paths ("\\`\\(test\\|tests\\)/" "[_\\.]test\\.[^/]+\\'")
      :label "TEST" :face git-appraise-file-type-test-face
      :collapse nil :hide nil :order 30)
    (documentation :paths ("\\`\\(CHANGELOG\\|CHANGES\\|NEWS\\|HISTORY\\|AUTHORS\\|CREDITS\\)\\(\\.[^/]*\\)?\\'\\|/\\(doc\\|docs\\|documentation\\)/\\|\\.\\(md\\|markdown\\|rst\\|org\\)\\'\\|\\(^\\|/\\)README\\(\\.[^/]*\\)?\\'")
      :attribute :documentation :label "DOC"
      :face git-appraise-file-type-documentation-face
      :collapse t :hide t :order 80)
    (whitespace :label "WS-ONLY"
      :face git-appraise-file-type-whitespace-face
      :collapse t :hide t :order 70))
  "Ordered file types used in review diffs.
Each entry is (TYPE :paths REGEXPS :attribute KEY :languages NAMES
:detectable STATE :label STRING :face FACE :collapse BOOL :hide BOOL
:order INTEGER).  TYPE is stable; changing :label does not change
classification behavior.  :attribute KEY matches a set Linguist
attribute and an explicit unset suppresses that type's path rules.
:languages matches the raw `linguist-language' value without case.
:detectable matches set or unset.  Attribute matches outrank path
and language matches; the first matching entry wins within a phase.
The whitespace type is assigned by diff analysis, not by a path."
  :group 'git-appraise
  :type 'sexp)

(defcustom git-appraise-file-tag-min-width 4
  "Minimum width of the file-type label column."
  :group 'git-appraise
  :type 'natnum)

(defconst git-appraise-file-types--linguist-attrs
  '(("linguist-generated" . :generated)
    ("linguist-vendored" . :vendored)
    ("linguist-documentation" . :documentation)
    ("linguist-language" . :language)
    ("linguist-detectable" . :detectable)))

(defun git-appraise-file-types--style (type)
  "Return TYPE's configured plist, or nil."
  (cdr (assq type git-appraise-file-types)))

(defun git-appraise-file-types--label (type)
  "Return TYPE's visible label, or nil."
  (plist-get (git-appraise-file-types--style type) :label))

(defun git-appraise-file-types--tag-width ()
  "Return the label-column width for configured file types."
  (max git-appraise-file-tag-min-width
       (--reduce-from (max acc (length (or (plist-get (cdr it) :label) "")))
                      0 git-appraise-file-types)))

(defun git-appraise-file-types--attribute-value (value)
  "Normalize Git attribute VALUE while preserving explicit unset."
  (pcase value
    ((or "set" "true" "1") 'set)
    ((or "unset" "false" "0") 'unset)
    ("unspecified" 'unspecified)
    (_ value)))

(defun git-appraise-file-types--attributes (paths repo &optional source)
  "Return PATHS' five Linguist attributes from REPO in one Git call.
SOURCE, when non-nil, is the reviewed commit whose .gitattributes
should be used.  Paths travel on stdin and output is NUL-delimited."
  (let ((table (make-hash-table :test #'equal)))
    (when (and paths repo (file-directory-p repo))
      (let ((default-directory (file-name-as-directory repo))
            (output (generate-new-buffer " *git-appraise-attributes*")))
        (unwind-protect
            (with-temp-buffer
              (--each paths (insert it "\0"))
              (let ((status
                     (apply #'call-process-region
                            (point-min) (point-max) "git" nil output nil
                            (append '("check-attr" "--stdin" "-z")
                                    (when source
                                      (list (concat "--source=" source)))
                                    (mapcar #'car git-appraise-file-types--linguist-attrs)))))
                (when (and (integerp status) (zerop status))
                  (with-current-buffer output
                    (let ((parts (split-string (buffer-string) "\0" t)))
                      (while parts
                        (let* ((path (pop parts))
                               (attr (pop parts))
                               (value (pop parts))
                               (key (cdr (assoc attr
                                                git-appraise-file-types--linguist-attrs))))
                          (when (and key path value)
                            (puthash path
                                     (plist-put (gethash path table)
                                                key
                                                (git-appraise-file-types--attribute-value
                                                 value))
                                     table)))))))))
          (kill-buffer output))))
    table))

(defun git-appraise-file-types--match-p (entry path attrs phase)
  "Whether ENTRY matches PATH and ATTRS in PHASE.
PHASE is `attribute' or `path'.  Explicit unset suppresses the
entry even when its path matches."
  (let* ((spec (cdr entry))
         (attribute (plist-get spec :attribute))
         (value (and attribute (plist-get attrs attribute)))
         (detectable (plist-get spec :detectable)))
    (and (not (eq value 'unset))
         (or (null detectable)
             (eq detectable (plist-get attrs :detectable)))
         (pcase phase
           ('attribute (eq value 'set))
           ('path (or (--any? (string-match-p it path)
                              (plist-get spec :paths))
                      (let ((language (plist-get attrs :language)))
                        (and (stringp language)
                             (--any? (string-equal-ignore-case it language)
                                     (plist-get spec :languages))))))))))

(defun git-appraise-file-types--classify (path &optional attrs)
  "Return PATH's configured file-type info using Linguist ATTRS."
  (let* ((entry (or (--first (git-appraise-file-types--match-p
                              it path attrs 'attribute)
                            git-appraise-file-types)
                    (--first (git-appraise-file-types--match-p
                              it path attrs 'path)
                            git-appraise-file-types)))
         (type (car-safe entry)))
    (git-appraise-file-types--info type attrs)))

(defun git-appraise-file-types--info (type &optional attrs)
  "Return configured display and behavior info for TYPE and ATTRS."
  (let ((style (git-appraise-file-types--style type)))
    (list :type type :tag (plist-get style :label)
          :face (or (plist-get style :face) 'git-appraise-diff-tag-face)
          :collapse (plist-get style :collapse)
          :hide (plist-get style :hide)
          :order (or (plist-get style :order) 0)
          :language (let ((value (plist-get attrs :language)))
                      (and (stringp value) value))
          :detectable (plist-get attrs :detectable))))

(provide 'git-appraise-file-types)
;;; git-appraise-file-types.el ends here
