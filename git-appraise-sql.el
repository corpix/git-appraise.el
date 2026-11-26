;;; git-appraise-sql.el --- Query builder for the local store -*- lexical-binding: t; -*-

;;; Commentary:
;; A small compositional builder for the parameterized SQL the
;; review store needs.  Every statement compiles to (SQL . PARAMS)
;; for the native `sqlite-select'/`sqlite-execute' API: identifiers
;; are validated and quoted, values are never interpolated -- they
;; bind through ? placeholders in order.  Where clauses are prefix
;; forms:
;;
;;   (:and CLAUSE...) (:or CLAUSE...) (:not CLAUSE)
;;   (:= COLUMN VALUE)          ; a nil VALUE compiles to IS NULL
;;   (:in COLUMN (VALUE...)) (:is-null COLUMN)
;;
;; A COLUMN is a keyword or symbol matching [a-z_][a-z0-9_]*;
;; anything else is refused, so table and column names are always
;; safe to splice.  Equality is written (= COLUMN VALUE) or
;; (:= COLUMN VALUE); a nil VALUE compiles to IS NULL.

;;; Code:

(require 'dash)

(defun git-appraise-sql--id (name)
  "Validate and quote the SQL identifier NAME (keyword or symbol)."
  (let ((name (if (keywordp name)
                  (substring (symbol-name name) 1)
                (symbol-name name))))
    (unless (string-match-p "\\`[a-z_][a-z0-9_]*\\'" name)
      (error "Unsafe SQL identifier: %S" name))
    (concat "\"" name "\"")))

(defun git-appraise-sql--ids (names)
  "Quote every identifier in NAMES, comma separated."
  (string-join (-map #'git-appraise-sql--id names) ", "))

(defun git-appraise-sql--columns (columns)
  "Compile COLUMNS: `*' or a list of identifiers."
  (if (eq columns '*) "*" (git-appraise-sql--ids columns)))

(defun git-appraise-sql--order (order-by)
  "Compile ORDER-BY entries (COLUMN [DIRECTION]) into SQL.
DIRECTION is `asc' or `desc', defaulting to `asc'."
  (string-join
   (--map (let ((column (if (consp it) (car it) it))
                (direction (if (consp it) (cadr it) 'asc)))
            (unless (memq direction '(asc desc))
              (error "Bad ORDER BY direction: %S" direction))
            (format "%s %s"
                    (git-appraise-sql--id column)
                    (upcase (symbol-name direction))))
          order-by)
   ", "))

(defun git-appraise-sql--where (form params)
  "Compile WHERE form FORM, appending bound values to PARAMS.
Return (SQL . PARAMS); a nil FORM compiles to the empty string."
  (cond
   ((null form) (cons "" params))
   ((not (consp form)) (error "Bad SQL where form: %S" form))
   ((memq (car form) '(:and :or))
    (unless (cdr form) (error "%S needs at least one clause" (car form)))
    (let ((parts nil))
      (dolist (clause (cdr form))
        (pcase-let ((`(,sql . ,next) (git-appraise-sql--where clause params)))
          (push sql parts)
          (setq params next)))
      (cons (format "(%s)"
                    (string-join (nreverse parts)
                                 (if (eq (car form) :and) " AND " " OR ")))
            params)))
   ((memq (car form) '(:= =))
    (let ((column (cadr form))
          (value (caddr form)))
      (if value
          (cons (concat (git-appraise-sql--id column) " = ?")
                (append params (list value)))
        (git-appraise-sql--where (list :is-null column) params))))
   ((eq (car form) :in)
    (let ((values (caddr form)))
      (unless (and values (listp values))
        (error ":in needs a non-empty list of values"))
      (cons (format "%s IN (%s)"
                    (git-appraise-sql--id (cadr form))
                    (string-join (make-list (length values) "?") ", "))
            (append params values))))
   ((eq (car form) :is-null)
    (cons (concat (git-appraise-sql--id (cadr form)) " IS NULL") params))
   ((eq (car form) :not)
    (pcase-let ((`(,sql . ,next) (git-appraise-sql--where (cadr form) params)))
      (cons (concat "NOT " sql) next)))
   (t (error "Unknown SQL where operator: %S" (car form)))))

(defun git-appraise-sql-select (&rest spec)
  "Compile a SELECT from SPEC.  Return (SQL . PARAMS).
Keys: :from TABLE, :columns (COLUMN... or *), :where FORM,
:order-by ((COLUMN [asc|desc])...) and :limit N.  With :count
non-nil the statement becomes SELECT count(*) and :columns is
ignored."
  (let* ((table (plist-get spec :from))
         (count (plist-get spec :count))
         (columns (if count "count(*)"
                    (git-appraise-sql--columns
                     (or (plist-get spec :columns) '*))))
         (order-by (plist-get spec :order-by))
         (limit (plist-get spec :limit)))
    (unless table (error ":from is required"))
    (pcase-let* ((`(,where-sql . ,params) (git-appraise-sql--where
                                           (plist-get spec :where) nil)))
      (cons (concat "SELECT " columns
                    " FROM " (git-appraise-sql--id table)
                    (if (equal where-sql "") "" (concat " WHERE " where-sql))
                    (if order-by
                        (concat " ORDER BY " (git-appraise-sql--order order-by))
                      "")
                    (if limit (format " LIMIT %d" limit) ""))
            params))))

(defun git-appraise-sql-insert (&rest spec)
  "Compile an INSERT from SPEC.  Return (SQL . PARAMS).
Keys: :into TABLE, :columns (COLUMN...), :rows (ROW...) with one
value per column in each ROW, and :on-conflict `ignore',
`replace' or nil (plain insert)."
  (let* ((table (plist-get spec :into))
         (columns (plist-get spec :columns))
         (rows (plist-get spec :rows))
         (conflict (plist-get spec :on-conflict))
         (width (length columns)))
    (unless (and table columns rows)
      (error ":into, :columns and :rows are required"))
    (unless (memq conflict '(nil :ignore :replace))
      (error "Bad :on-conflict: %S" conflict))
    (dolist (row rows)
      (unless (= (length row) width)
        (error "Row width %d does not match %d columns" (length row) width)))
    (cons (concat "INSERT "
                  (pcase conflict
                    (:ignore "OR IGNORE ")
                    (:replace "OR REPLACE ")
                    (_ ""))
                  "INTO " (git-appraise-sql--id table)
                  " (" (git-appraise-sql--ids columns) ") VALUES "
                  (string-join (--map (format "(%s)"
                                              (string-join
                                               (make-list width "?") ", "))
                                      rows)
                               ", "))
            (-flatten-n 1 rows))))

(defun git-appraise-sql--column (spec)
  "Compile one column spec (NAME TYPE &optional NOT-NULL),
e.g. (repo :text :not-null) or (range :text)."
  (let ((name (nth 0 spec))
        (type (nth 1 spec))
        (not-null (nth 2 spec)))
    (unless (and (symbolp name) (memq type '(:text :integer)))
      (error "Bad column spec: %S" spec))
    (concat (git-appraise-sql--id name) " "
            (upcase (if (keywordp type)
                        (substring (symbol-name type) 1)
                      (symbol-name type)))
            (if not-null " NOT NULL" ""))))

(defun git-appraise-sql-create-table (&rest spec)
  "Compile a CREATE TABLE from SPEC.  Return (SQL . PARAMS).
Keys: :name TABLE, :columns ((NAME :type :text|:integer [:not-null t])...),
:primary-key (COLUMN...) and :without-rowid non-nil."
  (let* ((table (plist-get spec :name))
         (columns (plist-get spec :columns))
         (primary-key (plist-get spec :primary-key))
         (without-rowid (plist-get spec :without-rowid)))
    (unless (and table columns) (error ":name and :columns are required"))
    (cons (concat "CREATE TABLE " (git-appraise-sql--id table) " ("
                  (string-join (-map #'git-appraise-sql--column columns) ", ")
                  (if primary-key
                      (concat ", PRIMARY KEY ("
                              (git-appraise-sql--ids primary-key) ")")
                    "")
                  (if without-rowid ") WITHOUT ROWID" ")"))
            nil)))

(defun git-appraise-sql-rename-table (from to)
  "Compile an ALTER TABLE RENAME from FROM to TO.  Return (SQL . PARAMS)."
  (cons (concat "ALTER TABLE " (git-appraise-sql--id from)
                " RENAME TO " (git-appraise-sql--id to))
        nil))

(defun git-appraise-sql-drop-table (name)
  "Compile a DROP TABLE for NAME.  Return (SQL . PARAMS)."
  (cons (concat "DROP TABLE " (git-appraise-sql--id name)) nil))

(defun git-appraise-sql-pragma (name &optional value)
  "Compile a PRAGMA statement for NAME.
With VALUE (a number), compile an assignment; without, compile a
read.  Return (SQL . PARAMS)."
  (let ((name (if (keywordp name)
                  (substring (symbol-name name) 1)
                (symbol-name name))))
    (unless (string-match-p "\\`[a-z][a-z0-9_-]*\\'" name)
      (error "Unsafe pragma name: %S" name))
    (if value
        (cons (format "PRAGMA \"%s\" = %s" name value) nil)
      (cons (format "PRAGMA \"%s\"" name) nil))))

(defun git-appraise-sql-delete (&rest spec)
  "Compile a DELETE from SPEC: :from TABLE and :where FORM.
An unconditional delete is refused."
  (let* ((table (plist-get spec :from))
         (where (plist-get spec :where)))
    (unless table (error ":from is required"))
    (pcase-let* ((`(,where-sql . ,params) (git-appraise-sql--where where nil)))
      (when (equal where-sql "")
        (error "A DELETE without a where clause is refused"))
      (cons (concat "DELETE FROM " (git-appraise-sql--id table)
                    " WHERE " where-sql)
            params))))

(provide 'git-appraise-sql)
;;; git-appraise-sql.el ends here
