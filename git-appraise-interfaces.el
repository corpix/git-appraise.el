;;; git-appraise-interfaces.el --- Generic review backend interfaces -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Dmitry Moskowski
;;
;; Version: 0.0.7
;; Homepage: https://github.com/corpix/git-appraise.el
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Generic backend interfaces used by git-appraise.
;;
;;; Code:

(cl-defgeneric git-appraise-send-review (obj callback)
  "Send review stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric git-appraise-send-replies (obj callback)
  "Send review comment replies stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric git-appraise-send-title (obj callback)
  "Set a pullrequest title for an OBJ and call CALLBACK afterward.")

(cl-defgeneric git-appraise-send-description (obj callback)
  "Set a pullrequest description for an OBJ and call CALLBACK afterward.")

(cl-defgeneric git-appraise-get-assignable-users (obj)
  "Get users that can review a PR for OBJ.")

(cl-defgeneric git-appraise-get-mentionable-users (obj)
  "Get users that can be @-mentioned in a PR comment for OBJ.")

(cl-defmethod git-appraise-get-mentionable-users ((obj t))
  "Users @-mentionable for OBJ: the assignable users by default."
  (git-appraise-get-assignable-users obj))

(cl-defgeneric git-appraise-new-issue-comment (obj comment-msg callback)
  "Create a new comment issue for OBJ sending the COMMENT-MSG and call CALLBACK.")

(cl-defgeneric git-appraise-new-code-comment (obj local-comment callback)
  "Create a new diff comment for OBJ given a LOCAL-COMMENT and call CALLBACK.")

;; Editing submitted comments/reviews
(cl-defgeneric git-appraise-update-comment (obj kind comment-id body callback)
  "Update the BODY of an already-submitted comment in OBJ.
KIND is a provider-agnostic token selecting what COMMENT-ID
refers to: \"issue-comment\" (a conversation comment),
\"review-summary\" (a submitted review body) or
\"review-comment\" (a diff-anchored review comment, including
thread replies and outdated ones).  Call CALLBACK when the
provider API call completes.")

(cl-defmethod git-appraise-update-comment (obj kind comment-id body callback)
  "Fallback when provider comment editing is not implemented."
  (ignore obj kind comment-id body callback)
  (user-error "Editing submitted comments is not implemented for this provider"))

(provide 'git-appraise-interfaces)
;;; git-appraise-interfaces.el ends here
