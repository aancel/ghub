;;; ghub-graphql.el --- access Github API using GrapthQL  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Code:

(require 'ghub)
(require 'subr-x)
(require 'treepy)

;;; Api

(cl-defun ghub-graphql (graphql &optional variables
                                &key username auth host
                                silent
                                callback errorback value extra)
  "Make a GraphQL request using GRAPHQL and VARIABLES.
Return the response as a JSON-like alist.  Even if the response
contains `errors', do not raise an error.  GRAPHQL is a GraphQL
string.  VARIABLES is a JSON-like alist.  The other arguments
behave as for `ghub-request' (which see)."
  (cl-assert (stringp graphql))
  (cl-assert (not (stringp variables)))
  (ghub-request "POST" "/graphql" nil :payload
                (json-encode `(("query" . ,graphql)
                               ,@(and variables `(("variables" ,@variables)))))
                :silent silent
                :username username :auth auth :host host
                :callback callback :errorback errorback
                :extra extra :value value))

(cl-defun ghub-graphql-rate-limit (&key username auth host)
  "Return rate limit information."
  (let-alist (ghub-graphql
              "query { rateLimit { limit cost remaining resetAt }}"
              nil :username username :auth auth :host host)
    .data.rateLimit))

(cl-defun ghub-repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (let-alist (ghub-graphql
              "query ($owner:String!, $name:String!) {
                 repository(owner:$owner, name:$name) { id }
               }"
              `((owner . ,owner)
                (name  . ,name))
              :username username :auth auth :host host)
    .data.repository.id))

;;; Api (drafts)

(cl-defun ghub-fetch-repository (owner name callback
                                       &optional variables
                                       &key username auth host)
  "Asynchronously fetch forge data about the specified repository.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql
   '(repository)
   `((:repository . ,(ghub--file-contents "ghub-repository.graphql"))
     (:issue      . ,(ghub--file-contents "ghub-issue.graphql"))
     (:pullreq    . ,(ghub--file-contents "ghub-pullreq.graphql")))
   `((owner . ,owner)
     (name  . ,name))
   '((repository
      (:query             . :repository)
      (getFields          . t)
      (getCommitComments  . t)
      (getForks           . nil) ; FIXME
      (getIssues          . t)
      (getLabels          . t)
      (getPullRequests    . t))
     (commitComments
      (:query             . :repository)
      (afterCommitComment . CURSOR)
      (getFields          . nil)
      (getCommitComments  . t)
      (getForks           . nil)
      (getIssues          . nil)
      (getLabels          . nil)
      (getPullRequests    . nil))
     (forks
      (:query             . :repository)
      (afterFork          . CURSOR)
      (getFields          . nil)
      (getCommitComments  . nil)
      (getForks           . t)
      (getIssues          . nil)
      (getLabels          . nil)
      (getPullRequests    . nil))
     (issues
      (:query             . :repository)
      (afterIssue         . CURSOR)
      (getFields          . nil)
      (getCommitComments  . nil)
      (getForks           . nil)
      (getIssues          . t)
      (getLabels          . nil)
      (getPullRequests    . nil))
     (labels
      (:query             . :repository)
      (afterLabel         . CURSOR)
      (getFields          . nil)
      (getCommitComments  . nil)
      (getForks           . nil)
      (getIssues          . nil)
      (getLabels          . t)
      (getPullRequests    . nil))
     (pullRequests
      (:query             . :repository)
      (afterPullRequest   . CURSOR)
      (getFields          . nil)
      (getCommitComments  . nil)
      (getForks           . nil)
      (getIssues          . nil)
      (getLabels          . nil)
      (getPullRequests    . t))
     ((issues comments)
      (:query             . :issue)
      (number             . IDENT)
      (afterComment       . CURSOR)
      (getFields          . nil)
      (getComments        . t))
     ((pullRequests comments)
      (:query             . :pullreq)
      (number             . IDENT)
      (afterComment       . CURSOR)
      (getFields          . nil)
      (getComments        . t)))
   callback variables
   :username username :auth auth :host host))

(cl-defun ghub-fetch-issue (owner name number callback
                                  &optional variables
                                  &key username auth host)
  "Asynchronously fetch forge data about the specified issue.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql
   '(repository issue)
   `((:issue . ,(ghub--file-contents "ghub-issue.graphql")))
   `((owner  . ,owner)
     (name   . ,name)
     (number . ,number))
   '((issue
      (:query           . :issue)
      (getFields        . t)
      (getComments      . t))
     (comments
      (:query           . :issue)
      (getFields        . nil)
      (getComments      . t)
      (afterComment     . CURSOR)))
   callback variables
   :username username :auth auth :host host))

(cl-defun ghub-fetch-pullreq (owner name number callback
                                    &optional variables
                                    &key username auth host)
  "Asynchronously fetch forge data about the specified pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql
   '(repository pullRequest)
   `((:pullreq . ,(ghub--file-contents "ghub-pullreq.graphql")))
   `((owner  . ,owner)
     (name   . ,name)
     (number . ,number))
   '((pullRequest
      (:query           . :pullreq)
      (getFields        . t)
      (getComments      . t))
     (comments
      (:query           . :pullreq)
      (getFields        . nil)
      (getComments      . t)
      (afterComment     . CURSOR)))
   callback variables
   :username username :auth auth :host host))

;;; Experimental

(cl-defun ghub--graphql (path queries constants unpaginate callback variables
                              &key username auth host forge)
  "Use the Source, Luke." ; looking at the above callers first.
  (unless host
    (setq host (ghub--host forge)))
  (unless (or username (stringp auth) (eq auth 'none))
    (setq username (ghub--username host forge)))
  (setq constants (copy-sequence constants))
  (pcase-dolist (`(,k . ,v) variables)
    (setf (alist-get k constants) v))
  (ghub--graphql-retrieve
   (ghub--make-req
    :url        (url-generic-parse-url (concat "https://" host "/graphql"))
    :method     "POST"
    :headers    (ghub--headers nil host auth username forge)
    :handler    'ghub--graphql-handle-response
    :unpaginate (list queries constants unpaginate)
    :extra      `((pages . 0))
    :callback   (if path
                    (lambda (data)
                      (let ((path path) key)
                        (while (setq key (pop path))
                          (setq data (cdr (assq key data)))))
                      (funcall callback data))
                  callback))))

(cl-defun ghub--graphql-retrieve (req &optional key cursor)
  (magit-msg "Fetching page %s..."
             (cl-incf (alist-get 'pages (ghub--req-extra req))))
  (pcase-let* ((`(,queries ,constants ,unpaginate)
                (ghub--req-unpaginate req))
               (key (or key (caar unpaginate)))
               (vars (cdr (or (assq key unpaginate)
                              (assoc (list (thread-last (ghub--req-value req)
                                             treepy-up treepy-up
                                             treepy-node car)
                                           key)
                                     unpaginate))))
               (query (cdr (assq (cdr (assq :query vars)) queries)))
               (variables (cl-remove-if #'keywordp
                                        (copy-sequence constants)
                                        :key #'car)))
    (pcase-dolist (`(,k . ,v) vars)
      (unless (keywordp k)
        (setf (alist-get k variables)
              (pcase v
                ('CURSOR cursor)
                ('IDENT
                 (let ((loc (treepy-up (ghub--req-value req))))
                   (cdr (assq k (treepy-node loc)))))
                (_ v)))))
    (ghub--retrieve (let ((json-false nil))
                      (ghub--encode-payload
                       `((query     . ,query)
                         (variables . ,variables))))
                    req)))

(defun ghub--graphql-handle-response (status req)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((headers (ghub--handle-response-headers status req))
                 (payload (ghub--handle-response-payload req))
                 (payload (ghub--handle-response-error status payload req))
                 (err     (plist-get status :error))
                 (errors  (cdr (assq 'errors payload)))
                 (errors  (and errors
                               (cons 'ghub-graphql-error errors)))
                 (data    (assq 'data payload))
                 (value   (ghub--req-value req)))
            (setf (ghub--req-value req) value)
            (if (or err errors)
                (if-let ((errorback (ghub--req-errorback req)))
                    (funcall errorback (or err errors) headers status req)
                  (ghub--signal-error (or err errors)))
              (ghub--graphql-walk-response value data req))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--graphql-walk-response (loc data req)
  (if (not loc)
      (setf (ghub--req-value req)
            (setq loc (ghub--alist-zip data)))
    (setq data (ghub--graphql-unwind loc data))
    (setf (alist-get 'edges data)
          (append (alist-get 'edges (treepy-node loc))
                  (or (alist-get 'edges data)
                      (error "BUG: Expected new nodes"))))
    (setq loc (treepy-replace loc data)))
  (cl-block nil
    (while t
      (when (eq (car-safe (treepy-node loc)) 'edges)
        (setq loc (treepy-up loc))
        (pcase-let ((`(,key . ,val) (treepy-node loc)))
          (let-alist val
            (let* ((cursor (and .pageInfo.hasNextPage
                                .pageInfo.endCursor))
                   (until (cdr (assq (intern (format ":%s-since" key))
                                     (cadr (ghub--req-unpaginate req)))))
                   (all (mapcar #'cdar .edges))
                   (new (if until
                            (--take-while
                             (or (string> (cdr (assq 'updatedAt it)) until)
                                 (setq cursor nil))
                             all)
                          all)))
              (if cursor
                  (progn
                    (setf (ghub--req-value req) loc)
                    (ghub--graphql-retrieve req key cursor)
                    (cl-return))
                (setq loc (treepy-replace loc (cons key new))))))))
      (if (not (treepy-end-p loc))
          (setq loc (treepy-next loc))
        (funcall (ghub--req-callback req)
                 (treepy-root loc))
        (cl-return)))))

(defun ghub--graphql-lineage (loc)
  (let (lineage)
    (while (treepy-up loc)
      (push (car (treepy-node loc)) lineage)
      (setq loc (treepy-up loc)))
    lineage))

(defun ghub--graphql-unwind (loc data)
  (let ((lineage (ghub--graphql-lineage loc))
        key)
    (while (setq key (pop lineage))
      (if (consp (car lineage))
          (progn (pop lineage)
                 (setf data (cadr data)))
        (setq data (assq key (cdr data)))))
    data))

(defun ghub--file-contents (file)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      file (file-name-directory (locate-library "ghub"))))
    (buffer-string)))

(defun ghub--alist-zip (root)
  (let ((branchp (lambda (elt) (and (listp elt) (listp (cdr elt)))))
        (make-node (lambda (_ children) children)))
    (treepy-zipper branchp #'identity make-node root)))

;;; _
(provide 'ghub-graphql)
;;; ghub-graphql.el ends here
