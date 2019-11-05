;;; bitbucket-server.el --- Bitbucket server open pull-request from branch  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Mihai

;; Author: Mihai <mihdin@work>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A very simple package for openning a new pull request from the current branch
;; when using a self managed Bitbucket Server git solution.
;; The main entry-point is `M-x bitbucket-server-open'. To refrain from
;; being asked what the Bitbucket Server URL is, set the `bitbucket-server-url'
;; variable to a string with your server's url.

;;; Code:
(require 'magit)
(require 'projectile)

(defvar bitbucket-server-url nil
  "The bitbucket server url to use for the bitbucket-server package.
The format is: `http(s)://<bitbucket-server-url>/'")

(defun bitbucket-server--ensure-url-is-set ()
  "Check if the $bitbucket-server-url is set or not."
  (unless bitbucket-server-url
    (setq bitbucket-server-url (read-from-minibuffer "Bitbucket server URL: ")))
  )

(defun bitbucket-server--get-git-remote ()
  "Get git remotes."
  (with-current-buffer (get-buffer-create "*git-remote*")
    (erase-buffer)
    (call-process "git" nil "*git-remote*" nil "remote" "-v")
    (buffer-string)))

(defun bitbucket-server--refs-heads (branch)
  "Return the BRANCH in the refs/heads/BRANCH format."
  (format "refs/heads/%s" branch))

(defun bitbucket-server--get-project-and-repo ()
  "Get Bitbucket project and repository.
The project and repository is calculated from the output
of the `bitbucket-server--get-git-remote' function."
  ;; FIXME Update the regex to support http remotes
  (let ((git-remotes (bitbucket-server--get-git-remote))
        (regex (rx (one-or-more anything)
                   "ssh://"
                   (one-or-more anything)
                   "/"
                   (group (one-or-more anything))
                   "/"
                   (group (one-or-more anything))
                   ".git"
                   space
                   "(push)")))
    (when (string-match regex git-remotes)
      (list (cons 'project (match-string 1 git-remotes))
            (cons 'repo (match-string 2 git-remotes))))))

(defun bitbucket-server--url-with-project-and-repo ()
  "Return the Bitbucket server url with the project and repo added at the end."
  (let* ((project-repo-alist (bitbucket-server--get-project-and-repo))
         (project (cdr(assoc 'project project-repo-alist)))
         (repo (cdr (assoc 'repo project-repo-alist)))
         (bitbucket-link (format "%s/projects/%s/repos/%s"
                                 bitbucket-server-url
                                 project
                                 repo)))
    bitbucket-link))

(defun bitbucket-server--get-file-path-from-project-root ()
  "Get file path of the current buffer, relative to the project root."
  (file-relative-name (buffer-file-name) (projectile-project-root)))

(defun bitbucket-server--query-params-to-string (query-params)
  "Transform a QUERY-PARAMS association list to a string.
The output will be similar to \"key1=value1&key2=value2\"."
  (string-join (cl-loop for (key . value) in query-params
                        collect (format "%s=%s" key (url-hexify-string value)))
               "&"))

(defun bitbucket-server--build-url (url-path query-params &optional extra-args)
  "Generic function for building the Bitbucket server URL.
It concatenates the `bitbucket-server-url' variable to
the URL-PATH and the url encoded QUERY-PARAMS. In case
EXTRA-ARGS are passed then those will be added to the end
of the resulting URL.

Function parameter types are described below.
URL-PATH: string
QUERY-PARAMS: association list
extra-args: string

Example usage:
\(bitbucket-server--build-url \"compare/commits\" '((sourceBranch . featureX) (tragetBranch . master)))

Result:
\"https://<bitbucket-server-host>/compare/commits?sourceBranch=refs%2Lheads%2LfeatureX&targetBranch=refs%2Lheads%2Lmaster\""
  (let* ((bitbucket-partial-url (bitbucket-server--url-with-project-and-repo))
         (bitbucket-final-url (format "%s/%s?%s"
                                      bitbucket-partial-url
                                      url-path
                                      (bitbucket-server--query-params-to-string query-params))))
    (if extra-args
        (format "%s%s" bitbucket-final-url extra-args)
      bitbucket-final-url)))


(defun bitbucket-server-open-pr (target-branch)
  "Open a browser to your Bitbucket server pull request creation page.
Uses TARGET-BRANCH as the starting point of the branch diff.

Prompts the user to set the Bitbucket server URL if not already set."
  (interactive
   (list (read-from-minibuffer "Bitbucket PR target branch: " "master")))
  (bitbucket-server--ensure-url-is-set)
  (let ((bitbucket-server-url (bitbucket-server--build-url "compare/commits"
                                                           (list (cons 'sourceBranch (bitbucket-server--refs-heads (magit-get-current-branch)))
                                                                 (cons 'targetBranch (bitbucket-server--refs-heads target-branch))))))
    (browse-url bitbucket-server-url)))

(defun bitbucket-server-open-file ()
  "Open a browser to your file in Bitbucket server."
  (interactive)
  (bitbucket-server--ensure-url-is-set)
  (let* ((file-path-in-project (bitbucket-server--get-file-path-from-project-root))
         (bitbucket-file-url (bitbucket-server--build-url (format "browse/%s" file-path-in-project)
                                                          (list (cons 'at (bitbucket-server--refs-heads (magit-get-current-branch)))))))

    (browse-url bitbucket-file-url)))

(defun bitbucket-server-open-file-at-point ()
  "Open a browser to your file in Bitbucket server.
Will open the file at the current line number."
  (interactive)
  (bitbucket-server--ensure-url-is-set)
  (let* ((file-path-in-project (bitbucket-server--get-file-path-from-project-root))
         (bitbucket-file-url (bitbucket-server--build-url (format "browse/%s" file-path-in-project)
                                                          (list (cons 'at (bitbucket-server--refs-heads (magit-get-current-branch))))
                                                          (format "#%s" (line-number-at-pos)))))

    (browse-url bitbucket-file-url)))

(provide 'bitbucket-server)
;;; bitbucket-server.el ends here
