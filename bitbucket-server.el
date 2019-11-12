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
(require 'request)

(defvar bitbucket-server-url nil
  "The Bitbucket server url to use for the bitbucket-server package.
The format is: `http(s)://<bitbucket-server-url>/'")

(defvar bitbucket-server-username "mihdin"
  "Your Bitbucket Server username.")

(defvar bitbucket-server-password "zh+CEwqDrI4="
  "Your Bitbucket Server password.")

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

(defun bitbucket-server--project-and-repo-path ()
  "Return the Bitbucket server url with the project and repo added at the end."
  (let* ((project-repo-alist (bitbucket-server--get-project-and-repo))
         (project (cdr(assoc 'project project-repo-alist)))
         (repo (cdr (assoc 'repo project-repo-alist))))
    (format "projects/%s/repos/%s"
            project
            repo)))

(defun bitbucket-server--get-file-path-from-project-root ()
  "Get file path of the current buffer, relative to the project root."
  (file-relative-name (buffer-file-name) (projectile-project-root)))

(defun bitbucket-server--query-params-to-string (query-params)
  "Transform a QUERY-PARAMS association list to a string.
The output will be similar to \"key1=value1&key2=value2\"."
  (string-join (cl-loop for (key . value) in query-params
                        collect (format "%s=%s" key (url-hexify-string value)))
               "&"))

(defun bitbucket-server--build-url (url-path &optional query-params)
  "Generic function for building the Bitbucket server URL.
It concatenates the `bitbucket-server-url' variable to
the URL-PATH and the url encoded QUERY-PARAMS as optional.

Function parameter types are described below.
URL-PATH: string
QUERY-PARAMS: association list

Example usage:
\(bitbucket-server--build-url \"compare/commits\" '((sourceBranch . featureX) (tragetBranch . master)))

Result:
\"https://<bitbucket-server-host>/compare/commits?sourceBranch=refs%2Lheads%2LfeatureX&targetBranch=refs%2Lheads%2Lmaster\""
  (let ((bitbucket-url (format "%s/%s"
                               bitbucket-server-url
                               url-path)))
    (if query-params
        (format "%s?%s"
                bitbucket-url
                (bitbucket-server--query-params-to-string query-params))
      bitbucket-url)))


(defun bitbucket-server--file-url (&optional suffix)
  "Return a Bitbucket Server URL for the current buffer.
Optionally, resulting string can be suffixed with SUFFIX."
  (bitbucket-server--ensure-url-is-set)
  (let* ((file-path-in-project (bitbucket-server--get-file-path-from-project-root))
         (bitbucket-file-url (bitbucket-server--build-url (format "%s/browse/%s" (bitbucket-server--project-and-repo-path) file-path-in-project)
                                                          (list (cons 'at (bitbucket-server--refs-heads (magit-get-current-branch)))))))

    (if suffix
        (format "%s%s" bitbucket-file-url suffix)
      bitbucket-file-url)))

(defun bitbucket-server-open-pr (target-branch)
  "Open a browser to your Bitbucket server pull request creation page.
Uses TARGET-BRANCH as the starting point of the branch diff.

Prompts the user to set the Bitbucket server URL if not already set."
  (interactive
   (list (read-from-minibuffer "Bitbucket PR target branch: " "master")))
  (bitbucket-server--ensure-url-is-set)
  (let ((bitbucket-server-pr-url (bitbucket-server--build-url (format "%s/%s" (bitbucket-server--project-and-repo-path) "compare/commits")
                                                           (list (cons 'sourceBranch (bitbucket-server--refs-heads (magit-get-current-branch)))
                                                                 (cons 'targetBranch (bitbucket-server--refs-heads target-branch))))))
    (browse-url bitbucket-server-pr-url)))

(defun bitbucket-server-open-file ()
  "Open a browser to your file in Bitbucket server."
  (interactive)
  (browse-url (bitbucket-server--file-url)))

(defun bitbucket-server-open-file-at-point ()
  "Open a browser to your file in Bitbucket server.
Will open the file at the current line number."
  (interactive)
  (browse-url (bitbucket-server--file-url (format "#%s" (line-number-at-pos)))))

(defun bitbucket-server-copy-file-url ()
  "Add Bitbucket Server file URL to the kill ring."
  (interactive)
  (kill-new (bitbucket-server--file-url)))

(defun bitbucket-server-copy-file-url-with-line-number ()
  "Add Bitbucket Server file URL, with line number, to the kill ring."
  (interactive)
  (kill-new (bitbucket-server--file-url (format "#%s" (line-number-at-pos)))))

(defun bitbucket-server--parse-pr-data (pr-data)
  "WIP: TBD DATA."
  (cl-loop for pr across pr-data
           do (cl-delete-if-not (lambda (key-value) (member (car key-value) '(id title state links))) pr))
  (message "%S" pr-data))

(defun bitbucket-server--own-pull-requests ()
  "WIP: Get the Bitbucket Server pull request data for the current user."
  (let ((bitbucket-server-api-url (bitbucket-server--build-url "rest/api/1.0/dashboard/pull-requests")))
    (request
     bitbucket-server-api-url
     :headers (list (cons "Content-Type" "application/json")
                    (cons "Authorization" (format "Basic %s"
                                                  (base64-encode-string (format "%s:%s"
                                                                                bitbucket-server-username
                                                                                bitbucket-server-password)))))
     :parser 'json-read
     :complete (cl-function
               (lambda (&key data &key error-thrown &allow-other-keys)
                 (if error-thrown
                     (message "Failed to fetch pull requests data: %s" error-thrown)
                   (bitbucket-server--parse-pr-data (cdr (assoc 'values data)))))))))

(provide 'bitbucket-server)
;;; bitbucket-server.el ends here
