;;; google-meta.el --- Support for the Google API -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2020 Warren Wilkinson

;; Author: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Maintainer: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Created: 20 Feb 2020
;; Version: 0.1
;; Keywords: extensions
;; Homepage: http://example.com/foo
;; Package-Requires: (url)

;; This file is not part of GNU Emacs.

;; This file is part of the Emacs-Google-Api.

;; Emacs-Google-Api is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Emacs-Google-Api is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs-Google-Api.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See this:
;; https://developers.google.com/discovery/v1/building-a-client-library
;; https://developers.google.com/discovery/v1/reference/apis
;; https://developers.google.com/api-client-library

;; Okay, what do I want to do with it?
;; Probably just evaluate it...

;; Okay, I've got a lot of things in an alist...
;; revision, documentationLink, id... basepath,
;; schemas... whatever those are.... okay,
;; and I need to define some kind of operation
;; upon these.. but I don't know precisely what.
;; But I'd like to get the data out into a format
;; I can use...
;; What kind of rules?
;; what kind of operation to understand this tree?
;; a PATH is useful...
;; so I can say "basePath"..
;; but recursion is probably sufficient...
;; with root...
;; for-each ... thing...
;; etc....
;;
;; Okay, you can imagine that I COULD parse a schema..
;; but is that really what I want?  More or less, don't I
;; just want to inquire about something? or find a certain
;; schema?  I want to make it easy for users... a structure
;; isn't easy because it's not defined.  Need to stay
;; flexible.
;;
;; I need two things... I need to be able to DESCRIBE a
;; thing, and to verify what the user gives me adheres to it.
;; SO, to each request, I should just attach the SCHEMA in the right place.

;;; Code:

(require 'url)

(defvar google-discovery-documents
  '("https://translation.googleapis.com/$discovery/rest?version=v3")
  "A list of discovery documents to process.")

(defun google--ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defmacro with--google-json-bind (fields thing &rest body)
  "A macro for extracting and binding values from Json."
  (let ((var (gensym)))
    `(let ((,var ,thing)
	   ,@(mapcan (lambda (f) (destructuring-bind (field &optional default provided-p)
				     (google--ensure-list f)
				   (if provided-p (list provided-p field) (list field))))
		     fields))
       ,@(mapcar (lambda (f)
       		   (destructuring-bind (field &optional default provided-p)
       		       (google--ensure-list f)
       		     `(let ((,var (assoc ',f ,var)))
       			(setq ,field (if ,var (cdr ,var) ,default))
       			,@(and provided-p `((setq ,provided-p (not (null ,vart))))))))
       		 fields)
       ,@body)))
(put 'with--google-json-bind 'lisp-indent-function 2)

;; ** Code Writer **
(defvar google--buffer nil)
(defun google--open-and-truncate-file-for-discovery-document (json)
  (with--google-json-bind (name version title description) json
    (let ((file-name (concat "google-" name "-" version ".el")))
    ;; revision
    ;; 			   ;;documentationLink
    ;; 			   ;;id
    ;; 			   schemas protocol
    ;; 				   ;;canonicalName auth
    ;; 				   rootUrl
    ;; 				   ;;ownerDoman
    ;; 				   ;;name batchPath title
    ;; 				   resources parameters
    ;; 				   ;;version kind description
    ;; 				   servicePath) json
      (setf google--buffer (find-file-literally file-name))
      (with-current-buffer google--buffer
	(erase-buffer)
	(lisp-mode)
	(insert ";;; " file-name " --- " title ", " version " -*- lexical-binding: t; -*-\n\n")
	(insert ";; Copyright (C) 2020-2020 Warren Wilkinson\n\n")
	(insert ";; Author: Warren Wilkinson <warrenwilkinson@gmail.com>\n")
	(insert ";; Maintainer: Warren Wilkinson <warrenwilkinson@gmail.com>\n")
	;; (insert ";; Created: 20 Feb 2020")
	;; (insert ";; Version: 0.1")
	(insert ";; Keywords: extensions\n")
	;; Homepage: http://example.com/foo
	(insert ";; Package-Requires: (url)\n\n")
	(insert ";; This file is not part of GNU Emacs.\n\n")
	(insert ";; This file is part of the Emacs-Google-Api.\n\n")
	(insert ";; Emacs-Google-Api is free software: you can redistribute it and/or modify\n")
	(insert ";; it under the terms of the GNU General Public License as published by\n")
	(insert ";; the Free Software Foundation, either version 3 of the License, or\n")
	(insert ";; (at your option) any later version.\n\n")
	(insert ";; Emacs-Google-Api is distributed in the hope that it will be useful,\n")
	(insert ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n")
	(insert ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n")
	(insert ";; GNU General Public License for more details.\n\n")
	(insert ";; You should have received a copy of the GNU General Public License\n")
	(insert ";; along with Emacs-Google-Api.  If not, see <https://www.gnu.org/licenses/>.\n\n")
	(insert ";;; Commentary:\n\n")
	(insert ";; " description "\n\n")
	(insert ";;; Code:\n\n")))))

(defun google--save-and-kill-discovery-document-buffer (json)
  (with-current-buffer google--buffer
    (basic-save-buffer)
    (kill-buffer)
    (setf google--buffer nil)))

(defun no-op (a b) nil)

;; ** Programmers interface. **
;; You should set the method, resource-after, resource-before, discovery-document-aftter and
;; discovery-document-before callbacks.  These callbacks can refer to the dynamic variables
;; schemas, parameters, url, and path.
;;
;; Once set, the entry point is google--discovery-document-process, which is usually
;; passed to google-discovery-document-call.
(defvar google--schemas)
(defvar google--parameters)
(defvar google--url)
(defvar google--path)
(defvar google--method-callback 'no-op) ;; args: methodKey json-sexp
(defvar google--resource-before-callback 'no-op) ;; args: resourceKey json-sexp
(defvar google--resource-after-callback 'no-op) ;; args: resourceKey json-sexp
(defvar google--discovery-document-before-callback 'google--open-and-truncate-file-for-discovery-document) ;; args: json-sexp
(defvar google--discovery-document-after-callback 'google--save-and-kill-discovery-document-buffer) ;; args: json-sexp

(defun google--handle-method (method)
  "Call the appropriate callback."
  (destructuring-bind (methodKey . json) method
    (funcall google--method-callback methodKey json)))

(defun google--handle-resource (resource)
  "Parse the resource and call the appropriate callbacks."
  (destructuring-bind (resourceKey . json) resource
    (let ((google--path (cons resource google--path)))
      (funcall google--resource-before-callback resourceKey json)
      (with--google-json-bind (methods resources) json
	(map nil 'google--handle-method methods)
	(map nil 'google--handle-resource resources))
      (funcall google--resource-after-callback resourceKey json))))

(defun google--discovery-document-process (status)
  "This is the primary callback function for google--discovery-document-call.
   It uses the callback interface above to process the discovery
   document.

   You should set the method, resource-after, resource-before, discovery-document-aftter and
   discovery-document-before callbacks.  These callbacks can refer to the dynamic variables
   schemas, parameters, url, and path.

   Once set, the entry point is google--discovery-document-process, which is usually
   passed to google-discovery-document-call."
  (unless (boundp 'google--method-callback)
    (error "google--method-callback should be bound in google-discovery-document-process."))
  (unless (boundp 'google--resource-before-callback)
    (error "google--resource-before-callback should be bound in google-discovery-document-process."))
  (unless (boundp 'google--resource-after-callback)
    (error "google--resource-after-callback should be bound in google-discovery-document-process."))
  (let ((json (save-excursion (goto-char url-http-end-of-headers) (json-read))))
    (with--google-json-bind (;;name revision
			     ;;documentationLink
			     ;;id
			     schemas protocol
			     ;;canonicalName auth
			     rootUrl
			     ;;ownerDoman
			     ;;name batchPath title
			     resources parameters methods
			     kind
			     ;;version kind description
			     servicePath) json
      (unless (string-equal kind "discovery#restDescription")
	(error "Google discovery document had wrong kind: %s" kind))
      (unless (string-equal protocol "rest")
	(error "Google discovery document had wrong protocol: %s" rest))

      ;; Bind special variables
      (let ((google--schemas schemas)
	    (google--parameters parameters)
	    (google--url (concat rootUrl servicePath))
	    (google--path (list (cons nil json))))
	(funcall google--discovery-document-before-callback json)
	(dolist (m methods)
	  ;; Do something with methods.... but what?
	  (google--handle-method m))
	(dolist (r resources)
	  (google--handle-resource r))
	(funcall google--discovery-document-after-callback json)))))

(defvar google--debug-json nil)
(defun google--discovery-document-debug (status)
  "This is a callback function for
   google--discovery-document-call that displays the retrieved
   discovery document for investigation."
  (switch-to-buffer (current-buffer))
  (setq google--debug-json (save-excursion
			     (goto-char url-http-end-of-headers)
			     (json-read))))

(defvar google--discovery-documents nil
  "Stores an association list of URIs and discovery-document-responses.")

(defun google--discovery-document-call (uri callback &optional no-cache)
  "Entry point to fetch and process (with the callback) a google
   discovery document at a specific URL. There are two
   ways to call:

   1. With google--discovery-document-process to transform the
      document into a elisp file.
   2. With google--discovery-document-debug to bring up a buffer
      with the raw HTTP request results."
  (let ((existing (assoc uri google--discovery-documents)))
    (if (and (not no-cache) existing)
	(save-excursion
	  (with-current-buffer (cdr existing)
	    (funcall callback nil)))
      (let ((url-request-method "GET"))
	(message "Fetching google discovery document %s" uri)
	(url-queue-retrieve
	 uri
	 (lambda (status)
	   (if (eq (first status) :error)
	       (destructuring-bind (error-symbol . data) (second status)
		 (signal error-symbol data))
	     (progn
	       (setf google--discovery-documents
		     (cons (cons uri (current-buffer))
			   (remove existing google--discovery-documents)))
	       (funcall callback status)))))))))

;; (google--discovery-document-call
;;  (first google-discovery-documents)
;;  'google--discovery-document-debug)

(google--discovery-document-call
 (first google-discovery-documents)
 'google--discovery-document-process)

;;; google-meta.el ends here
