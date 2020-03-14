;;; google-meta.el --- Support for the Google API -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2020 Warren Wilkinson

;; Author: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Maintainer: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Created: 20 Feb 2020
;; Version: 20200221
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

;; Basically, we download the google Discovery Document,
;; and then write out Emacs Lisp code that serves as a library
;; for accessing it.

;;; Code:

(require 'url)

(defvar google-default-timeout 10 "The default timeout the API uses when a custom request-callback has not been provided.")

(eval-when-compile
  (defun google--ensure-list (thing)
    (if (listp thing) thing (list thing))))

(defun google--indent-by-two (string)
  (replace-regexp-in-string "\n" "\n  " string t t))

(defun google--escape-dbl-quotes (string)
  (replace-regexp-in-string "\"" "\\\"" string t t))

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

;; ** Programmatic Interface Dynamic Variables Available to callbacks **
(defvar google--schemas)
(defvar google--parameters)
(defvar google--url)
(defvar google--path nil "List of (ResourceKey . Json)")
(defvar google--json nil "Toplevel Json")
(defvar google--output-directory default-directory)

;; ** Code Writer **
(defvar google--buffer nil)
(defvar google--beginning-of-code-section nil "The beginning of the code section. The encoders/decoders are placed at the beginning.")
(defvar google--required-encoders nil "The names of resources are pushed onto this by emit-method as it's determined we need a specific encoder.")
(defvar google--required-decoders nil "The names of resources are pushed onto this by emit-method as it's determined we need a specific decoder.")

(defun google--open-and-truncate-file-for-discovery-document (json)
  (with--google-json-bind (name version title description documentationLink revision) json
    (let* ((file-name (concat "google-" name "-" version ".el"))
	   (file-path (concat google--output-directory file-name)))
      (setf google--buffer (find-file-literally file-path))
      (with-current-buffer google--buffer
	(erase-buffer)
	(lisp-mode)
	(insert ";;; " file-name " --- " title ", " version " -*- lexical-binding: t; -*-\n\n")
	(insert ";; Copyright (C) 2020-2020 Warren Wilkinson\n\n")
	(insert ";; Author: Warren Wilkinson <warrenwilkinson@gmail.com>\n")
	(insert ";; Maintainer: Warren Wilkinson <warrenwilkinson@gmail.com>\n")
	(insert ";; Keywords: extensions\n")
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
	(insert ";; " description "\n")
	(insert ";; Usage: " documentationLink "\n")
	(insert ";; Revision: " revision "\n\n")
	(insert ";;; Code:\n")
	(setq google--beginning-of-code-section (point))
	(insert "\n")))))

(defun google--ref-p (ref)
  (assert (listp ref)) ;; So far, I've only see refs as either nil or 1 element lists.
  (assert (or (null ref) (= 1 (length ref))))
  (if (null ref)
      nil
    (let ((value (assoc '$ref ref)))
      (assert value)
      (assert (stringp (cdr value)))
      (intern (cdr value)))))

(defun google--ref-target (ref)
  (google--ref-p ref))

(defun google--name-of-structure-json-encoder (google-api-name google-api-version structure-name)
  (assert (symbolp structure-name))
  (intern (concat "google--" google-api-name "-" google-api-version ":structure-encode:"  (symbol-name structure-name))))

(defun google--name-of-structure-json-decoder (google-api-name google-api-version structure-name)
  (assert (symbolp structure-name))
  (intern (concat "google--" google-api-name "-" google-api-version ":structure-decode:"  (symbol-name structure-name))))

(defun google--name-of-structure-metadata-variable (google-api-name google-api-version structure-name)
  (assert (symbolp structure-name))
  (intern (concat "google--" google-api-name "-" google-api-version ":structure-metadata:"  (symbol-name structure-name))))

(defun google--get-schema-by-name (schema-name)
  (let ((found (assoc schema-name google--schemas)))
    (if found
	(cdr found)
      (error "Could not find schema %s. Got %s. Known schemas are: %s" schema-name found (mapcar 'first google--schemas)))))

;; encoders and decoders
;;
;; Okay, so the google discovery document doesn't tell us which fields of the schemas are required and which are optional (it's
;; in the documentation string, not in the metadata). Additionally, it's conceivable a user might provide extra fields that aren't documented or
;; are left-over from upgrading code.  Finally, some fields are marked "output only", which means that they're not intended to be present.
;;
;; Due to all these concerns, I don't bother checking the json --
;; neither the outgoing nor the incoming. Instead we emit a nice
;; doc-string so the user can hopefully piece together what should be
;; there, and then just json-encode/decode whatever we get.

(defun google--emit-function (function-name documentation properties args expressions)
  (let ((doc-string (if properties
			(concat documentation "\n\nAlist Parameters:")
		      (concat documentation "\n\nNO PARAMETERS."))))
    (map nil (lambda (property)
	       (with--google-json-bind (description type properties id) (cdr property)
		 (setf doc-string (concat doc-string (google--indent-by-two (format "\n\n * %s (%s): %s" (car property) type (google--indent-by-two description)))))))
	 properties)
    (with-current-buffer google--buffer
      (insert "\n;;;###autoload")
      (insert "\n(defun " (symbol-name function-name) " (" (mapconcat 'prin1-to-string args " ") ")\n  \"" (google--escape-dbl-quotes (google--indent-by-two doc-string)) "\"")
      (insert (google--indent-by-two (concat "\n" (mapconcat 'pp-to-string expressions "\n"))))
      (let ((end (point)))
	(search-backward ")")
	(delete-region (point) end)
	(insert "))\n")))))

(defun google--emit-decoder (schema-name)
  (with--google-json-bind (name version) google--json
    (let ((function-name (google--name-of-structure-json-decoder name version schema-name))
	  (schema (google--get-schema-by-name schema-name)))
      (with--google-json-bind (description type properties id) schema
	(assert (string= schema-name id))
	(assert (string= type "object"))
	(google--emit-function function-name description properties '() '((json-read)))))))

(defun google--emit-encoder (schema-name)
  (with--google-json-bind (name version) google--json
    (let ((function-name (google--name-of-structure-json-encoder name version schema-name))
	  (schema (google--get-schema-by-name schema-name)))
      (with--google-json-bind (description type properties id) schema
	(assert (string= schema-name id))
	(assert (string= type "object"))
	(google--emit-function function-name description properties '(alist) '((encode-coding-string (json-encode alist) 'utf-8)))))))

(defun google--emit-encoders-and-decoders-save-and-kill-discovery-document-buffer (json)
  (with-current-buffer google--buffer
    (goto-char google--beginning-of-code-section)
    (insert "\n;; Json Decoders\n")
    (map nil 'google--emit-decoder (sort google--required-decoders 'string<))
    (insert "\n;; Json Encoders\n")
    (map nil 'google--emit-encoder (sort google--required-encoders 'string<))
    (basic-save-buffer)
    (kill-buffer)
    (setf google--buffer nil)))

(defun google--create-section-for-resource (resourceKey json)
  (with-current-buffer google--buffer
    (insert "\n;; Resource " (mapconcat 'symbol-name (nreverse (mapcar 'car google--path)) ".")
	    ":\n")))

(defun google--close-section-for-resource (resourceKey json)
  (with-current-buffer google--buffer
    ;; (insert "\n")
    ))

(defvar rfc6570-reserved-characters (list ?: ?/ ?? ?# ?[ ?] ?@
					  ?! ?$ ?& ?' ?( ?)
					  ?* ?+ ?, 59 ?=)) ;; 59 is ?; but using  latter wrecks syntax highlighting and indentation.

(defmacro google--push/concat (value place)
  (let ((v (gensym)))
    `(let ((,v ,value))
       (if (and (stringp ,v) (stringp (car ,place)))
	   (setf ,place (cons (concat (car ,place) ,v) (cdr ,place)))
	 (push ,v ,place)))))

(defun google--build-url-expression (&rest url-fragments)
  "Given a set of url-fragments, each with zero or more fancy {+variable} markers, returns a lisp expression
   which can evaluate and return the final string.

   For example, if called with: 'http://root.com/' 'api/v3/{+hello}/how/are/' '{you}', it would return:
   (list 'http://root.com/api/v3/' '(expand hello) '/how/are/' '(expand you))"
  (let ((fragments
	 (mapcan #'(lambda (fragment)
		     (let ((results nil)
			   (search-start 0))
		       (while (string-match "{[^}]*}" fragment search-start)
			 (let ((start (match-beginning 0))
			       (end (match-end 0)))
			   (when (> start 0)
			     (push (subseq fragment 0 start) results))
			   (setf search-start end)
			   ;; Handle expression, level 2 only...
			   (push
			    (case (elt fragment (+ 1 start))
			      (?+ `(rfc6570-expand-reserved ,(intern (subseq fragment (+ 2 start) (- end 1)))))
			      (?# (push "#" results)
				  `(rfc6570-expand-fragment-identfier ,(intern (subseq fragment (+ 2 start) (- end 1)))))
			      (otherwise
			       `(rfc6570-expand-basic ,(intern (subseq fragment (+ 1 start) (- end 1))))))
			    results)))
		       (when (< search-start (length fragment))
			 (push (subseq fragment search-start) results))
		       (nreverse results)))
		 url-fragments))
	(results))
    (dolist (element fragments (nreverse results))
      (if (and (stringp element) (stringp (car results)))
	  (setf (car results) (concat (car results) element))
	(push element results)))))

(defun google--get-alist (variable alist required type pattern)
  "Extract variable from the users provided alist, ensuring it is
   there if it's required, has the right type and fits the pattern.

   The only supported type is \"string\" and pattern is a regular
   expression on it."
  (let ((binding (assoc variable alist)))
    (when (and (null binding) required)
      (error "Required variable %s was not found." variable))
    (let ((value (cdr binding)))
      (when value
	(cond ((string-equal type "string")
	       (unless (stringp value)
		 (error "Variable %s was %s, which is not a string." variable value))
	       (when (and pattern (not (string-match pattern value)))
		 (error "Variable %s was %s does not match pattern %s." variable value pattern)))
	      ((string-equal type "integer")
	       (unless (integerp value)
		 (error "Variable %s was %s, which is not an integer." variable value)))
	      ((string-equal type "boolean")
	       (unless (booleanp value)
		 (error "Variable %s was %s, which is not an boolean." variable value)))
	      (t nil)))
      value)))

(defun google--build-query-string (alist include-?-prefix)
  (let ((string (mapconcat (lambda (k.v)
			     (let ((k (car k.v))
				   (v (cdr k.v)))
			       (format "%s=%s" k (url-hexify-string v))))
			   (remove-if-not 'cdr alist) "&")))
    (if include-?-prefix
	(concat "?" string)
      string)))
  
(defun google--emit-method (methodKey json)
  (with--google-json-bind (description httpMethod response request path parameters) json
    (assert (member httpMethod '("POST" "GET" "PUT" "DELETE")))
    (let* ((name (with--google-json-bind (name version) google--json
		   (intern (concat "google-" name "-" version ":" (mapconcat 'symbol-name (nreverse (cons methodKey (mapcar 'car google--path))) "/")))))
	   ;; Okay, so what are the variables I have?
	   (local-description (format "path %s\nparams %s\nresponse %s\nrequest: %s" path parameters response request))
	   (doc-string ;(replace-regexp-in-string "\"" "\\\""
	    (if local-description (concat description "\n\n" local-description) description)); t t))
	   (all-parameters
	    (sort (copy-list (append parameters google--parameters)) (lambda (a b) (string< (car a) (car b)))))
	   (expression
	    `(let ,(mapcar (lambda (parameter)
			     (let ((name (first parameter)))
			       (with--google-json-bind (required location type pattern) (cdr parameter)
				 (assert (member type '("string" "integer" "boolean")) t "Unknown type %s, expected one of %s.")
				 (assert (member location '("query" "path")) t "Unknown location %s, expected one of %s")
				 `(,name (google--get-alist ',name alist ,required ,type ,pattern)))))
			   all-parameters)
	       (let ((request-url (concat ,@(google--build-url-expression google--url path)
					  ;; More to go here... url query paramateres which may exist.
					  ,@(let ((query-alist-expression
						   (mapcan (lambda (parameter)
							     (with--google-json-bind (location) (cdr parameter)
							       (when (string-equal location "query")
								 (let ((name (first parameter)))
								   `((cons ,(symbol-name name) ,name))))))
							   all-parameters)))
					      (when query-alist-expression
						`((google--build-query-string (list ,@query-alist-expression) t))))))
		     (url-request-method ,httpMethod)
		     (url-request-data ,(when (google--ref-p request)
				      (with--google-json-bind (name version) google--json
					 (pushnew (google--ref-target request) google--required-encoders)
					`(,(google--name-of-structure-json-encoder name version (google--ref-target request)) request-alist))))
		     (response-parser ,(when (google--ref-p response)
					 (with--google-json-bind (name version) google--json
					   (pushnew (google--ref-target response) google--required-decoders)
					   (list 'quote (google--name-of-structure-json-decoder name version (google--ref-target response)))))))
		 (if request-handler
		     (funcall request-handler request-url url-request-method url-request-data response-parser)
		   (with-current-buffer (url-retrieve-synchronously request-url nil nil google-default-timeout)
		     (goto-char url-http-end-of-headers)
		     (prog1 (funcall response-parser)
		       (kill-buffer))))))))
      (google--emit-function name doc-string all-parameters (if (google--ref-p request) '(request-handler alist request-alist) '(request-handler alist))
			     (list expression)))))

;; ** Programmers interface callbacks **
;; You should set the method, resource-after, resource-before, discovery-document-aftter and
;; discovery-document-before callbacks.  These callbacks can refer to the dynamic variables
;; schemas, parameters, url, and path.
;;
;; Once set, the entry point is google--discovery-document-process, which is usually
;; passed to google-discovery-document-call.

(defvar google--method-callback 'google--emit-method) ;; args: methodKey json-sexp
(defvar google--resource-before-callback 'google--create-section-for-resource) ;; args: resourceKey json-sexp
(defvar google--resource-after-callback 'google--close-section-for-resource) ;; args: resourceKey json-sexp
(defvar google--discovery-document-before-callback 'google--open-and-truncate-file-for-discovery-document) ;; args: json-sexp
(defvar google--discovery-document-after-callback 'google--emit-encoders-and-decoders-save-and-kill-discovery-document-buffer) ;; args: json-sexp

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
	    (google--path nil)
	    (google--json json)
	    (google--beginning-of-code-section nil)
	    (google--required-encoders nil)
	    (google--required-decoders nil)
	    (print-level nil)
	    (print-length nil))
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

(defvar google-discovery-documents
  '("https://translation.googleapis.com/$discovery/rest?version=v3"
    "https://texttospeech.googleapis.com/$discovery/rest?version=v1")
  "A list of discovery documents to process.")

(defun google--meta-process-all ()
  "Run through the list of discovery documents generating lisp
   code for all of them."
  (map nil (lambda (url)
	     (message "Process %s" url)
	     (google--discovery-document-call url 'google--discovery-document-process))
       google-discovery-documents))

;; (google--discovery-document-call
;;  (first google-discovery-documents)
;;  'google--discovery-document-debug)

;; (google--discovery-document-call
;;  (first google-discovery-documents)
;;  'google--discovery-document-process)

;;; google-meta.el ends here
