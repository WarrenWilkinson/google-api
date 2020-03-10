;;; google-translate-v3.el --- Cloud Translation API, v3 -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2020 Warren Wilkinson

;; Author: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Maintainer: Warren Wilkinson <warrenwilkinson@gmail.com>
;; Keywords: extensions
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

;; Integrates text translation into your website or application.
;; Usage: https://cloud.google.com/translate/docs/quickstarts
;; Revision: 20200221

;;; Code:

;; Json Decoders

(defun google--translate-v3:structure-decode:DetectLanguageResponse ()
  "The response message for language detection.
  
  Alist Parameters:
    
     * languages (array): A list of detected languages sorted by detection confidence in descending
      order. The most probable language first."
  (json-read))

(defun google--translate-v3:structure-decode:Empty ()
  "A generic empty message that you can re-use to avoid defining duplicated
  empty messages in your APIs. A typical example is to use it as the request
  or the response type of an API method. For instance:
  
      service Foo {
        rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty);
      }
  
  The JSON representation for `Empty` is empty JSON object `{}`.
  
  NO PARAMETERS."
  (json-read))

(defun google--translate-v3:structure-decode:Glossary ()
  "Represents a glossary built from user provided data.
  
  Alist Parameters:
    
     * inputConfig (nil): Required. Provides examples to build the glossary from.
      Total glossary must not exceed 10M Unicode codepoints.
    
     * submitTime (string): Output only. When CreateGlossary was called.
    
     * name (string): Required. The resource name of the glossary. Glossary names have the form
      `projects/{project-number-or-id}/locations/{location-id}/glossaries/{glossary-id}`.
    
     * languagePair (nil): Used with unidirectional glossaries.
    
     * languageCodesSet (nil): Used with equivalent term set glossaries.
    
     * endTime (string): Output only. When the glossary creation was finished.
    
     * entryCount (integer): Output only. The number of entries defined in the glossary."
  (json-read))

(defun google--translate-v3:structure-decode:ListGlossariesResponse ()
  "Response message for ListGlossaries.
  
  Alist Parameters:
    
     * nextPageToken (string): A token to retrieve a page of results. Pass this value in the
      [ListGlossariesRequest.page_token] field in the subsequent call to
      `ListGlossaries` method to retrieve the next page of results.
    
     * glossaries (array): The list of glossaries for a project."
  (json-read))

(defun google--translate-v3:structure-decode:ListLocationsResponse ()
  "The response message for Locations.ListLocations.
  
  Alist Parameters:
    
     * locations (array): A list of locations that matches the specified filter in the request.
    
     * nextPageToken (string): The standard List next-page token."
  (json-read))

(defun google--translate-v3:structure-decode:ListOperationsResponse ()
  "The response message for Operations.ListOperations.
  
  Alist Parameters:
    
     * operations (array): A list of operations that matches the specified filter in the request.
    
     * nextPageToken (string): The standard List next-page token."
  (json-read))

(defun google--translate-v3:structure-decode:Location ()
  "A resource that represents Google Cloud Platform location.
  
  Alist Parameters:
    
     * metadata (object): Service-specific metadata. For example the available capacity at the given
      location.
    
     * labels (object): Cross-service attributes for the location. For example
      
          {\"cloud.googleapis.com/region\": \"us-east1\"}
    
     * name (string): Resource name for the location, which may vary between implementations.
      For example: `\"projects/example-project/locations/us-east1\"`
    
     * locationId (string): The canonical id for this location. For example: `\"us-east1\"`.
    
     * displayName (string): The friendly name for this location, typically a nearby city name.
      For example, \"Tokyo\"."
  (json-read))

(defun google--translate-v3:structure-decode:Operation ()
  "This resource represents a long-running operation that is the result of a
  network API call.
  
  Alist Parameters:
    
     * response (object): The normal response of the operation in case of success.  If the original
      method returns no data on success, such as `Delete`, the response is
      `google.protobuf.Empty`.  If the original method is standard
      `Get`/`Create`/`Update`, the response should be the resource.  For other
      methods, the response should have the type `XxxResponse`, where `Xxx`
      is the original method name.  For example, if the original method name
      is `TakeSnapshot()`, the inferred response type is
      `TakeSnapshotResponse`.
    
     * name (string): The server-assigned name, which is only unique within the same service that
      originally returns it. If you use the default HTTP mapping, the
      `name` should be a resource name ending with `operations/{unique_id}`.
    
     * error (nil): The error result of the operation in case of failure or cancellation.
    
     * metadata (object): Service-specific metadata associated with the operation.  It typically
      contains progress information and common metadata such as create time.
      Some services might not provide such metadata.  Any method that returns a
      long-running operation should document the metadata type, if any.
    
     * done (boolean): If the value is `false`, it means the operation is still in progress.
      If `true`, the operation is completed, and either `error` or `response` is
      available."
  (json-read))

(defun google--translate-v3:structure-decode:SupportedLanguages ()
  "The response message for discovering supported languages.
  
  Alist Parameters:
    
     * languages (array): A list of supported language responses. This list contains an entry
      for each language the Translation API supports."
  (json-read))

(defun google--translate-v3:structure-decode:TranslateTextResponse ()
  "
  
  Alist Parameters:
    
     * glossaryTranslations (array): Text translation responses if a glossary is provided in the request.
      This can be the same as
      `translations` if no terms apply.
      This field has the same length as
      `contents`.
    
     * translations (array): Text translation responses with no glossary applied.
      This field has the same length as
      `contents`."
  (json-read))

;; Json Encoders

(defun google--translate-v3:structure-encode:BatchTranslateTextRequest (alist)
  "The batch translation request.
  
  Alist Parameters:
    
     * outputConfig (nil): Required. Output configuration.
      If 2 input configs match to the same file (that is, same input path),
      we don't generate output for duplicate inputs.
    
     * sourceLanguageCode (string): Required. Source language code.
    
     * inputConfigs (array): Required. Input configurations.
      The total number of files matched should be <= 1000.
      The total content size should be <= 100M Unicode codepoints.
      The files must use UTF-8 encoding.
    
     * glossaries (object): Optional. Glossaries to be applied for translation.
      It's keyed by target language code.
    
     * models (object): Optional. The models to use for translation. Map's key is target language
      code. Map's value is model name. Value can be a built-in general model,
      or an AutoML Translation model.
      
      The value format depends on model type:
      
      - AutoML Translation models:
        `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
      
      - General (built-in) models:
        `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
        `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
      
      
      If the map is empty or a specific model is
      not requested for a language pair, then default google model (nmt) is used.
    
     * labels (object): Optional. The labels with user-defined metadata for the request.
      
      Label keys and values can be no longer than 63 characters
      (Unicode codepoints), can only contain lowercase letters, numeric
      characters, underscores and dashes. International characters are allowed.
      Label values are optional. Label keys must start with a letter.
      
      See https://cloud.google.com/translate/docs/advanced/labels for more
      information.
    
     * targetLanguageCodes (array): Required. Specify up to 10 language codes here."
  (encode-coding-string
   (json-encode alist)
   'utf-8))

(defun google--translate-v3:structure-encode:CancelOperationRequest (alist)
  "The request message for Operations.CancelOperation.
  
  NO PARAMETERS."
  (encode-coding-string
   (json-encode alist)
   'utf-8))

(defun google--translate-v3:structure-encode:DetectLanguageRequest (alist)
  "The request message for language detection.
  
  Alist Parameters:
    
     * content (string): The content of the input stored as a string.
    
     * model (string): Optional. The language detection model to be used.
      
      Format:
      `projects/{project-number-or-id}/locations/{location-id}/models/language-detection/{model-id}`
      
      Only one language detection model is currently supported:
      `projects/{project-number-or-id}/locations/{location-id}/models/language-detection/default`.
      
      If not specified, the default model is used.
    
     * mimeType (string): Optional. The format of the source text, for example, \"text/html\",
      \"text/plain\". If left blank, the MIME type defaults to \"text/html\".
    
     * labels (object): Optional. The labels with user-defined metadata for the request.
      
      Label keys and values can be no longer than 63 characters
      (Unicode codepoints), can only contain lowercase letters, numeric
      characters, underscores and dashes. International characters are allowed.
      Label values are optional. Label keys must start with a letter.
      
      See https://cloud.google.com/translate/docs/advanced/labels for more
      information."
  (encode-coding-string
   (json-encode alist)
   'utf-8))

(defun google--translate-v3:structure-encode:Glossary (alist)
  "Represents a glossary built from user provided data.
  
  Alist Parameters:
    
     * inputConfig (nil): Required. Provides examples to build the glossary from.
      Total glossary must not exceed 10M Unicode codepoints.
    
     * submitTime (string): Output only. When CreateGlossary was called.
    
     * name (string): Required. The resource name of the glossary. Glossary names have the form
      `projects/{project-number-or-id}/locations/{location-id}/glossaries/{glossary-id}`.
    
     * languagePair (nil): Used with unidirectional glossaries.
    
     * languageCodesSet (nil): Used with equivalent term set glossaries.
    
     * endTime (string): Output only. When the glossary creation was finished.
    
     * entryCount (integer): Output only. The number of entries defined in the glossary."
  (encode-coding-string
   (json-encode alist)
   'utf-8))

(defun google--translate-v3:structure-encode:TranslateTextRequest (alist)
  "The request message for synchronous translation.
  
  Alist Parameters:
    
     * sourceLanguageCode (string): Optional. The BCP-47 language code of the input text if
      known, for example, \"en-US\" or \"sr-Latn\". Supported language codes are
      listed in Language Support. If the source language isn't specified, the API
      attempts to identify the source language automatically and returns the
      source language within the response.
    
     * model (string): Optional. The `model` type requested for this translation.
      
      The format depends on model type:
      
      - AutoML Translation models:
        `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
      
      - General (built-in) models:
        `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
        `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
      
      
      For global (non-regionalized) requests, use `location-id` `global`.
      For example,
      `projects/{project-number-or-id}/locations/global/models/general/nmt`.
      
      If missing, the system decides which google base model to use.
    
     * contents (array): Required. The content of the input in string format.
      We recommend the total content be less than 30k codepoints.
      Use BatchTranslateText for larger text.
    
     * mimeType (string): Optional. The format of the source text, for example, \"text/html\",
       \"text/plain\". If left blank, the MIME type defaults to \"text/html\".
    
     * glossaryConfig (nil): Optional. Glossary to be applied. The glossary must be
      within the same region (have the same location-id) as the model, otherwise
      an INVALID_ARGUMENT (400) error is returned.
    
     * labels (object): Optional. The labels with user-defined metadata for the request.
      
      Label keys and values can be no longer than 63 characters
      (Unicode codepoints), can only contain lowercase letters, numeric
      characters, underscores and dashes. International characters are allowed.
      Label values are optional. Label keys must start with a letter.
      
      See https://cloud.google.com/translate/docs/advanced/labels for more
      information.
    
     * targetLanguageCode (string): Required. The BCP-47 language code to use for translation of the input
      text, set to one of the language codes listed in Language Support."
  (encode-coding-string
   (json-encode alist)
   'utf-8))

(defun google--translate-v3:structure-encode:WaitOperationRequest (alist)
  "The request message for Operations.WaitOperation.
  
  Alist Parameters:
    
     * timeout (string): The maximum duration to wait before timing out. If left blank, the wait
      will be at most the time permitted by the underlying HTTP/RPC protocol.
      If RPC context deadline is also specified, the shorter one will be used."
  (encode-coding-string
   (json-encode alist)
   'utf-8))


;; Resource projects:

(defun google-translate-v3:projects/detectLanguage (request-handler alist request-alist)
  "Detects the language of text within a request.
  
  path v3/{+parent}:detectLanguage
  params ((parent (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}/locations/{location-id}` or
  `projects/{project-number-or-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Only models within the same region (has same location-id) can be used.
  Otherwise an INVALID_ARGUMENT (400) error is returned.) (required . t) (type . string) (pattern . ^projects/[^/]+$)))
  response (($ref . DetectLanguageResponse))
  request: (($ref . DetectLanguageRequest))
  
  Alist Parameters:
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}/locations/{location-id}` or
      `projects/{project-number-or-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Only models within the same region (has same location-id) can be used.
      Otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		":detectLanguage"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:DetectLanguageRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:DetectLanguageResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/translateText (request-handler alist request-alist)
  "Translates input text and returns translated text.
  
  path v3/{+parent}:translateText
  params ((parent (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}` or
  `projects/{project-number-or-id}/locations/{location-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Non-global location is required for requests using AutoML models or
  custom glossaries.
  
  Models and glossaries must be within the same region (have same
  location-id), otherwise an INVALID_ARGUMENT (400) error is returned.) (required . t) (type . string) (pattern . ^projects/[^/]+$)))
  response (($ref . TranslateTextResponse))
  request: (($ref . TranslateTextRequest))
  
  Alist Parameters:
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}` or
      `projects/{project-number-or-id}/locations/{location-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Non-global location is required for requests using AutoML models or
      custom glossaries.
      
      Models and glossaries must be within the same region (have same
      location-id), otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		":translateText"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:TranslateTextRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:TranslateTextResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/getSupportedLanguages (request-handler alist)
  "Returns a list of supported languages for translation.
  
  path v3/{+parent}/supportedLanguages
  params ((model (type . string) (location . query) (description . Optional. Get supported languages of this model.
  
  The format depends on model type:
  
  - AutoML Translation models:
    `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
  
  - General (built-in) models:
    `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
    `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
  
  
  Returns languages supported by the specified model.
  If missing, we get supported languages of Google general base (PBMT) model.)) (parent (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}` or
  `projects/{project-number-or-id}/locations/{location-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Non-global location is required for AutoML models.
  
  Only models within the same region (have same location-id) can be used,
  otherwise an INVALID_ARGUMENT (400) error is returned.) (required . t) (type . string) (pattern . ^projects/[^/]+$)) (displayLanguageCode (type . string) (location . query) (description . Optional. The language to use to return localized, human readable names
  of supported languages. If missing, then display names are not returned
  in a response.)))
  response (($ref . SupportedLanguages))
  request: nil
  
  Alist Parameters:
    
     * model (string): Optional. Get supported languages of this model.
      
      The format depends on model type:
      
      - AutoML Translation models:
        `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
      
      - General (built-in) models:
        `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
        `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
      
      
      Returns languages supported by the specified model.
      If missing, we get supported languages of Google general base (PBMT) model.
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}` or
      `projects/{project-number-or-id}/locations/{location-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Non-global location is required for AutoML models.
      
      Only models within the same region (have same location-id) can be used,
      otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * displayLanguageCode (string): Optional. The language to use to return localized, human readable names
      of supported languages. If missing, then display names are not returned
      in a response.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((model
        (google--get-alist 'model alist nil "string" nil))
       (parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+$"))
       (displayLanguageCode
        (google--get-alist 'displayLanguageCode alist nil "string" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		"/supportedLanguages"
  		(google--build-query-string
  		 (list
  		  (cons "model" model)
  		  (cons "displayLanguageCode" displayLanguageCode)
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:SupportedLanguages))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

;; Resource projects.locations:

(defun google-translate-v3:projects/locations/get (request-handler alist)
  "Gets information about a location.
  
  path v3/{+name}
  params ((name (description . Resource name for the location.) (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path)))
  response (($ref . Location))
  request: nil
  
  Alist Parameters:
    
     * name (string): Resource name for the location.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:Location))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/batchTranslateText (request-handler alist request-alist)
  "Translates a large volume of text in asynchronous batch mode.
  This function provides real-time output as the inputs are being processed.
  If caller cancels a request, the partial results (for an input file, it's
  all or nothing) may still be available on the specified output location.
  
  This call returns immediately and you can
  use google.longrunning.Operation.name to poll the status of the call.
  
  path v3/{+parent}:batchTranslateText
  params ((parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. Location to make a call. Must refer to a caller's project.
  
  Format: `projects/{project-number-or-id}/locations/{location-id}`.
  
  The `global` location is not supported for batch translation.
  
  Only AutoML Translation models or glossaries within the same region (have
  the same location-id) can be used, otherwise an INVALID_ARGUMENT (400)
  error is returned.)))
  response (($ref . Operation))
  request: (($ref . BatchTranslateTextRequest))
  
  Alist Parameters:
    
     * parent (string): Required. Location to make a call. Must refer to a caller's project.
      
      Format: `projects/{project-number-or-id}/locations/{location-id}`.
      
      The `global` location is not supported for batch translation.
      
      Only AutoML Translation models or glossaries within the same region (have
      the same location-id) can be used, otherwise an INVALID_ARGUMENT (400)
      error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		":batchTranslateText"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:BatchTranslateTextRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:Operation))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/list (request-handler alist)
  "Lists information about the supported locations for this service.
  
  path v3/{+name}/locations
  params ((filter (description . The standard list filter.) (type . string) (location . query)) (name (location . path) (description . The resource that owns the locations collection, if applicable.) (required . t) (type . string) (pattern . ^projects/[^/]+$)) (pageToken (description . The standard list page token.) (type . string) (location . query)) (pageSize (description . The standard list page size.) (format . int32) (type . integer) (location . query)))
  response (($ref . ListLocationsResponse))
  request: nil
  
  Alist Parameters:
    
     * filter (string): The standard list filter.
    
     * name (string): The resource that owns the locations collection, if applicable.
    
     * pageToken (string): The standard list page token.
    
     * pageSize (integer): The standard list page size.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((filter
        (google--get-alist 'filter alist nil "string" nil))
       (name
        (google--get-alist 'name alist t "string" "^projects/[^/]+$"))
       (pageToken
        (google--get-alist 'pageToken alist nil "string" nil))
       (pageSize
        (google--get-alist 'pageSize alist nil "integer" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		"/locations"
  		(google--build-query-string
  		 (list
  		  (cons "filter" filter)
  		  (cons "pageToken" pageToken)
  		  (cons "pageSize" pageSize)
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:ListLocationsResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/translateText (request-handler alist request-alist)
  "Translates input text and returns translated text.
  
  path v3/{+parent}:translateText
  params ((parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}` or
  `projects/{project-number-or-id}/locations/{location-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Non-global location is required for requests using AutoML models or
  custom glossaries.
  
  Models and glossaries must be within the same region (have same
  location-id), otherwise an INVALID_ARGUMENT (400) error is returned.)))
  response (($ref . TranslateTextResponse))
  request: (($ref . TranslateTextRequest))
  
  Alist Parameters:
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}` or
      `projects/{project-number-or-id}/locations/{location-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Non-global location is required for requests using AutoML models or
      custom glossaries.
      
      Models and glossaries must be within the same region (have same
      location-id), otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		":translateText"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:TranslateTextRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:TranslateTextResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/getSupportedLanguages (request-handler alist)
  "Returns a list of supported languages for translation.
  
  path v3/{+parent}/supportedLanguages
  params ((displayLanguageCode (description . Optional. The language to use to return localized, human readable names
  of supported languages. If missing, then display names are not returned
  in a response.) (type . string) (location . query)) (model (description . Optional. Get supported languages of this model.
  
  The format depends on model type:
  
  - AutoML Translation models:
    `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
  
  - General (built-in) models:
    `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
    `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
  
  
  Returns languages supported by the specified model.
  If missing, we get supported languages of Google general base (PBMT) model.) (type . string) (location . query)) (parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}` or
  `projects/{project-number-or-id}/locations/{location-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Non-global location is required for AutoML models.
  
  Only models within the same region (have same location-id) can be used,
  otherwise an INVALID_ARGUMENT (400) error is returned.)))
  response (($ref . SupportedLanguages))
  request: nil
  
  Alist Parameters:
    
     * displayLanguageCode (string): Optional. The language to use to return localized, human readable names
      of supported languages. If missing, then display names are not returned
      in a response.
    
     * model (string): Optional. Get supported languages of this model.
      
      The format depends on model type:
      
      - AutoML Translation models:
        `projects/{project-number-or-id}/locations/{location-id}/models/{model-id}`
      
      - General (built-in) models:
        `projects/{project-number-or-id}/locations/{location-id}/models/general/nmt`,
        `projects/{project-number-or-id}/locations/{location-id}/models/general/base`
      
      
      Returns languages supported by the specified model.
      If missing, we get supported languages of Google general base (PBMT) model.
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}` or
      `projects/{project-number-or-id}/locations/{location-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Non-global location is required for AutoML models.
      
      Only models within the same region (have same location-id) can be used,
      otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((displayLanguageCode
        (google--get-alist 'displayLanguageCode alist nil "string" nil))
       (model
        (google--get-alist 'model alist nil "string" nil))
       (parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		"/supportedLanguages"
  		(google--build-query-string
  		 (list
  		  (cons "displayLanguageCode" displayLanguageCode)
  		  (cons "model" model)
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:SupportedLanguages))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/detectLanguage (request-handler alist request-alist)
  "Detects the language of text within a request.
  
  path v3/{+parent}:detectLanguage
  params ((parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. Project or location to make a call. Must refer to a caller's
  project.
  
  Format: `projects/{project-number-or-id}/locations/{location-id}` or
  `projects/{project-number-or-id}`.
  
  For global calls, use `projects/{project-number-or-id}/locations/global` or
  `projects/{project-number-or-id}`.
  
  Only models within the same region (has same location-id) can be used.
  Otherwise an INVALID_ARGUMENT (400) error is returned.)))
  response (($ref . DetectLanguageResponse))
  request: (($ref . DetectLanguageRequest))
  
  Alist Parameters:
    
     * parent (string): Required. Project or location to make a call. Must refer to a caller's
      project.
      
      Format: `projects/{project-number-or-id}/locations/{location-id}` or
      `projects/{project-number-or-id}`.
      
      For global calls, use `projects/{project-number-or-id}/locations/global` or
      `projects/{project-number-or-id}`.
      
      Only models within the same region (has same location-id) can be used.
      Otherwise an INVALID_ARGUMENT (400) error is returned.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		":detectLanguage"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:DetectLanguageRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:DetectLanguageResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

;; Resource projects.locations.glossaries:

(defun google-translate-v3:projects/locations/glossaries/delete (request-handler alist)
  "Deletes a glossary, or cancels glossary construction
  if the glossary isn't created yet.
  Returns NOT_FOUND, if the glossary doesn't exist.
  
  path v3/{+name}
  params ((name (description . Required. The name of the glossary to delete.) (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+/glossaries/[^/]+$) (location . path)))
  response (($ref . Operation))
  request: nil
  
  Alist Parameters:
    
     * name (string): Required. The name of the glossary to delete.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/glossaries/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "DELETE")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:Operation))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/glossaries/list (request-handler alist)
  "Lists glossaries in a project. Returns NOT_FOUND, if the project doesn't
  exist.
  
  path v3/{+parent}/glossaries
  params ((parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. The name of the project from which to list all of the glossaries.)) (filter (location . query) (description . Optional. Filter specifying constraints of a list operation.
  Filtering is not supported yet, and the parameter currently has no effect.
  If missing, no filtering is performed.) (type . string)) (pageToken (description . Optional. A token identifying a page of results the server should return.
  Typically, this is the value of [ListGlossariesResponse.next_page_token]
  returned from the previous call to `ListGlossaries` method.
  The first page is returned if `page_token`is empty or missing.) (type . string) (location . query)) (pageSize (type . integer) (location . query) (description . Optional. Requested page size. The server may return fewer glossaries than
  requested. If unspecified, the server picks an appropriate default.) (format . int32)))
  response (($ref . ListGlossariesResponse))
  request: nil
  
  Alist Parameters:
    
     * parent (string): Required. The name of the project from which to list all of the glossaries.
    
     * filter (string): Optional. Filter specifying constraints of a list operation.
      Filtering is not supported yet, and the parameter currently has no effect.
      If missing, no filtering is performed.
    
     * pageToken (string): Optional. A token identifying a page of results the server should return.
      Typically, this is the value of [ListGlossariesResponse.next_page_token]
      returned from the previous call to `ListGlossaries` method.
      The first page is returned if `page_token`is empty or missing.
    
     * pageSize (integer): Optional. Requested page size. The server may return fewer glossaries than
      requested. If unspecified, the server picks an appropriate default.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (filter
        (google--get-alist 'filter alist nil "string" nil))
       (pageToken
        (google--get-alist 'pageToken alist nil "string" nil))
       (pageSize
        (google--get-alist 'pageSize alist nil "integer" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		"/glossaries"
  		(google--build-query-string
  		 (list
  		  (cons "filter" filter)
  		  (cons "pageToken" pageToken)
  		  (cons "pageSize" pageSize)
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:ListGlossariesResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/glossaries/get (request-handler alist)
  "Gets a glossary. Returns NOT_FOUND, if the glossary doesn't
  exist.
  
  path v3/{+name}
  params ((name (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+/glossaries/[^/]+$) (location . path) (description . Required. The name of the glossary to retrieve.)))
  response (($ref . Glossary))
  request: nil
  
  Alist Parameters:
    
     * name (string): Required. The name of the glossary to retrieve.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/glossaries/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:Glossary))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/glossaries/create (request-handler alist request-alist)
  "Creates a glossary and returns the long-running operation. Returns
  NOT_FOUND, if the project doesn't exist.
  
  path v3/{+parent}/glossaries
  params ((parent (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$) (location . path) (description . Required. The project name.)))
  response (($ref . Operation))
  request: (($ref . Glossary))
  
  Alist Parameters:
    
     * parent (string): Required. The project name.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((parent
        (google--get-alist 'parent alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved parent)
  		"/glossaries"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:Glossary request-alist))
         (response-parser 'google--translate-v3:structure-decode:Operation))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

;; Resource projects.locations.operations:

(defun google-translate-v3:projects/locations/operations/cancel (request-handler alist request-alist)
  "Starts asynchronous cancellation on a long-running operation.  The server
  makes a best effort to cancel the operation, but success is not
  guaranteed.  If the server doesn't support this method, it returns
  `google.rpc.Code.UNIMPLEMENTED`.  Clients can use
  Operations.GetOperation or
  other methods to check whether the cancellation succeeded or whether the
  operation completed despite cancellation. On successful cancellation,
  the operation is not deleted; instead, it becomes an operation with
  an Operation.error value with a google.rpc.Status.code of 1,
  corresponding to `Code.CANCELLED`.
  
  path v3/{+name}:cancel
  params ((name (description . The name of the operation resource to be cancelled.) (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+/operations/[^/]+$) (location . path)))
  response (($ref . Empty))
  request: (($ref . CancelOperationRequest))
  
  Alist Parameters:
    
     * name (string): The name of the operation resource to be cancelled.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/operations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		":cancel"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:CancelOperationRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:Empty))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/operations/delete (request-handler alist)
  "Deletes a long-running operation. This method indicates that the client is
  no longer interested in the operation result. It does not cancel the
  operation. If the server doesn't support this method, it returns
  `google.rpc.Code.UNIMPLEMENTED`.
  
  path v3/{+name}
  params ((name (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+/operations/[^/]+$) (location . path) (description . The name of the operation resource to be deleted.)))
  response (($ref . Empty))
  request: nil
  
  Alist Parameters:
    
     * name (string): The name of the operation resource to be deleted.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/operations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "DELETE")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:Empty))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/operations/list (request-handler alist)
  "Lists operations that match the specified filter in the request. If the
  server doesn't support this method, it returns `UNIMPLEMENTED`.
  
  NOTE: the `name` binding allows API services to override the binding
  to use different resource name schemes, such as `users/*/operations`. To
  override the binding, API services can add a binding such as
  `\"/v1/{name=users/*}/operations\"` to their service configuration.
  For backwards compatibility, the default name includes the operations
  collection id, however overriding users must ensure the name binding
  is the parent resource, without the operations collection id.
  
  path v3/{+name}/operations
  params ((name (location . path) (description . The name of the operation's parent resource.) (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+$)) (pageToken (description . The standard list page token.) (type . string) (location . query)) (pageSize (type . integer) (location . query) (description . The standard list page size.) (format . int32)) (filter (type . string) (location . query) (description . The standard list filter.)))
  response (($ref . ListOperationsResponse))
  request: nil
  
  Alist Parameters:
    
     * name (string): The name of the operation's parent resource.
    
     * pageToken (string): The standard list page token.
    
     * pageSize (integer): The standard list page size.
    
     * filter (string): The standard list filter.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+$"))
       (pageToken
        (google--get-alist 'pageToken alist nil "string" nil))
       (pageSize
        (google--get-alist 'pageSize alist nil "integer" nil))
       (filter
        (google--get-alist 'filter alist nil "string" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		"/operations"
  		(google--build-query-string
  		 (list
  		  (cons "pageToken" pageToken)
  		  (cons "pageSize" pageSize)
  		  (cons "filter" filter)
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:ListOperationsResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/operations/get (request-handler alist)
  "Gets the latest state of a long-running operation.  Clients can use this
  method to poll the operation result at intervals as recommended by the API
  service.
  
  path v3/{+name}
  params ((name (pattern . ^projects/[^/]+/locations/[^/]+/operations/[^/]+$) (location . path) (description . The name of the operation resource.) (required . t) (type . string)))
  response (($ref . Operation))
  request: nil
  
  Alist Parameters:
    
     * name (string): The name of the operation resource.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/operations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--translate-v3:structure-decode:Operation))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

(defun google-translate-v3:projects/locations/operations/wait (request-handler alist request-alist)
  "Waits for the specified long-running operation until it is done or reaches
  at most a specified timeout, returning the latest state.  If the operation
  is already done, the latest state is immediately returned.  If the timeout
  specified is greater than the default HTTP/RPC timeout, the HTTP/RPC
  timeout is used.  If the server does not support this method, it returns
  `google.rpc.Code.UNIMPLEMENTED`.
  Note that this method is on a best-effort basis.  It may return the latest
  state before the specified timeout (including immediately), meaning even an
  immediate response is no guarantee that the operation is done.
  
  path v3/{+name}:wait
  params ((name (description . The name of the operation resource to wait on.) (required . t) (type . string) (pattern . ^projects/[^/]+/locations/[^/]+/operations/[^/]+$) (location . path)))
  response (($ref . Operation))
  request: (($ref . WaitOperationRequest))
  
  Alist Parameters:
    
     * name (string): The name of the operation resource to wait on.
    
     * alt (string): Data format for response.
    
     * access_token (string): OAuth access token.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\").
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * callback (string): JSONP
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * $.xgafv (string): V1 error format."
  (let
      ((name
        (google--get-alist 'name alist t "string" "^projects/[^/]+/locations/[^/]+/operations/[^/]+$"))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       ($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://translation.googleapis.com/v3/"
  		(rfc6570-expand-reserved name)
  		":wait"
  		(google--build-query-string
  		 (list
  		  (cons "alt" alt)
  		  (cons "access_token" access_token)
  		  (cons "key" key)
  		  (cons "upload_protocol" upload_protocol)
  		  (cons "quotaUser" quotaUser)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "fields" fields)
  		  (cons "uploadType" uploadType)
  		  (cons "callback" callback)
  		  (cons "oauth_token" oauth_token)
  		  (cons "$.xgafv" $\.xgafv))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--translate-v3:structure-encode:WaitOperationRequest request-alist))
         (response-parser 'google--translate-v3:structure-decode:Operation))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))
