;;; google-texttospeech-v1.el --- Cloud Text-to-Speech API, v1 -*- lexical-binding: t; -*-

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

;; Synthesizes natural-sounding speech by applying powerful neural network models.
;; Usage: https://cloud.google.com/text-to-speech/
;; Revision: 20200221

;;; Code:

;; Json Decoders

;;;###autoload
(defun google--texttospeech-v1:structure-decode:ListVoicesResponse ()
  "The message returned to the client by the `ListVoices` method.
  
  Alist Parameters:
    
     * voices (array): The list of voices."
  (json-read))

;;;###autoload
(defun google--texttospeech-v1:structure-decode:SynthesizeSpeechResponse ()
  "The message returned to the client by the `SynthesizeSpeech` method.
  
  Alist Parameters:
    
     * audioContent (string): The audio data bytes encoded as specified in the request, including the
      header for encodings that are wrapped in containers (e.g. MP3, OGG_OPUS).
      For LINEAR16 audio, we include the WAV header. Note: as
      with all bytes fields, protobuffers use a pure binary representation,
      whereas JSON representations use base64."
  (json-read))

;; Json Encoders

;;;###autoload
(defun google--texttospeech-v1:structure-encode:SynthesizeSpeechRequest (alist)
  "The top-level message sent by the client for the `SynthesizeSpeech` method.
  
  Alist Parameters:
    
     * input (nil): Required. The Synthesizer requires either plain text or SSML as input.
    
     * voice (nil): Required. The desired voice of the synthesized audio.
    
     * audioConfig (nil): Required. The configuration of the synthesized audio."
  (encode-coding-string
   (json-encode alist)
   'utf-8))


;; Resource voices:

;;;###autoload
(defun google-texttospeech-v1:voices/list (request-handler alist)
  "Returns a list of Voice supported for synthesis.
  
  path v1/voices
  params ((languageCode (description . Optional. Recommended.
  [BCP-47](https://www.rfc-editor.org/rfc/bcp/bcp47.txt) language tag. If
  specified, the ListVoices call will only return voices that can be used to
  synthesize this language_code. E.g. when specifying \"en-NZ\", you will get
  supported \"en-*\" voices; when specifying \"no\", you will get supported
  \"no-*\" (Norwegian) and \"nb-*\" (Norwegian Bokmal) voices; specifying \"zh\"
  will also get supported \"cmn-*\" voices; specifying \"zh-hk\" will also get
  supported \"yue-*\" voices.) (type . string) (location . query)))
  response (($ref . ListVoicesResponse))
  request: nil
  
  Alist Parameters:
    
     * $.xgafv (string): V1 error format.
    
     * access_token (string): OAuth access token.
    
     * alt (string): Data format for response.
    
     * callback (string): JSONP
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * languageCode (string): Optional. Recommended.
      [BCP-47](https://www.rfc-editor.org/rfc/bcp/bcp47.txt) language tag. If
      specified, the ListVoices call will only return voices that can be used to
      synthesize this language_code. E.g. when specifying \"en-NZ\", you will get
      supported \"en-*\" voices; when specifying \"no\", you will get supported
      \"no-*\" (Norwegian) and \"nb-*\" (Norwegian Bokmal) voices; specifying \"zh\"
      will also get supported \"cmn-*\" voices; specifying \"zh-hk\" will also get
      supported \"yue-*\" voices.
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\")."
  (let
      (($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (languageCode
        (google--get-alist 'languageCode alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://texttospeech.googleapis.com/v1/voices"
  		(google--build-query-string
  		 (list
  		  (cons "$.xgafv" $\.xgafv)
  		  (cons "access_token" access_token)
  		  (cons "alt" alt)
  		  (cons "callback" callback)
  		  (cons "fields" fields)
  		  (cons "key" key)
  		  (cons "languageCode" languageCode)
  		  (cons "oauth_token" oauth_token)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "quotaUser" quotaUser)
  		  (cons "uploadType" uploadType)
  		  (cons "upload_protocol" upload_protocol))
  		 t)))
         (url-request-method "GET")
         (url-request-data nil)
         (response-parser 'google--texttospeech-v1:structure-decode:ListVoicesResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))

;; Resource text:

;;;###autoload
(defun google-texttospeech-v1:text/synthesize (request-handler alist request-alist)
  "Synthesizes speech synchronously: receive results after all text input
  has been processed.
  
  path v1/text:synthesize
  params nil
  response (($ref . SynthesizeSpeechResponse))
  request: (($ref . SynthesizeSpeechRequest))
  
  Alist Parameters:
    
     * $.xgafv (string): V1 error format.
    
     * access_token (string): OAuth access token.
    
     * alt (string): Data format for response.
    
     * callback (string): JSONP
    
     * fields (string): Selector specifying which fields to include in a partial response.
    
     * key (string): API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.
    
     * oauth_token (string): OAuth 2.0 token for the current user.
    
     * prettyPrint (boolean): Returns response with indentations and line breaks.
    
     * quotaUser (string): Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters.
    
     * uploadType (string): Legacy upload protocol for media (e.g. \"media\", \"multipart\").
    
     * upload_protocol (string): Upload protocol for media (e.g. \"raw\", \"multipart\")."
  (let
      (($\.xgafv
        (google--get-alist '$\.xgafv alist nil "string" nil))
       (access_token
        (google--get-alist 'access_token alist nil "string" nil))
       (alt
        (google--get-alist 'alt alist nil "string" nil))
       (callback
        (google--get-alist 'callback alist nil "string" nil))
       (fields
        (google--get-alist 'fields alist nil "string" nil))
       (key
        (google--get-alist 'key alist nil "string" nil))
       (oauth_token
        (google--get-alist 'oauth_token alist nil "string" nil))
       (prettyPrint
        (google--get-alist 'prettyPrint alist nil "boolean" nil))
       (quotaUser
        (google--get-alist 'quotaUser alist nil "string" nil))
       (uploadType
        (google--get-alist 'uploadType alist nil "string" nil))
       (upload_protocol
        (google--get-alist 'upload_protocol alist nil "string" nil)))
    (let
        ((request-url
  	(concat "https://texttospeech.googleapis.com/v1/text:synthesize"
  		(google--build-query-string
  		 (list
  		  (cons "$.xgafv" $\.xgafv)
  		  (cons "access_token" access_token)
  		  (cons "alt" alt)
  		  (cons "callback" callback)
  		  (cons "fields" fields)
  		  (cons "key" key)
  		  (cons "oauth_token" oauth_token)
  		  (cons "prettyPrint" prettyPrint)
  		  (cons "quotaUser" quotaUser)
  		  (cons "uploadType" uploadType)
  		  (cons "upload_protocol" upload_protocol))
  		 t)))
         (url-request-method "POST")
         (url-request-data
  	(google--texttospeech-v1:structure-encode:SynthesizeSpeechRequest request-alist))
         (response-parser 'google--texttospeech-v1:structure-decode:SynthesizeSpeechResponse))
      (if request-handler
  	(funcall request-handler request-url url-request-method url-request-data response-parser)
        (with-current-buffer
  	  (url-retrieve-synchronously request-url nil nil google-default-timeout)
  	(goto-char url-http-end-of-headers)
  	(prog1
  	    (funcall response-parser)
  	  (kill-buffer)))))))
