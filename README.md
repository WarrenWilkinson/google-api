

# Google Cloud API For Emacs

This is an emacs library of Google Cloud API routines. They are generated
using Googles' discovery documents.

<https://developers.google.com/discovery/v1/building-a-client-library>
<https://developers.google.com/discovery/v1/reference/apis>
<https://developers.google.com/api-client-library>

It basically is just a library of functions for accessing google-cloud api endpoints.
For example, I'm learning French and I use Google's text to speech interface to
translate French text into audio like this:

    (defvar my-api-key "<my-api-key>")
    (defvar my-response nil)
    (defvar my-bytes nil)
    (defun my-save-speech (text outputfile)
      (setq my-response (google-texttospeech-v1:text/synthesize
       nil
       `((key . ,my-api-key))
       `((input . ((text . ,text)))
         (voice . ((languageCode . "fr-CA")
    	       (name . "fr-CA-Wavenet-B")))
         (audioConfig . ((audioEncoding . "OGG_OPUS"))))))
      (setq my-bytes (base64-decode-string (cdr (assoc 'audioContent my-response))))
      (with-current-buffer (find-file-literally outputfile)
        (erase-buffer)
        (insert my-bytes)
        (basic-save-buffer nil)
        (kill-buffer)))
    
    (my-save-speech "bonjour parlez-vous française garçon?" "/tmp/speech.opus")

