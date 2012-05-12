;;; curl.el --- CURL binding for elisp

;; Copyright (C) 2011  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: lisp, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;; TODO: async interface?

(defconst curl/temp-buffer "*curl*"     ; replace with " *curl*" later.
  "Name of the temporary buffer for cURL.")

(defconst curl/process-name "curl"
  "Name of the curl asynchronous process.")

(defconst curl/return-status-messages
  #s(hash-table data
                (1 "unsupported protocol"
                 2 "initialization failed"
                 3 "URL malformed"
                 5 "couldn't resolve proxy"
                 6 "couldn't resolve host"
                 7 "connection failed"
                 8 "FTP weired server reply"
                 9 "FTP access denied"
                 11 "FTP weird PASS reply"
                 13 "FTP weird PASV reply"
                 14 "FTP weird 227 format"
                 15 "FTP can't get host"
                 17 "FTP couldn't set binary"
                 18 "partial file"
                 19 "FTP RETR failed"
                 21 "FTP quote error"
                 22 "not found"
                 23 "write error"
                 25 "FTP STOR denied"
                 26 "read error"
                 27 "out of memory"
                 28 "operation timeout"
                 30 "FTP PORT failed"
                 31 "FTP REST failed"
                 33 "HTTP range error"
                 34 "HTTP POST error"
                 35 "SSL connect error"
                 36 "FTP resume failed"
                 37 "FILE read error"
                 38 "LDAP bind failed"
                 39 "LDAP search failed"
                 41 "function not found"
                 42 "aborted by callback"
                 43 "internal error"
                 45 "interface error"
                 47 "too many redirects"
                 48 "unknown TELNET option"
                 49 "malformed telnet option"
                 51 "invalid certificate or fingerprint"
                 52 "server replies nothing"
                 53 "SSL crypto engine not found"
                 54 "can't set SSL crypto engine as default"
                 55 "failed sending network data"
                 56 "failure in receiving network data"
                 58 "problem with the local certificate"
                 59 "couldn't use specified SSL cipher"
                 60 "certificate authentication failed"
                 61 "unrecognized transfer encoding"
                 62 "invalid LDAP URL"
                 63 "maximum file size exceeded"
                 64 "FTP SSL level failed"
                 65 "rewind failed"
                 66 "SSL engine initialization failed"
                 67 "login failed"
                 68 "file not found"
                 69 "permission problem"
                 70 "out of disk"
                 71 "invalid TFTP opeation"
                 72 "unknown TFTP transfer ID"
                 73 "file already exists"
                 74 "no such user"
                 75 "character conversion failed"
                 76 "character conversion required"
                 77 "cannot read SSL CA cert"
                 78 "resource does not exist"
                 79 "unspecified error during the SSH session"
                 80 "failed to shut down SSL connection"
                 82 "couldn't load CRL file"
                 83 "issuer check failed"
                 84 "FTP PRET failed"
                 85 "RTSP: mismatch of CSeq"
                 86 "RTSP: mismatch of Session Identifiers"
                 87 "unable to parse FTP file list"
                 88 "FTP chuck callback error"))
  "error messages for CURL exit status")
                 
(defconst curl/http-status-messages
  #s(hash-table size 40 data 
                (100 "Continue"
                 101 "Switching Protocols"
                 200 "OK"
                 201 "Created"
                 202 "Accepted"
                 203 "Non-Authoritative Information"
                 204 "No Content"
                 205 "Reset Content"
                 206 "Partial Content"
                 300 "Multiple Choices"
                 301 "Moved Permanently"
                 302 "Moved Temporarily"
                 303 "See Other"
                 304 "Not Modified"
                 305 "Use Proxy"
                 400 "Bad Request"
                 401 "Unauthorized"
                 402 "Payment Required"
                 403 "Forbidden"
                 404 "Not Found"
                 405 "Method Not Allowed"
                 406 "Not Acceptable"
                 407 "Proxy Authentication Required"
                 408 "Request Timeout"
                 409 "Conflict"
                 410 "Gone"
                 411 "Length Required"
                 412 "Precondition Failed"
                 413 "Request Entity Too Large"
                 414 "Request-URI Too Long"
                 415 "Unsupported Media Type"
                 500 "Internal Server Error"
                 501 "Not Implemented"
                 502 "Bad Gateway"
                 503 "Service Unavailable"
                 504 "Gateway Timeout"
                 505 "HTTP Version Not Supported"))
  "Hashmap of HTTP status code and status string")

(defconst curl/program "curl"
  "curl executable pathname")

(defconst curl/common-options
  "--compressed --silent --location --location-trusted"
  "common curl options")

(defconst curl/cookie-jar (make-temp-file ".curl-cookie")
  "Cookie jar")

(defconst curl/cookie-options
  (format "--cookie %s --cookie-jar %s" curl/cookie-jar curl/cookie-jar)
  "Options to pass for using our cookie jar.")

(defconst curl/crlf-pair
  (format "%c%c%c%c" 13 10 13 10)
  "HTTP headers are ended by a CRLF pair.
Note that in the Curl output, we see lf rather than crlf.")
  
(defconst curl/status-line-regexp
  ;; The first sub-expression should contain the HTTP Status-Code, and
  ;; the second sub-expression should contain the line
  ;; terminator(CR/LF) pair.  Since `curl/http-headers' will decode
  ;; the region so that last CR should be optional for following
  ;; `curl/http-body'.
  (format "^HTTP/[0-9]+\\.[0-9]+ +\\([0-9]+\\) +[^%c%c]*\\(%c?%c\\)" 
          13 10 13 10)
  "Regular expression for HTTP Status-Line.")

(defvar curl/read-only-map (let ((map (make-sparse-keymap)))
                             (suppress-keymap map)
                             map)
  "Read-only keymap used during the async process")

(defsubst curl/http-status-string (status)
  "Return the human-readable string for HTTP status, STATUS."
  (and (stringp status)
       (setq status (string-to-number status)))
  (format "%d: %s" status
          (gethash status curl/http-status-messages "*UNKNOWN*")))


(defsubst curl/buffer-bytes (&optional buffer)
  "Return number of bytes in a buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (and buffer (set-buffer buffer))
      (widen)
      (1- (position-bytes (point-max))))))


(defmacro curl/with-cookie-file (spec &rest body)
  (declare (indent 1) (debug ((symbolp &optional form &optional form) &rest form)))
  (let ((tmpfile (if (cadr spec)
                     (cadr spec)
                   (make-temp-file "cookie")))
        (keepfile (caddr spec)))
    `(let ,(append (list (list (car spec) (format "-b '%s' -c '%s'" 
                                                  tmpfile tmpfile))
                   (if (cadr spec)
                       (list (list (cadr spec) tmpfile)))))
       (unwind-protect
           (progn ,@body)
         (if ,keepfile
             (delete-file ,tmpfile))))))
  
(defmacro curl/with-cookie (spec &rest body)
  "Create a cookie file, and execute BODY.

COOKIE-OPTIONS will contains the string can be used CURL options,
Optional COOKIE-FILE have the pathname of the cookie file.  The
cookie file will be automatically deleted in the end.

\(fn (COOKIE-OPTIONS [COOKIE-FILE]) BODY...)"
  (declare (indent 1) (debug ((symbolp symbolp) &rest form)))
  (let ((tmpfile (make-temp-file "cookie")))
    `(let ,(append (list (list (car spec) (format "-b '%s' -c '%s'" 
                                                  tmpfile tmpfile))
                   (if (cadr spec)
                       (list (list (cadr spec) tmpfile)))))
       (unwind-protect
           (progn ,@body)
         (delete-file ,tmpfile)))))


(defmacro curl/with-temp-buffer (&rest body)
  "Evaluate forms in a  ready to use temporary buffer."
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "TEMP-BUFFER")))
    `(let ((,buffer (get-buffer-create curl/temp-buffer))
           (default-process-coding-system (cons 'utf-8 'utf-8))
           ;; Since we need to capture \r\n in the http resonse
           ;; buffer, we need to set `coding-system-for-read' to
           ;; 'binary.  Otherwise `curl/http-body' and
           ;; `curl/http-headers' will fail.
           (coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           ;; For release version, uncomment below line.
           ;; (buffer-undo-list t)
           )
       (with-current-buffer ,buffer
         (buffer-disable-undo ,buffer)
         (kill-all-local-variables)     ; do I need this?
         (erase-buffer)
         (progn ,@body)))))


(defsubst curl/http-body (start end &optional target coding-system)
  "Return the body part of the HTTP response in string.

START and END are buffer positions for the scanning.

If optional TARGET is non-nil, the body part will be copied into TARGET buffer.
In this case, this function returns nil.  Otherwise, this function returns
the body part in string.

If optional CODING-SYSTEM is non-nil, this function will decode the
body content using CODING-SYSTEM."
  (goto-char start)

  (let ((cont t) status)
    (while (and cont (re-search-forward curl/status-line-regexp nil t))
      (setq status (match-string-no-properties 1))
      (goto-char (match-beginning 2))
      (if (not (string-match "\\`3[0-9][0-9]\\'" status))
          (setq cont nil))))

  (unless (search-forward curl/crlf-pair nil 'no-error)
    (error "malformed HTTP response; no CRLF between headers and body"))

  (setq start (point))              ; points the beginning of the body

  (save-restriction
    (narrow-to-region start end)
    (and coding-system
         (decode-coding-region (point-min) (point-max) coding-system))
    (if target
        (let ((oldbuf (current-buffer))
              (start (point-min))
              (end (point-max)))
          (with-current-buffer (get-buffer-create target)
            ;; TODO: what about the difference in regard to the
            ;; coding system between TARGET and current buffer?
            (insert-buffer-substring oldbuf start end))
          nil)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defsubst curl/http-headers (start end)
  "Return alist of HTTP response header.

START and END are buffer positions.

All keys and values are in string in the list. The key \"Status\"
holds the HTTP Status code in string.

If the any of the HTTP response Status-Code is 3xx, the buffer may contains
more than one HTTP Response.  In this case, this function parses only the
last HTTP Reponse."
  (goto-char start)

  (let ((cont t) status)
    (while (and cont (re-search-forward curl/status-line-regexp nil t))
      (setq start (match-beginning 0))
      (setq status (match-string-no-properties 1))
      (goto-char (match-beginning 2))
      (if (not (string-match "\\`3[0-9][0-9]\\'" status))
          (setq cont nil))))
  ;; The point points the last HTTP status-line (excluding CR/LF).

  (unless (search-forward curl/crlf-pair nil 'no-error)
    (error "malformed HTTP response; no CRLF between headers and body"))

  ;; END points the end of the headers
  (setq end (- (point) (length curl/crlf-pair)))

  (save-restriction
    (narrow-to-region start end)
    (decode-coding-region (point-min) (point-max) 'utf-8-dos)
    (let ((headers nil)
          (pos nil)
          (fields nil))    (goto-char (point-min))
          (when (looking-at "HTTP/[0-9.]+")
            (skip-syntax-forward "^ ")
            (skip-syntax-forward " ")
            (setq pos (point))
            (skip-syntax-forward "^ ")
            (push
             (cons "Status"
                   (buffer-substring-no-properties
                    pos (point)))
             headers)
            (forward-line 1))
          (while (not (eobp))
            (setq fields
                  (split-string (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))
                                ": "))
            (when (= 2 (length fields))
              (push
               (cons (first fields) (second fields))
               headers))
            (forward-line 1))
          headers)))


(defun curl/buffer-bytes (&optional buffer)
  "Return number of bytes in a buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (and buffer (set-buffer buffer))
      (widen)
      (1- (position-bytes (point-max))))))

(defun curl/content-type-coding-system (content-type &optional default)
  "Get the coding-system (symbol) from the Content-type HTTP value.

If there's no matching coding-system, this function returns DEFUALT."
  (let ((charset ""))
    (save-match-data
      (string-match 
       ";?[[:space:]]*charset[[:space:]]*=[[:space:]]*\\([^[:space:];]*\\)"
       content-type)
      (let ((match (match-string 1 content-type)))
        (and match
             (setq charset match))))
    (condition-case err
        (check-coding-system (intern (downcase charset)))
      (error default))))


(defun curl/http-send-buffer (http-method url buffer content-type
                                          &optional options
                                          &rest custom-headers)
  ;; TODO: how to specify charset?
  ;; 
  ;; If the user specify the charset explicitly in CONTENT-TYPE
  ;; (e.g. "application/json; charset=euc-kr"), set
  ;; `coding-system-for-write' to that value.
  ;;
  ;; If not, the best solution would be to determine the charset in
  ;; BUFFER, and append it to CONTENT-TYPE. <- Is this possible?

  ;; TODO: target buffer support?
  ;;       If the response will be large amount, it's better to store the
  ;;       result into some other buffer.
  (let ((cl (format "-H 'Content-Length: %d'" 
                    (if buffer (curl/buffer-bytes) 0)))
        (ct (if content-type 
                (format "-H 'Content-Type: %s'" content-type)
              ""))
        (hds "")
        curlcode)
    (dolist (hd custom-headers)
      (setq hds (concat hds (format " -H '%s'" hd))))

    (setq curlcode
          (shell-command-on-region
           (point-min) (point-max)
           (format "%s %s %s %s %s %s -i -X %s --data-binary @- '%s' 2>/dev/null"
                   curl/program curl/common-options
                   ct cl
                   (if options options "")
                   hds
                   (upcase (symbol-name http-method))
                   url)
           (current-buffer) 'replace))

    (unless (eq curlcode 0)
      (let ((msg (gethash curlcode curl/return-status-messages)))
        (error (or (format "CURL: %s" msg)
                   (format "CURL: returns %d" curlcode)))))

    (let* ((headers (curl/http-headers (point-min) (point-max)))
           (body (curl/http-body (point-min) (point-max) nil 'utf-8)))
      (cons headers body))))


(defun curl/sentinel (process event &optional sentinel)
  (save-match-data
    (cond ((string-equal event "finished\n")
           ;; process finished, do finalization
           (message "curl finished")
           (let ((buf (process-buffer process)))
             (message "proc buffer: %S" buf)
             (message "live-p: %S" (buffer-live-p buf))
             (message "local-p: %S" (local-variable-p 'curl-sentinel))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (local-variable-p 'curl-sentinel)
                     (message "calling local sentinel")
                     (funcall curl-sentinel process event))))))
          ((string-match "exited abnormally with code \\([0-9]*\\)\n" event)
           (let ((ecode (string-to-number (match-string 1 event))))
             ;; nonzero exit status, do something
             (let ((msg (gethash ecode curl/return-status-messages)))
               (message (or (format "CURL: %s" msg)
                            (format "CURL: returns %d" ecode))))))
          ((string-match "\\`hangup\\'" event)
           ;; user explicitly killed the process
           )
          (t ;; unhandled event
           (message "%S: %s" process event)))))
           
    
         
(defun curl/http-start-recv (http-method url
                                         &optional target-buffer sentinel
                                         options 
                                         &rest custom-headers)
  (let ((hds "") 
        proc
        (req (cond ((string-equal (upcase (symbol-name http-method)) "GET")
                    "")
                   ((string-equal (upcase (symbol-name http-method)) "HEAD")
                    "-I")
                   ((string-equal (upcase (symbol-name http-method)) "DELETE")
                    "-X DELETE")
                   (t (error "unknown method")))))
    (dolist (hd custom-headers)
      (setq hds (concat hds (format " -H '%s'" hd))))

    (upcase (symbol-name http-method))

    (curl/with-temp-buffer
      (let ((proc (start-process-shell-command 
                   curl/process-name (current-buffer)
                   (format "%s %s %s %s %s %s %s -i '%s' 2>/dev/null"
                           curl/program
                           curl/common-options
                           curl/cookie-options
                           req
                           (if options options "")
                           "-H 'Accept-Charset: UTF-8'"
                           hds
                           url))))
        (make-variable-buffer-local 'curl-sentinel)
        (setq curl-sentinel sentinel)
        (set-process-sentinel proc #'curl/sentinel)))))

(defun curl/http-recv (http-method url &optional target-buffer options
                                   &rest custom-headers)
  "Do HTTP request to URL for receiving data.

HTTP-METHOD is one of 'GET, 'HEAD or 'DELETE.

For OPTIONS argument, you can specify additional CURL options in
string.  CUSTOM-HEADERS are one or more HTTP custom headers in
string.

If TARGET-BUFFER is non-nil, this function copies the body of the
respond into TARGET-BUFFER, and returns a form, (HEADER-ALIST
. nil).

If TARGET-BUFFER is nil, this function returns a
form, (HEADER-ALIST . BODY).

HEADER-ALIST is a alist of HTTP response headers.  Among other
fields, the response code will be a member of HEADER-ALIST in the
form of (\"Status\" . Status-Code)."
  (let ((hds "") 
        (ecode 0)
        (req (cond ((string-equal (upcase (symbol-name http-method)) "GET")
                    "")
                   ((string-equal (upcase (symbol-name http-method)) "HEAD")
                    "-I")
                   ((string-equal (upcase (symbol-name http-method)) "DELETE")
                    "-X DELETE")
                   (t (error "unknown method")))))
    (dolist (hd custom-headers)
      (setq hds (concat hds (format " -H '%s'" hd))))

    (upcase (symbol-name http-method))

    (curl/with-temp-buffer
      ;; `shell-commaond-on-region' uses `display-message-or-buffer'
      ;; to display the buffer for the shell command, which is not
      ;; suitable for this function.  Thus, this function will provide
      ;; local function to shadow `display-message-or-buffer'.
      (flet ((display-message-or-buffer (msg &optional buffer not-this frame)))
        (setq ecode (shell-command-on-region
                     (point) (point)
                     (format "%s %s %s %s %s %s %s -i '%s' 2>/dev/null"
                             curl/program
                             curl/common-options
                             curl/cookie-options
                             req
                             (if options options "")
                             "-H 'Accept-Charset: UTF-8'"
                             hds
                             url)
                     (current-buffer))))

      ;; TODO: check the return status of curl(1)
      (unless (eq ecode 0)
        (let ((msg (gethash ecode curl/return-status-messages)))
          (error (or (format "CURL: %s" msg)
                     (format "CURL: returns %d" ecode)))))

      (let* ((headers (curl/http-headers (point-min) (point-max))))
        (if (not target-buffer)
            (cons headers (curl/http-body (point-min) (point-max) nil 'utf-8))
          (curl/http-body (point-min) (point-max) target-buffer 'utf-8)
          (cons headers nil))))))

;; (defun curl/http-get (url &rest headers)
;;   (let ((hd-options ""))
;;     (dolist (hd headers)
;;       (setq hds (concat hds (format " -H '%s'" hd))))

;;     (defmacro curl/with-temp-buffer(&rest body)
    
(provide 'curl)
;;; curl.el ends here
