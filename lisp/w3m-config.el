;;; lisp/w3m-config.el --- -*- lexical-binding: t; -*-
;;set w3m as def browser
(setq browse-url-browser-function 'w3m-browse-url)

;;Open a url embedded in any buffer
 (setq browse-url-browser-function  'w3m-goto-url-new-session)

 (autoload 'w3m-browse-url  "w3m"  "Ask a WWW browser to show a URL." t)
  ;; optional keyboard short-cut
 ;; (global-set-key  " \C-xm" 'browse-url-at-point)
;; Basic Setup:1 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMAuthentication\]\[EmacsWiki: W Three M Authentication\]\]][[[https://www.emacswiki.org/emacs/WThreeMAuthentication][EmacsWiki: W Three M Authentication]]:1]]
;;[[https://www.emacswiki.org/emacs/WThreeM][WThreeM]] has a problem when you don't get the username/password right for http authentication. It basically ends up in la-la land. If you try to C-g at the username/password prompt it ends up in la-la-land.

;;Hers's some code I wrote that will allow w3m.el to “forget” authentication info so that you can try to authenticate again with a different username/password:

(defun w3m-erase-authinfo-root (root)
  (setq w3m-process-authinfo-alist
        (assq-delete-all
         nil (mapcar
              (lambda (elem) (if (not (equal root (car elem))) elem))
              w3m-process-authinfo-alist))))

(defun w3m-forget-authinfo ()
  (interactive)
  (let* ((roots (mapcar
                 (lambda (elem) (list (car elem) (car elem)))
                 w3m-process-authinfo-alist))
         (root (completing-read "URL: " roots nil t)))
    (w3m-erase-authinfo-root root)))
;; [[https://www.emacswiki.org/emacs/WThreeMAuthentication][EmacsWiki: W Three M Authentication]]:1 ends here

;; [[file:w3m-config.org::*Spawning a new buffer][Spawning a new buffer:1]]
(defun  w3m-new-buffer nil
    "Opens a new, empty w3m buffer."
    "As opposed to `w3m-copy-buffer', which opens a non-empty buffer.
 This ought to be snappier, as the old buffer needs not to be render To be quite honest, this new function doesn't open a buffer completely
 empty, but visits the about: pseudo-URI that is going to have to
 suffice for now."
   (interactive)
   (w3m-goto-url-new-session  "about://"))
;; Spawning a new buffer:1 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMTypeAhead\]\[EmacsWiki: W Three M Type Ahead\]\]][[[https://www.emacswiki.org/emacs/WThreeMTypeAhead][EmacsWiki: W Three M Type Ahead]]:1]]
;;w3m-type-ahead.el  can be used to add type-ahead functionality (as seen, for example, in Mozilla and related browsers) to =w3m-mode=. After loading w3m and w3m-type-ahead, it can be switched on as a minor mode in =w3m-mode= buffers using:

;;By default, this binds / to =w3m-type-ahead= and M-/ to =w3m-type-ahead-regexp=. With these, incremental searching limited to the text associated with links can be done. Use a prefix argument of =C-u 2= or =C-u C-u= to create a new session.

 (add-hook 'w3m-mode-hook 'w3m-type-ahead-mode)
;; [[https://www.emacswiki.org/emacs/WThreeMTypeAhead][EmacsWiki: W Three M Type Ahead]]:1 ends here

;; [[file:w3m-config.org::*Make the previous search engine the default for the next search][Make the previous search engine the default for the next search:1]]
;;Makes the previous search engine to be default for the next search
(setq w3m-search-default-engine "duckduckgo")
(defadvice  w3m-search (after change-default activate)
      (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))
;; Make the previous search engine the default for the next search:1 ends here

;; [[file:w3m-config.org::*Weird Characters][Weird Characters:1]]
(setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)
;; Weird Characters:1 ends here

;; [[file:w3m-config.org::*Filtering To Downcast To ASCII][Filtering To Downcast To ASCII:2]]
(setq w3m-use-filter t)
 ;; send all pages through one filter
(setq w3m-filter-rules `(( " \\`.+" w3m-filter-all)))

( defun  w3m-filter-all (url)
  ( let ((list '(
                 ;; add more as you see fit!
                ( "» ;"  " >; >;")
                ( " «  class="comment">;"  " <;")
                ( " »  class="comment">;"  " >;")
                ( " ö  class="comment">;"  "o")
                ( "… ;"  "...")
                ( "‘ ;"  "'")
                ( "’ ;"  "'")
                ( " &rsquo  class="comment">;"  "'")
                ( " &lsquo  class="comment">;"  "'")
                ( " \u2019"  " \'")
                ( " \u2018"  " \'")
                ( " \u201c"  " \"")
                ( " \u201d"  " \"")
                ( " &rdquo  class="comment">;"  " \"")
                ( " &ldquo  class="comment">;"  " \"")
                ( "“ ;"  " \"")
                ( "” ;"  " \"")
                ( " \u2013"  "-")
                ( " \u2014"  "-")
                ( "– ;"  "-")
                ( "— ;"  "-")
                ( " &ndash  class="comment">;"  "-")
                ( " &mdash  class="comment">;"  "-")
                )))
  ( while list
    ( let ((pat (car (car list)))
          (rep (car (cdr (car list)))))
      (goto-char (point-min))
      ( while (search-forward pat nil t)
        (replace-match rep))
      (setq list (cdr list))))))
;; Filtering To Downcast To ASCII:2 ends here

;; [[file:w3m-config.org::*Disable default c-x b behaviour][Disable default c-x b behaviour:1]]
;; Disable default c-x b behaviour
 (add-hook 'w3m-mode-hook ( lambda () (define-key w3m-mode-map (kbd  "C-x b") nil)))
;; Disable default c-x b behaviour:1 ends here

;; [[file:w3m-config.org::*Use isearch for link following][Use isearch for link following:1]]
  (defvar w3m-isearch-links-do-wrap nil
    "Used internally for fast search wrapping.")
;; Use isearch for link following:1 ends here

;; [[file:w3m-config.org::*Use isearch for link following][Use isearch for link following:2]]
  (defun w3m-isearch-links (&optional regexp)
    (interactive "P")
    (let ((isearch-wrap-function
	   #'(lambda ()
	       (setq w3m-isearch-links-do-wrap nil)
	       (if isearch-forward
		   (goto-char (window-start))
		 (goto-char (window-end)))))
	  (isearch-search-fun-function
	   #'(lambda () 'w3m-isearch-links-search-fun))
	  post-command-hook		;inhibit link echoing
	  do-follow-link
	  (isearch-mode-end-hook
	   (list  #'(lambda nil
		      (when (and (not isearch-mode-end-hook-quit)
				 (w3m-anchor))
			(setq do-follow-link t))))))
      (setq w3m-isearch-links-do-wrap t)
      (isearch-mode t
		    regexp
		    ;; fast wrap
		    #'(lambda nil
			(if isearch-success
			    (setq w3m-isearch-links-do-wrap t)
			  (when w3m-isearch-links-do-wrap
			    (setq w3m-isearch-links-do-wrap nil)
			    (setq isearch-forward
				  (not isearch-forward))
			    (isearch-repeat isearch-forward))))
		    t)
      (when do-follow-link
	(w3m-view-this-url))))
;; Use isearch for link following:2 ends here

;; [[file:w3m-config.org::*Use isearch for link following][Use isearch for link following:3]]
  (defun w3m-isearch-links-search-fun (string &optional bound no-error)
    (let* (isearch-search-fun-function
	   (search-fun  (isearch-search-fun))
	   error
	   (bound  (if isearch-forward
		       (max (or bound 0)
			    (window-end))
		     (min (or bound (window-start))
			  (window-start)))))
      (condition-case err
	  (while (and (apply search-fun (list string bound))
		      (not (w3m-anchor (point)))))
	(error (setq error err)))
      (if error
	  (if (not no-error)
	      (signal (car error) (cadr error)))
	(point))))
;; Use isearch for link following:3 ends here

;; [[file:w3m-config.org::*Use isearch for link following][Use isearch for link following:4]]
  (require ' w3m)
  (define-key w3m-mode-map [?f] 'w3m-isearch-links)
;; Use isearch for link following:4 ends here

;; [[file:w3m-config.org::*Removing trailing whitespace][Removing trailing whitespace:1]]
    (add-hook 'w3m-display-hook
              ( lambda (url)
                ( let ((buffer-read-only nil))
                  (delete-trailing-whitespace))))
;; Removing trailing whitespace:1 ends here

;; [[file:w3m-config.org::*Consistent browse-url in buffers/w3m/gnus][Consistent browse-url in buffers/w3m/gnus:1]]
(defun  rgr/browse (url)
   "If prefix is specified use the system default browser else use the configured emacs one"
  (if current-prefix-arg
      (when url (browse-url-default-browser url))
    (if  url (browse-url url) (call-interactively 'browse-url))
    ))

(defun  rgr/browse-url ( &optional url)
   "browse the url passed in"
  (interactive)
  (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
  (setq url (read-string (format  "Url  \"%s \" :" url) url nil url))
  (rgr/browse url))
;; Consistent browse-url in buffers/w3m/gnus:1 ends here

;; [[file:w3m-config.org::*Middle click to open url in a new sesstion (tab)][Middle click to open url in a new sesstion (tab):1]]
;; Middle click to open url in a new sesstion (tab)
    (define-key w3m-mode-map [mouse-2] 'w3m-mouse-view-this-url-new-session)
;; Middle click to open url in a new sesstion (tab):1 ends here

;; [[file:w3m-config.org::*Improving w3m-browse-*][Improving w3m-browse-*:1]]
 ;; make browse-url-url-at-point use w3m links if they exist
(defadvice  browse-url-url-at-point (after w3m-anchor-at-point activate)
     "Browse the url at point. If w3m-anchor finds a url, use it."
    (setq ad-return-value
                (or
                 (w3m-anchor)
                 (ad-return-value))))
;; Improving w3m-browse-*:1 ends here

;; [[file:w3m-config.org::*Conkeror/Vimperator functionality][Conkeror/Vimperator functionality:1]]
(w3m-lnum-mode 1)
(add-hook 'w3m-mode-hook 'w3m-lnum-mode)
;; Conkeror/Vimperator functionality:1 ends here

;; [[file:w3m-config.org::*Browse url on other window][Browse url on other window:1]]
(setq browse-url-browser-function 'w3m-browse-url-other-window)

(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))
;; Browse url on other window:1 ends here

;; [[file:w3m-config.org::*Browse url on other window][Browse url on other window:2]]
(defun ffap-w3m-other-window (url &optional new-session)
  "Browse url in w3m.
  If current frame has only one window, create a new window and browse the webpage"
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "Emacs-w3m URL: ")))
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url new-session)))
;; Browse url on other window:2 ends here

;; [[file:w3m-config.org::*Google Suggest][Google Suggest:1]]
    (defun google-suggest ()
     "Search `w3m-search-default-engine' with google completion canditates."
     (interactive)
     (w3m-search w3m-search-default-engine
		 (completing-read  "Google search: "
				   (dynamic-completion-table
				   'google-suggest-aux))))
;; Google Suggest:1 ends here

;; [[file:w3m-config.org::*Google Suggest][Google Suggest:2]]
   (defun google-suggest-aux (input)
     (with-temp-buffer
       (insert
	(shell-command-to-string
	 (format "w3m -dump_source %s"
		 (shell-quote-argument
		  (format
		   "http://www.google.com/complete/search?hl=en&js=true&qu=%s"
		   input)))))
       (read
	(replace-regexp-in-string "," ""
				  (progn
				    (goto-char (point-min))
				    (re-search-forward "\(" (point-max) t 2)
				    (backward-char 1)
				    (forward-sexp)
				    (buffer-substring-no-properties
				     (1- (match-end 0)) (point)))))))
;; Google Suggest:2 ends here

;; [[file:w3m-config.org::*Using TextMode for textareas][Using TextMode for textareas:1]]
 (eval-after-load "w3m-form"
  '(progn
     (define-minor-mode dme:w3m-textarea-mode
       "Minor mode used when editing w3m textareas."
       nil " dme:w3m-textarea" w3m-form-input-textarea-map)
;; Using TextMode for textareas:1 ends here

;; [[file:w3m-config.org::*Using TextMode for textareas][Using TextMode for textareas:2]]
     (defun dme:w3m-textarea-hook ()
       ; protect the form local variables from being killed by `text-mode'
       (mapcar (lambda (v)
		 (if (string-match "^w3m-form-input-textarea.*"
				   (symbol-name (car v)))
		     (put (car v) 'permanent-local t)))
	       (buffer-local-variables))
       (text-mode)
       (dme:w3m-textarea-mode))
;; Using TextMode for textareas:2 ends here

;; [[file:w3m-config.org::*Using TextMode for textareas][Using TextMode for textareas:3]]
  (add-hook 'w3m-form-input-textarea-mode-hook 'dme:w3m-textarea-hook)))
;; Using TextMode for textareas:3 ends here

;; [[file:w3m-config.org::*Using TextMode for textareas][Using TextMode for textareas:4]]
 (defun dka-w3m-textarea-hook()
  (save-excursion
    (while (re-search-forward "\r\n" nil t)
      (replace-match "\n" nil nil))
    (delete-other-windows)))
;; Using TextMode for textareas:4 ends here

;; [[file:w3m-config.org::*Easy forward/back in a new buffer][Easy forward/back in a new buffer:1]]
    (defun w3m-view-previous-page-2 (&optional count)
      "Move back count pages in the history.
    If `count' is a positive integer, move backward count times in the
    history.  If `count' is a negative integer, moving forward is performed.
    `count' is treated as 1 by default if it is omitted.
;; Easy forward/back in a new buffer:1 ends here

;; [[file:w3m-config.org::*Easy forward/back in a new buffer][Easy forward/back in a new buffer:2]]
    If `count' is 16 (C-u C-u), it will open in a new buffer. If it is greater
    than 16, it will open in a new buffer and move backward `count' - 16 times."
      (interactive "p")
      (cond ((not count)
    	     (setq count 1))
    	    ((= count 16)
    	     (setq count 1)
    	     (w3m-copy-buffer))
    	    ((> count 16)
    	     (setq count (- count 16))
    	     (w3m-copy-buffer)))
      (w3m-view-previous-page count))
;; Easy forward/back in a new buffer:2 ends here

;; [[file:w3m-config.org::*Easy forward/back in a new buffer][Easy forward/back in a new buffer:3]]
    (defun w3m-view-next-page-2 (&optional count)
      "Move back count pages in the history.
    If `count' is a positive integer, move forward count times in the
    history.  If `count' is a negative integer, moving backwards is performed.
    `count' is treated as 1 by default if it is omitted.
;; Easy forward/back in a new buffer:3 ends here

;; [[file:w3m-config.org::*Easy forward/back in a new buffer][Easy forward/back in a new buffer:4]]
    If ` count' is 16 (C-u C-u), it will open in a new buffer. If it is greater
    than 16, it will open in a new buffer and move forward ` count' - 16 times. "
      (interactive "p")
      ( cond ((not count)
             (setq count 1))
            ((= count 16)
             (setq count 1)
             (w3m-copy-buffer))
            ((> count 16)
             (setq count (- count 16))
             (w3m-copy-buffer)))
       (w3m-view-next-page count))
;; Easy forward/back in a new buffer:4 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMDelicious\]\[EmacsWiki: W Three M Delicious\]\]][[[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:1]]
(defun delicious-post-url ()
  (interactive)
  (if (null (w3m-anchor))
      (message "no anchor at point")
    (let ((url (w3m-anchor)))
      (if (w3m-url-valid url)
      (progn
            (w3m-goto-url (concat "http://delicious.com/save?url=" url)))
    (message "no URL at point!")))))
;; [[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:1 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMDelicious\]\[EmacsWiki: W Three M Delicious\]\]][[[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:2]]
(setq w3m-use-cookies t)
 (setq w3m-cookie-accept-bad-cookies t)
;; [[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:2 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMDelicious\]\[EmacsWiki: W Three M Delicious\]\]][[[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:3]]
(defun ted-delicious-url ()
   "Bookmark this page with del.icio.us."
   (interactive)
   (w3m-goto-url
    (concat "http://del.icio.us/hober?"
            "url="    (w3m-url-encode-string w3m-current-url)
            "&title=" (w3m-url-encode-string w3m-current-title))))
;; [[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:3 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMDelicious\]\[EmacsWiki: W Three M Delicious\]\]][[[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:4]]
(eval-after-load "w3m"
   '(define-key w3m-info-like-map "a" 'ted-delicious-url))
;; [[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:4 ends here

;; [[file:w3m-config.org::*\[\[https:/www.emacswiki.org/emacs/WThreeMDelicious\]\[EmacsWiki: W Three M Delicious\]\]][[[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:5]]
(defun /john-delicious-url ()
   "Post either the url under point or the url of the current w3m page to delicious."
   (interactive)
   (let ((w3m-async-exec nil))
     (if (thing-at-point-url-at-point)
         (unless (eq (current-buffer) (w3m-alive-p))
           (w3m-goto-url (thing-at-point-url-at-point))))
     (w3m-goto-url
      (concat "http://del.icio.us/johnsu01?"
              "url="    (w3m-url-encode-string w3m-current-url)
              "&title=" (w3m-url-encode-string w3m-current-title)))))
;; [[https://www.emacswiki.org/emacs/WThreeMDelicious][EmacsWiki: W Three M Delicious]]:5 ends here

;; [[file:w3m-config.org::*uri-replace-alist][uri-replace-alist:1]]
(setq w3m-uri-replac-alist
'(("\\`gg:" w3m-search-uri-replace "google")
 ("\\`ya:" w3m-search-uri-replace "yahoo")
 ("\\`bts:" w3m-search-uri-replace "debian-bts")
 ("\\`dpkg:" w3m-search-uri-replace "debian-pkg")
 ("\\`alc:" w3m-search-uri-replace "alc")
 ("\\`gvd:" w3m-search-uri-replace "video")
 ("\\`ddg:" w3m-search-uri-replace "duckduckgo")
 ("\\`13:" w3m-search-uri-replace "1377x")
 ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace "http://www.ietf.org/rfc/rfc\\1.txt")))
;; uri-replace-alist:1 ends here

;; [[file:w3m-config.org::*engine list][engine list:1]]
(setq w3m-search-engine-alist
'(("yahoo" "https://search.yahoo.com/bin/search?p=%s")
 ("yahoo-ja" "https://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
 ("alc" "https://eow.alc.co.jp/%s/UTF-8/" utf-8)
 ("blog" "https://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8)
 ("blog-en" "https://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8)
 ("google" "https://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&gbv=1" utf-8)
 ("google-en" "https://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8&gbv=1" utf-8)
 ("google news" "https://news.google.com/news?q=%s&ie=utf-8&oe=utf-8" utf-8)
 ("google news-en" "https://news.google.com/news?q=%s&hl=en&ie=utf-8&oe=utf-8")
 ("technorati" "https://www.technorati.com/search/%s" utf-8)
 ("technorati-ja" "https://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8)
 ("technorati-tag" "https://www.technorati.com/tag/%s" utf-8)
 ("goo-ja" "https://search.goo.ne.jp/web.jsp?MT=%s" euc-japan)
 ("excite-ja" "https://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis)
 ("altavista" "https://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search")
 ("rpmfind" "https://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
 ("debian-pkg" "https://packages.debian.org/search?&searchon=names&suite=stable&section=all&arch=amd64&keywords=%s")
 ("debian-bts" "https://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s")
 ("freebsd-users-jp" "https://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan)
 ("iij-archie" "https://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice")
 ("waei" "https://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan)
 ("eiwa" "https://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej")
 ("kokugo" "https://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan)
 ("eiei" "https://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67")
 ("amazon" "https://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s")
 ("amazon-ja" "https://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis)
 ("emacswiki" "https://www.emacswiki.org/cgi-bin/wiki?search=%s")
 ("en.wikipedia" "https://en.wikipedia.org/w/index.php?title=Special:Search&search=%s")
 ("de.wikipedia" "https://de.wikipedia.org/w/index.php?title=Spezial:Suche&search=%s" utf-8)
 ("ja.wikipedia" "https://ja.wikipedia.org/w/index.php?title=Special:Search&search=%s" utf-8)
 ("msdn" "https://search.msdn.microsoft.com/search/default.aspx?query=%s")
  ("duckduckgo" "https://lite.duckduckgo.com/lite/?q=%s&kf=-1&kz=-1&kq=-1&kv=-1&k1=-1&kp=-2&kaf=1&kd=-1" utf-8)
  ("github" "https://github.com/search?ref=simplesearch&q=%s" utf-8)
  ("cern-gitlab" "https://gitlab.cern.ch/search?search=%s" utf-8)
  ("duckduckgo-first" "https://duckduckgo.com/html?q=\\%s" utf-8)
  ("google-maps" "https://www.google.com/maps/search/%s/" utf-8)
  ("openstreetmap" "https://www.openstreetmap.org/search?query=%s" utf-8)
  ("wordreference" "https://www.wordreference.com/es/translation.asp?tranword=%s" utf-8)
  ("wikipedia" "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" utf-8)
  ("youtube" "http://www.youtube.com/results?aq=f&oq=&search_query=%s" utf-8)
  ("invidious" "https://invidious.garudalinux.org/search?q=%s" utf-8)
  ("url" "https:%s" utf-8)
  ("bing https://www.bing.com/search?q=%s" utf-8)
  ("wiktionary https://en.wiktionary.org/w/index.php?search=%s" utf-8)
  ("reddit https://www.reddit.com/search/?q=%s" utf-8)
  ("amazon https://www.amazon.in/s?k=%s" utf-8)
  ("archaur https://aur.archlinux.org/packages/?O=0&K=%s" utf-8)
  ("archpackages https://archlinux.org/packages/?sort=&q=%s" utf-8)
  ("archlinux https://wiki.archlinux.org/index.php?search=%s" utf-8)
  ("gitlab https://gitlab.com/search?search=%s" utf-8)
  ("opensource https://opensource.google/projects/search?q=%s" utf-8)
  ("sourceforge https://sourceforge.net/directory/?q=%s" utf-8)
  ("stackoverflow https://stackoverflow.com/search?q=%s" utf-8)
  ("craigslist" "https://www.craigslist.org/search/sss?query=" utf-8)
  ("gumtree" "https://www.gumtree.com/search?search_category=all&q=" utf-8)
  ("brave" "https://search.brave.com/search?q=" utf-8)
  ("gemini" "https://portal.mozz.us/gemini/geminispace.info/search%3F" utf-8)
  ("qwant" "https://www.qwant.com/?q=" utf-8)
  ("swisscows" "https://swisscows.com/web?query=" utf-8)
  ("acronym"  "[[http://www.acronymfinder.com/af-query.asp?acronym=%s&string=exact]]")
  ("ebay" "[[http://search.ebay.com/search/search.dll?query=%s]]")
  ("google-groups" "[[http://www.google.com/groups?q=%s]]")
  ("syndic8" "[[http://www.syndic8.com/feedlist.php?ShowStatus=all&ShowMatch=%s]]")
  ("weather"  "[[http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared]]")
  ("worldclock" "[[http://www.timeanddate.com/worldclock/results.html?query=%s]]")
  ("yandex" "https://yandex.com/search/?text=" utf-8)
  ("bbc" "https://www.bbc.co.uk/search?q=" utf-8)
  ("cnn" "https://www.cnn.com/search?q=" utf-8)
  ("video" "https://www.google.com/search?q=%s&num=30&safe=off&tbo=p&tbm=vid" utf-8)
  ("archive" "https://archive.org/search.php?query=%s" utf-8)
  ("btdig" "https://btdig.com/search?q=%s" utf-8)
  ("dailymotion" "https://dailymotion.com/search/%s/videos" utf-8)
  ("eztv" "https://eztv.re/search/%s/1/" utf-8)
  ("iplayer" "https://bbc.co.uk/iplayer/search?q=%s" utf-8)
  ("limetorrents" "https://www.limetorrents.info/search/all/%s" utf-8)
  ("rarbg" "https://rarbg.to/torrents.php?search=%s&order=seeders&by=DESC" utf-8)
  ("sunxdcc" "http://sunxdcc.com/?sterm=%s%20-tar%20-rar" utf-8)
  ("thepacket" "https://thepacket.info/?q=%s" utf-8)
  ("torrentdownloads" "https://www.torrentdownloads.pro/search/?search=%s" utf-8)
  ("torrentgalaxy" "https://torrentgalaxy.to/torrents.php?search=%s" utf-8)
  ("vimeo" "https://vimeo.com/search?q=%s" utf-8)
  ("xdcceu" "http://www.xdcc.eu/search.php?searchkey=%s" utf-8)
  ("1377x" "https://1337x.to/search/%s/1/" utf-8)))
;; engine list:1 ends here
