;;; lisp/setup-engine-mode.el -*- lexical-binding: t; -*-

   ;; :browser 'browse-url-qutebrowser) this can be used to open specific urls with specific brouwser

(use-package! engine-mode
  :defer t
  :config
  (defengine duckduckgo
    "https://lite.duckduckgo.com/lite/?q=%s&kf=-1&kz=-1&kq=-1&kv=-1&k1=-1&kp=-2&kaf=1&kd=-1"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
:keybinding "g")
  (defengine cern-gitlab
    "https://gitlab.cern.ch/search?search=%s"
    :keybinding "l")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "O")
  (defengine duckduckgo-first
    "https://duckduckgo.com/html?q=\\%s"
    :keybinding "f")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/"
    :keybinding "M")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wordreference
    "https://www.wordreference.com/es/translation.asp?tranword=%s"
    :keybinding "r")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"
   :browser 'browse-url-firefox)
  (defengine invidious
    "https://invidious.garudalinux.org/search?q=%s"
    :keybinding "v")
  (defengine url
    "https:%s"
    :keybinding "u")


;; # Search Engines

;; websearch[bing]="https://www.bing.com/search?q="
  (defengine bing
"https://www.bing.com/search?q=%s"
:keybinding "b")
;; websearch[googlenews]="https://news.google.com/search?q="
  (defengine googlenews
"https://news.google.com/search?q=%s"
:keybinding "n")


;; websearch[wiktionary]="https://en.wiktionary.org/w/index.php?search="
  (defengine wiktionary
"https://en.wiktionary.org/w/index.php?search=%s"
:keybinding "D")

;; # Social Media
;; websearch[reddit]="https://www.reddit.com/search/?q="
  (defengine reddit
"https://www.reddit.com/search/?q=%s"
:keybinding "r")

;; # Online Shopping
;; websearch[amazon]="https://www.amazon.in/s?k="
  (defengine amazon
"https://www.amazon.in/s?k=%s"
:keybinding "a"
:browser 'browse-url-firefox)

;; websearch[ebay]="https://www.ebay.com/sch/i.html?&_nkw="
  (defengine ebay
"https://www.ebay.com/sch/i.html?&_nkw=%s"
:keybinding "e")
;; # Linux
;; websearch[archaur]="https://aur.archlinux.org/packages/?O=0&K="
  (defengine archaur
"https://aur.archlinux.org/packages/?O=0&K=%s"
:keybinding "U")

;; websearch[archpkg]="https://archlinux.org/packages/?sort=&q="
  (defengine archpackages
"https://archlinux.org/packages/?sort=&q=%s"
:keybinding "P")

;; websearch[archwiki]="https://wiki.archlinux.org/index.php?search="
  (defengine archlinux
"https://wiki.archlinux.org/index.php?search=%s"
:keybinding "W")

;; websearch[gitlab]="https://gitlab.com/search?search="
  (defengine gitlab
"https://gitlab.com/search?search=%s"
:keybinding "G")

;; websearch[googleOpenSource]="https://opensource.google/projects/search?q="
  (defengine opensource
"https://opensource.google/projects/search?q=%s"
:keybinding "o")

;; websearch[sourceforge]="https://sourceforge.net/directory/?q="
  (defengine sourceforge
"https://sourceforge.net/directory/?q=%s"
:keybinding "S")

;; websearch[stackoverflow]="https://stackoverflow.com/search?q="
  (defengine stackoverflow
"https://stackoverflow.com/search?q=%s"
:keybinding "s")

;; websearch[craigslist]="https://www.craigslist.org/search/sss?query="
;;   (defengine craigslist
;; "https://www.craigslist.org/search/sss?query="
;; :keybinding "c")


;; websearch[gumtree]="https://www.gumtree.com/search?search_category=all&q="
;;   (defengine gumtree
;; "https://www.gumtree.com/search?search_category=all&q="
;; :keybinding "g")


;; websearch[debianpkg]="https://packages.debian.org/search?suite=default&section=all&arch=any&searchon=names&keywords="
;;   (defengine debian
;; "https://packages.debian.org/search?suite=default&section=all&arch=any&searchon=names&keywords="
;; :keybinding "D")

;; websearch[brave]="https://search.brave.com/search?q="
;;   (defengine brave
;; "https://search.brave.com/search?q="
;; :keybinding "b")


;; websearch[gemini search \(https\)]="https://portal.mozz.us/gemini/geminispace.info/search%3F"
;;   (defengine gemini
;; "https://portal.mozz.us/gemini/geminispace.info/search%3F"
;; :keybinding "G")


;; websearch[qwant]="https://www.qwant.com/?q="
;;   (defengine qwant
;; "https://www.qwant.com/?q="
;; :keybinding "q")

;; websearch[swisscows]="https://swisscows.com/web?query="
;;   (defengine swisscows
;; "https://swisscows.com/web?query="
;; :keybinding "s")

;; websearch[yandex]="https://yandex.com/search/?text="
;;   (defengine yandex
;; "https://yandex.com/search/?text="
;; :keybinding "Y")

;; # Information/News
;; websearch[bbcnews]="https://www.bbc.co.uk/search?q="
;;   (defengine bbc
;; "https://www.bbc.co.uk/search?q="
;; :keybinding "b")

;; websearch[cnn]="https://www.cnn.com/search?q="
;;   (defengine cnn
;; "https://www.cnn.com/search?q="
;; :keybinding "c")

  (engine/set-keymap-prefix (kbd "C-c w "))
  (engine/set-keymap-prefix (kbd "s-b"))
  (engine-mode t))
