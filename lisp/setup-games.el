;;; lisp/setup-games.el -*- lexical-binding: t; -*-

(global-unset-key (kbd "C-c g"))
;; Yahtzee
;; Fun dice game. Now I can get mad at Emacs instead of my sister.

(use-package! yahtzee
  :defer t
  :bind ("C-c g y" . yahtzee))
;; Sudoku
;; I love sudoku puzzles.

(use-package! sudoku

  :defer t
  :bind (("C-c g s" . sudoku)
  :map sudoku-mode-map
  ("j" . sudoku-move-point-down)
  ("J" . sudoku-move-point-downmost)
  ("k" . sudoku-move-point-up)
  ("K" . sudoku-move-point-upmost)
  ("h" . sudoku-move-point-left)
  ("H" . sudoku-move-point-leftmost)
  ("l" . sudoku-move-point-right)
  ("L" . sudoku-move-point-rightmost)

  ;; Start/quit/print game
  ("N" . sudoku)
  ("q" . sudoku-quit)
  ("Q" . sudoku-quit-immediately)
  ("P" . sudoku-print)

  ;; Undo/redo
  ("u"    . sudoku-undo)
  ("\C-r" . sudoku-redo)

  ;; Inserting values
  ("1" . sudoku-change-point)
  ("2" . sudoku-change-point)
  ("3" . sudoku-change-point)
  ("4" . sudoku-change-point)
  ("5" . sudoku-change-point)
  ("6" . sudoku-change-point)
  ("7" . sudoku-change-point)
  ("8" . sudoku-change-point)
  ("9" . sudoku-change-point)))
;; Tetris
;; Tetris is my childhood. No way I wouldn’t set it up to be nice and comfy.

(use-package! tetris
  :defer t
  :bind (("C-c g t" . 'tetris)
         :map tetris-mode-map
         ("J" . tetris-move-bottom)
         ("h" . tetris-move-left)
         ("j" . tetris-mode-down)
         ("l" . tetris-move-right)
         ([left] . tetris-rotate-next)
         ("r" . tetris-rotate-next)
         ([right] . tetris-rotate-prev)
         ("R" . tetris-rotate-prev)
         ([?\t] . tetris-pause-game)
         ("p" . tetris-pause-game)
         ("@" . tetris-start-game)
         ("Q" . tetris-end-game)))
  ;; Chess
  ;; Just for fun. I suck at chess but it’s nice to have.

  (use-package! chess

    :defer t
    :bind ("C-c g c" . chess))
  ;; 2048
  ;; A simple and fun game. Was a big deal when I was in high school. I still play it from time to time, to pass the time and remember my powers of 2.

  (use-package! 2048-game

    :defer t
    :bind (("C-c g 2" . 2048-game)
    :map 2048-mode-map
    ("j" . 2048-down)
    ("k" . 2048-up)
    ("h" . 2048-left)
    ("l" . 2048-right)))

  (use-package! pacmacs

    :defer t
    :bind (("C-c g p" . pacmacs)
    :map pacmacs-mode-map
    ("h" . pacmacs-left)
    ("j" . pacmacs-down)
    ("k" . pacmacs-up)
    ("l" . pacmacs-right)))
