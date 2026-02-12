;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "emms" "20260113.1922"
  "The Emacs Multimedia System."
  '((cl-lib  "0.5")
    (nadvice "0.3")
    (seq     "0"))
  :url "https://www.gnu.org/software/emms/"
  :commit "1d48a1133db2670d839f2cf2aff17d6c32aeb15f"
  :revdesc "1d48a1133db2"
  :keywords '("emms" "mp3" "ogg" "flac" "music" "mpeg" "video" "multimedia")
  :authors '(("Jorgen Schäfer" . "forcer@forcix.cx"))
  :maintainers '(("Yoni Rabkin" . "yrk@gnu.org")))
