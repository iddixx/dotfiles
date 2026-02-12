;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "pass" "20260109.1144"
  "Major mode for password-store.el."
  '((emacs              "25.1")
    (password-store     "1.7.4")
    (password-store-otp "0.1.5")
    (f                  "0.17"))
  :url "https://github.com/NicolasPetton/pass"
  :commit "de4adfaeba5eb4d1facaf75f582f1ba36373299a"
  :revdesc "de4adfaeba5e"
  :keywords '("tools" "files")
  :authors '(("Nicolas Petton" . "petton.nicolas@gmail.com")
             ("Damien Cassou" . "damien@cassou.me"))
  :maintainers '(("Nicolas Petton" . "petton.nicolas@gmail.com")
                 ("Damien Cassou" . "damien@cassou.me")))
