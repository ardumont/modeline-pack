;;; modeline-pack.el --- Modeline setup

;;; Commentary:

;;; Code:

(use-package dash)
(use-package s)
(use-package diminish)

(defvar modeline-pack-clean-mode-alist
  `((auto-complete-mode       " α")
    (yas-minor-mode           " γ")
    (paredit-mode             " Φ")
    (eldoc-mode               "")
    (abbrev-mode              "")
    (guide-key-mode           "")
    (guru-mode                "")
    (undo-tree-mode           " τ")
    (volatile-highlights-mode " υ")
    (elisp-slime-nav-mode     " δ")
    (nrepl-mode               " ηζ")
    (nrepl-interaction-mode   " ηζ")
    (cider-mode               " ηζ")
    (cider-interaction        " ηζ")
    ;; Major modes
    (clojure-mode             "cλ")
    (hi-lock-mode             "")
    (python-mode              "pλ")
    (emacs-lisp-mode          "eλ")
    (markdown-mode            "md")
    (magit                    "ma")
    (haskell-mode             "hλa")
    (tuareg-mode              "mλ")
    (flymake-mode             "fm")
    (js2-mode                 "jλ")
    (company-mode             " Ψ"))
  "List of modes, clean mode string representation.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline instead of the original.")

(defun modeline-pack--log (&rest args)
  "Log the message ARGS in the mini-buffer."
  (apply #'message (format "Modeline Pack - %s" (car args)) (cdr args)))

(defun modeline-pack-clean-mode-line ()
  "Pretty symbol for the modeline."
  (interactive)
  (-map (lambda (mmode)
          (-let (((mode mode-str) mmode))
            (if (eq mode major-mode)
                (setq mode-name (s-trim mode-str))
              (when (assq mode minor-mode-alist)
                (diminish mode (s-trim-right mode-str))))))
        modeline-pack-clean-mode-alist))

(defun modeline-pack--abbrev-to-mode-name (abbrev-name)
  "Given an ABBREV-NAME, return the corresponding mode if it exists."
  (-filter (lambda (mmode)
             (-let (((mode s) mmode))
               (and (stringp s) (string-match-p abbrev-name s)))) minor-mode-alist))

(defun modeline-pack-abbrev-to-mode-name ()
  "Discover the full name of a modeline abbreviation."
  (interactive)
  (let ((abbrev (read-string "modeline abbreviation: ")))
    (->> abbrev
         modeline-pack--abbrev-to-mode-name
         (modeline-pack--log "%s -> %s" abbrev))))

(modeline-pack-clean-mode-line)

;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ

(provide 'modeline-pack)
;;; modeline-pack ends here
