(setq warning-minimum-level :error)
(load-theme 'wombat)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setq inhibit-startup-screen t)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-cannot-complete-command 'ido-next-match)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-<backspace>") 'ido-delete-backward-updir)
            (define-key ido-completion-map (kbd "DEL") 'delete-backward-char)))
(add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS16-24"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)  
(electric-indent-mode 1)
(global-set-key (kbd "TAB") 'indent-for-tab-command)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000000"))))
 '(fringe ((t (:background "#000000"))))
 '(mode-line ((t (:background "#202020" :foreground "#FFFFFF"))))
 '(mode-line-inactive ((t (:background "#101010" :foreground "#A0A0A0"))))
 '(whitespace-empty ((t (:background "#000000" :foreground "#202020"))))
 '(whitespace-indentation ((t (:background "#000000" :foreground "#202020"))))
 '(whitespace-newline ((t (:background "#000000" :foreground "#202020"))))
 '(whitespace-space ((t (:background "#000000" :foreground "#202020")))))

(set-face-attribute 'line-number-current-line nil :weight 'bold)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; ALIASES and FUNCTIONS ;;
(defun delete-whole-line (&optional arg)
  (interactive "P")
  (let ((arg (prefix-numeric-value arg)))
    (delete-region
     (save-excursion (beginning-of-line) (point))
     (save-excursion (beginning-of-line (1+ arg)) (point)))))

(defun delete-region-or-char ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char 1)))

(defun kill-ring-save-region ()
  (interactive "r")
  (kill-ring-save))

(defun kill-current-buffer ()
  (interactive) 
  (kill-buffer (current-buffer)))

(defalias 'bn 'next-buffer)
(defalias 'bp 'previous-buffer)
(defalias 'kw 'kill-buffer-and-window)
(defalias 'kb 'kill-current-buffer)
(defalias 'dw 'delete-window)

;; BINDINGS ;;
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-S-d"))
(global-unset-key (kbd "C-S-k"))
(global-set-key (kbd "C-d") 'delete-region-or-char)
(global-set-key (kbd "C-S-d") 'delete-whole-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(defun my-java-mode-hook-setup ()
  (define-key java-mode-map (kbd "C-d") 'delete-region-or-char)
  (define-key java-mode-map (kbd "C-S-d") 'delete-whole-line)
  (define-key java-mode-map (kbd "C-S-k") 'kill-whole-line))

(add-hook 'java-mode-hook 'my-java-mode-hook-setup)


;; PACKAGES ;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; simple c mode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; discord rich presence
(require 'elcord)
(elcord-mode)

;; java support
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; surround, like vim-surround
(use-package surround
  :ensure t
  :bind-keymap ("C-'" . surround-keymap))

;; custom welcome screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner "~/.emacs.d/menhera_chan_sticker.gif")
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content nil)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents . 3)))

;; music player in emacs
(setq lyrics-fetcher-genius-access-token (password-store-get "genius-client-access"))
(setq lyrics-fetcher-lyrics-folder (file-truename "~/music/lyrics/"))
(require 'emms-setup)
(require 'emms-info-mediainfo)
(require 'emms-mode-line-cycle)
(emms-all)
(emms-default-players)
(emms-mpris-enable)
(setq-default emms-player-list '(emms-player-mpv))
(setq-default emms-info-functions '(emms-info-native
				                    emms-info-metaflac
				                    emms-info-ogginfo
				                    emms-info-cueinfo
				                    emms-info-mediainfo))
(emms-mode-line-cycle 1)
(setq emms-mode-line-cycle-max-width 24)
(setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-config" "--force-window=no" "--audio-display=no" "--audio-device=pipewire"))
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
(emms-add-directory-tree (file-truename "~/music/"))
(emms-add-directory (file-truename "~/music/"))
(emms-playlist-current-clear) ;; i wanna keep my playlist empty

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emms-mode-line-cycle-velocity 1)
 '(emms-volume-change-function 'emms-volume-mpv-change)
 '(package-selected-packages
   '(d-mode dashboard elcord emms emms-info-mediainfo
	        emms-mode-line-cycle emms-player-simple-mpv emms-state
	        lsp-java lyrics-fetcher magit pass password-store surround)))

