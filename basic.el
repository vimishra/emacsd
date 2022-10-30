;; basic stuff
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq vc-handled-backends nil
         make-backup-files nil)

;; recentf
(require 'recentf)
(setq recentf-save-file "~/.recentf")
(recentf-mode 1)
(setq recentf-max-menu-items 20
      recentf-max-saved-items 1000)

;; ido
(ido-mode 1)
(setq ido-use-virtual-buffers t)

                                        ;: Basic font
(set-frame-font "CaskaydiaCove Nerd Font 18"  nil t)

(load-theme 'wombat t)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
