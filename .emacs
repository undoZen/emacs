(set-language-environment 'English)

(custom-set-variables
 '(js2-strict-missing-semi-warning nil t)
 '(scroll-bar-mode 'right))

(customize-set-variable 'scroll-bar-mode 'right)
(add-to-list 'load-path "~/.emacs.d/")

(when (display-graphic-p)
  (require 'color-theme)
  (color-theme-initialize)
  (load-file "~/.emacs.d/color-theme-irblack.el")
  (color-theme-irblack)

  (when (eq system-type 'darwin)
    (set-face-attribute
     'default nil :font "Verily Serif Mono 13")
;     'default nil :font "Luxi Mono 13")

    ;; Chinese Font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "FZYaSongS-L-GB" :size 13)))))

(setq-default line-spacing 2)

;; Mac 下置换 option 和 command 键
;; 建议直接在系统偏好设置里面更改
;(when (eq system-type 'darwin)
;  (setq mac-command-modifier 'meta)
;  (setq mac-option-modifier 'super))

;; 一打开就启用 text 模式。 
(setq default-major-mode 'text-mode)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; 语法高亮
(global-font-lock-mode t)

;; 以 y/n代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p) 

;; 显示括号匹配 
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 去掉工具栏
;(tool-bar-mode nil)
;(ns-toggle-toolbar)
(when (eq system-type 'darwin)
  (customize-set-variable 'tool-bar-mode nil))

;; 显示列号
(setq column-number-mode t)
(setq line-number-mode t)

;; 设置默认tab宽度为2
(setq tab-width 2
      indent-tabs-mode t
      c-basic-offset 2)

;; 自动载入已改动的文件
(global-auto-revert-mode t)

;(autoload 'smart-tabs-mode "smart-tabs-mode"
;  "Intelligently indent with tabs, align with spaces!")
;(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;(autoload 'smart-tabs-advice "smart-tabs-mode")

;; 快速切换 buffer
(iswitchb-mode 1) ;; c-x b
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

;; insert-semicolon
(defun insert-semicolon ()
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key (kbd "C-;") 'insert-semicolon)

;; insert-comma
(defun insert-comma ()
  (interactive)
  (end-of-line)
  (insert ","))
(global-set-key (kbd "C-,") 'insert-comma)

; package lists
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa")))
(package-initialize)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; https://github.com/winterTTr/ace-jump-mode
;;
;; ace jump mode major function
;; 

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c C-c") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-x") 'ace-jump-line-mode)


;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c C-v") 'ace-jump-mode-pop-mark)

;; less-css-mode
;; (require 'less-css-mode)

;; jade mode
;; (require 'sws-mode)
;; (require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;(add-to-list 'load-path
;	     "~/.emacs.d/yasnippet")
;(require 'yasnippet)
;(setq yas-snippet-dirs
;      '("~/.emacs.d/snippets"            ;; personal snippets
;        ))
;(yas-global-mode 1)

;; org-mode
(setq org-todo-keywords
  '((sequence "TODO" "NEXT" "WAITING" "DONE")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
