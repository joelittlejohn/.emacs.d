(define-key global-map (kbd "M-[") 'paredit-wrap-square)
(define-key global-map (kbd "M-{") 'paredit-wrap-curly)

(global-set-key (kbd "C-x C-<left>")  'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<up>")    'windmove-up)
(global-set-key (kbd "C-x C-<down>")  'windmove-down)

(global-set-key (kbd "M-\\") 'delete-whitespace-except-one)
