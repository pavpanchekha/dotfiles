
;; PyFlakes
(when (load "flymake" t) 
      (defun flymake-pyflakes-init () 
        (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                           'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
          (list "pyflakes" (list local-file)))) 

      (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))

      (setq flymake-allowed-file-name-masks (remove-if (lambda (x) (eq (elt x 1) 'flymake-master-tex-init)) flymake-allowed-file-name-masks)))

(add-hook 'find-fileile-hook 'flymake-find-file-hook)
