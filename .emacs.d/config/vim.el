(setq viper-inhibit-startup-message t
      viper-expert-level '3
      viper-mode t)

(require 'viper)

(add-to-list 'load-path "~/.emacs.d/load-path")
(require 'vimpulse)

(windmove-default-keybindings 'hyper)
