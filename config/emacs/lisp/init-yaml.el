;;; init-yaml.el --- YAML editing configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for YAML files edition.

;;; Code:

;; YAML files
(use-package yaml-mode
  :preface (defun ngq/setup-yaml-mode () (auto-fill-mode -1))
  :hook (yaml-mode . ngq/setup-yaml-mode)
  :bind (:map yaml-mode-map
              ([remap reindent-then-newline-and-indent] . #'newline-and-indent))
  :init
  (when (and (fboundp #'eglot-ensure) (executable-find "yaml-language-server"))
    (setq lsp-yaml-schema-store-local-db "~/.emacs.d/var/lsp/lsp-yaml-schemas.json"
          lsp-yaml-schemas '((kubernetes . ["kube.yaml"
                                            "resources.yaml"
                                            "resources/*"
                                            "pod.yaml"
                                            "deployment.yaml"
                                            "serviceaccount.yaml"
                                            "clusterrole.yaml"
                                            "role.yaml"
                                            "clusterrolebinding.yaml"
                                            "rolebinding.yaml"
                                            "configmap.yaml"
                                            "service.yaml"])
                             (http://json\.schemastore\.org/kustomization . ["kustomization.yaml"])))
    (add-hook 'yaml-mode-hook #'eglot-ensure)))

(provide 'init-yaml)

;;; init-yaml.el ends here
