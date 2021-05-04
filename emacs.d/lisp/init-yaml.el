;;; init-yaml.el --- YAML editing configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for YAML files edition.

;;; Code:

;; YAML files
(use-package yaml-mode
  :preface (defun ngq/yaml-mode-hook () (auto-fill-mode -1))
  :config
  (add-hook 'yaml-mode-hook #'ngq/yaml-mode-hook)

  ;; LSP integration w/ yaml-language-server
  (when (executable-find "yaml-language-server")
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
    (add-hook 'yaml-mode-hook #'lsp-deferred)
    (add-hook 'yaml-mode-hook #'company-mode)))

(provide 'init-yaml)

;;; init-yaml.el ends here
