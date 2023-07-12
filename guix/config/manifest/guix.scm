(use-modules (guix packages))

(packages->manifest
 (list
  ;;; Essensial tools
  (specification->package "clang")
  (specification->package "cmake")
  (specification->package "fontconfig")
  (specification->package "gcc-toolchain")
  (specification->package "git")
  (list (specification->package "git") "send-email")
  (specification->package "glibc")
  (specification->package "gnupg")
  (specification->package "make")
  (specification->package "stow")
  (specification->package "unzip")

  ;;; File syncing
  (specification->package "rclone")
  (specification->package "syncthing")
  (specification->package "syncthing-gtk")

  ;;; Emacs
  (specification->package "emacs")
  (specification->package "emacs-keg")
  (specification->package "emacs-cask")

  ;; Tools needed by Emacs
  (specification->package "ccls")
  (specification->package "cmigemo")
  (specification->package "migemo-dict")
  (specification->package "miscfiles")
  (specification->package "ripgrep")

  ;;; Other tools
  (specification->package "f3")
  (specification->package "go-github-com-cli-cli-v2")
  (specification->package "go-github-com-gohugoio-hugo")
  (specification->package "go-github-com-gohugoio-hugo-extended")
  (specification->package "go-github-com-profclems-glab")
  (specification->package "node")
  (specification->package "online-judge-template-generator")
  (specification->package "online-judge-tools")
  (specification->package "python-cookiecutter")
  (specification->package "rust-rhq")

  ;;; Fonts
  (specification->package "font-cica")

  ;;; For use of installing other packages
  (specification->package "flatpak")))
