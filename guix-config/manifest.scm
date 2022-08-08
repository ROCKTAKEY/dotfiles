(use-modules (guix packages))

(packages->manifest
 (list
  ;;; Essensial tools
  (specification->package "clang")
  (specification->package "cmake")
  (specification->package "gcc-toolchain@10.3.0")
  (specification->package "git")
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

  ;; Tools needed by Emacs
  (specification->package "ccls")
  (specification->package "cmigemo")
  (specification->package "migemo-dict")
  (specification->package "miscfiles")
  (specification->package "the-silver-searcher")

  ;;; Other tools
  (specification->package "f3")
  (specification->package "gitlab-runner")
  (specification->package "go-github-com-cli-cli-v2")
  (specification->package "go-github-com-gohugoio-hugo")
  (specification->package "go-github-com-gohugoio-hugo-extended")
  (specification->package "go-github-com-profclems-glab")
  (specification->package "node-latest")
  (specification->package "online-judge-template-generator")
  (specification->package "online-judge-tools")
  (specification->package "python-cookiecutter")
  (specification->package "rust-rhq")

  ;;; Libraries
  (specification->package "eigen")

  ;;; Desktop and interface
  (specification->package "gconf")
  (specification->package "gnome-shell")
  (specification->package "gnome-shell-extension-dash-to-dock")
  (specification->package "gnome-shell-extensions")
  (specification->package "gnome-tweaks")
  (specification->package "setxkbmap")
  (specification->package "xdg-desktop-portal")
  (specification->package "xdg-desktop-portal-gtk")
  (specification->package "xdg-utils")
  (specification->package "xmodmap")

  ;;; GUI application
  (specification->package "firefox")
  (specification->package "freerdp")
  (specification->package "inkscape")
  (specification->package "libreoffice")
  (specification->package "obs")
  (specification->package "steam")

  ;;; Fonts
  (specification->package "font-cica")

  ;;; IME
  (specification->package "skk-jisyo")
  (specification->package "uim")
  (specification->package "uim-gtk")

  ;;; For use of installing other packages
  (specification->package "flatpak")))
