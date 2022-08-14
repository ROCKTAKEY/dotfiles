(use-modules (guix packages))

(packages->manifest
 (list
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

  ;;; IME
  (specification->package "skk-jisyo")
  (specification->package "uim")
  (specification->package "uim-gtk")))
