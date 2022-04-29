# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# For flatpak
export XDG_DATA_DIRS=$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share
export PATH=$PATH:$HOME/.local/share/flatpak/exports/bin

# For ibus
# export GUIX_GTK2_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"
# export GUIX_GTK3_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"
# export IBUS_COMPONENT_PATH="$HOME/.guix-profile/share/ibus/component"
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus
export GUIX_GTK2_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"
export GUIX_GTK3_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

# For keyboard
setxkbmap -option ctrl:nocaps
xmodmap -e 'keycode 102=Alt_L'  # 102 is Muhenkan
xmodmap -e 'keycode 100=Muhenkan' # 100 is Henkan
xmodmap -e 'keycode 101=Muhenkan' # 101 is Hiragana_Katakana
