source ~/.profile

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# For flatpak
export XDG_DATA_DIRS=$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share
export PATH=$PATH:$HOME/.local/share/flatpak/exports/bin

# For IME
export GUIX_GTK2_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"
export GUIX_GTK3_IM_MODULE_FILE="$HOME/.guix-profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"

# ibus
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus

# uim
# export GTK_IM_MODULE='uim'
# export QT_IM_MODULE='uim'
# uim-xim &
# export XMODIFIERS='@im=uim'
# uim-toolbar-gtk3 &
# uim-toolbar-gtk3-systray &

# fcitx
export GTK_IM_MODULE='fcitx'
export QT_IM_MODULE='fcitx'
export XMODIFIERS='@im=fcitx'
fcitx5 &

if [ -x "$(command -v rclone)" ]; then
	for remote in $(rclone listremotes); do
		remoteDir=~/${remote:0:(-1)}
		mkdir -p "${remoteDir}"
		rclone mount "${remote}/" "${remoteDir}" --vfs-cache-mode full &
	done
fi

PATH="$HOME/.local/bin:$PATH"
export PATH=~/.npm-global/bin:$PATH

export XDG_DATA_DIRS=~/.nix-profile/share:$XDG_DATA_DIRS
export PATH=~/.nix-profile/bin:$PATH

# Prefer dark mode
export GTK_THEME=Adwaita:dark

mkdir -p "$HOME/.local/state/xremap"
xremap --watch=device,config ~/.config/xremap/config.yml >>"$HOME/.local/state/xremap/xremap.log" &
