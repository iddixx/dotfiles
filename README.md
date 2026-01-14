> [!WARNING]
> Do not send any pull requests or/and issues.
> My dotfiles supposed to work **ONLY** on my machine.
> If it doesn't work on yours, clone/fork it, and fix it yourself.

# Dependencies and requirements
- Iosevka Fixed SS14 Font(used almost everywhere) and Iosevka Fixed SS16 Font(used in kitty)
- rdmd(needed to execute deploy.d, usually comes with dmd compiler)
- nvim v0.11+(also works on v0.10+)
- kitty (i've installed v0.41.1)
- [this](https://github.com/drscream/lemonbar-xft) lemonbar fork
- dmenu
- copyq
- cava(optional)
- neowofetch(optional)
- nitrogen
- redshift
## Herbstluftwm-specific configuration dependencies
- herbstluftwm (i've installed v0.9.5)
- [this](https://github.com/drscream/lemonbar-xft) lemonbar fork
- stalonetray (i've installed v0.8.3)
- conky (i've installed v1.22.1)
- flameshot
## Ratpoison-specific configuration dependencies
- ratpoison v1.4.10-beta+
- rpws
- Iosevka Nerd Font(used in polybar)
- polybar v3.7.0+
- brightnessctl
- unclutter-xfixes
- picom v1.12.0+
- maim
- boomer(zoomer application)
- xcolor
## XMonad-specific configuration dependencies
- XMonad 0.17.0+
- xmobar 0.48.0+
- stalonetray
- unclutter-xfixes
- copyq
- picom v1.12.0+
- pactl
- awk
- brightnessctl
- xkb-switch
- NetworkManager
- maim
- boomer(zoomer application)
- xcolor

# Deployment

> [!CAUTION]
> It will `rm -rf` all your configs, that conflict with mine.
> If you don't want this to happen, run `deploy.d` with `--skip-conflicting` flag.

```bash
git clone https://github.com/iddixx/dotfiles.git
cd dotfiles
./deploy.d
```

> [!NOTE]
> If you also want to link your flatpaks to `$HOME/.local/bin`, use `--link-flatpaks` flag. 
> If you want **only** link your flatpaks, use `--only-link-flatpaks` flag.

> [!NOTE]
> If you don't want to symlink some of the directories, just remove them.
> (it won't work with anything specified in `specific_links` in `get_config()` in deploy.d)
