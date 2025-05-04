# Dependencies and requirements

- nvim v0.10+
- kitty (i've installed v0.41.1)
- herbstluftwm (i've installed v0.9.5)
- [this](https://github.com/drscream/lemonbar-xft) lemonbar fork
- conky (i've installed v1.22.1)
- stalonetray (i've installed v0.8.3)
- dmenu
- flameshot
- copyq
- cava
- neowofetch

# Deployment

```bash
git clone https://github.com/iddixx/dotfiles.git
cd dotfiles
./deploy.d
```

> [!WARNING]
> It will `rm -rf` all your configs, that conflict with mine.
> If you value your configurations, don't forget to backup them.

> [!NOTE]
> If you don't want to symlink some of the directories, just remove them.
> (it won't work with anything specified in `specific_links` in main function in deploy.d)


