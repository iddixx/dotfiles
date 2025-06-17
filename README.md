# Dependencies and requirements

- rdmd(needed to execute deploy.d, usually comes with dmd compiler)
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
- nitrogen
- redshift

# Deployment

> [!WARNING]
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



