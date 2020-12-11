# Emacs config for back- and frontend-development

This configuration created for working with Python 2/3, JavaScript and HTML files.

## Icon fonts

You must install required fonts with your package manager.
Debian-based distros:

```bash
apt-get install fonts-font-awesome fonts-materialdesignicons-webfont fonts-octicons
```

Also every start `init.el` checks existing firectory `~/.local/share/fonts`. If this directory does not exists, called command `all-the-icons-install-fonts`.
