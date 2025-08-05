# Emacs Configuration

[![Emacs](https://img.shields.io/badge/Emacs-30.1%2B-blueviolet.svg)](https://www.gnu.org/software/emacs/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Language: Elisp](https://img.shields.io/badge/Language-Emacs%20Lisp-green.svg)](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
[![EXWM](https://img.shields.io/badge/WM-EXWM-orange.svg)](https://github.com/ch11ng/exwm)
[![Use Package](https://img.shields.io/badge/Package%20Manager-use--package-lightgrey.svg)](https://github.com/jwiegley/use-package)

A comprehensive Emacs configuration with EXWM window manager, modern completion frameworks, and extensive development tools.

## Features

### 🪟 Window Management
- **EXWM**: Complete X11 window manager integration
- Multi-monitor support with dynamic configuration
- System tray integration
- Desktop environment features (volume, brightness, screenshots)
- Firefox integration with custom keybindings

### 🎨 UI/UX
- **Theme**: Ef-themes (Winter/Summer toggle)
- **Font**: AporeticSansMono Nerd Font
- **Completion**: Vertico + Orderless + Consult + Marginalia + Corfu
- **Auto-completion**: Corfu with global auto-completion
- **Visual aids**: Rainbow delimiters, indent-bars, highlight-thing
- Variable-pitch fonts for comments

### 🛠️ Development Tools
- **LSP**: Eglot (built-in) for language server protocol
- **Git**: Magit with Forge integration
- **Search**: Consult-ripgrep for code search
- **Snippets**: YASnippet with yasnippet-snippets collection
- **Smart editing**: Electric-pair-mode (built-in), expand-region
- **Lisp Development**: SLY for Common Lisp

### 📝 Note-Taking & Organization
- **Org-mode**: Enhanced configuration with capture templates and agenda views
- **AI Assistant**: Claude-code integration for programming assistance

## Requirements

- Emacs 30.1 or later
- Native compilation support recommended
- Nerd Fonts (AporeticSansMono recommended)
- For EXWM: X11 environment

## Installation

1. Clone this repository:
```bash
git clone https://github.com/theesfeld/emacs.git ~/.config/emacs
```

2. Start Emacs - packages will be automatically installed on first run

## Key Bindings

### General
- `C-g` / `ESC`: Smart keyboard quit
- `C-=` / `C--`: Increase/decrease text size
- `M-s-<backspace>`: Toggle theme (Winter/Summer)

### EXWM (Window Manager)
- `s-l`: Lock screen
- `s-SPC`: Application launcher
- `s-r`: Reset EXWM
- `s-&`: Run shell command
- `s-[0-9]`: Switch to workspace
- `s-f`: Toggle floating window
- `s-F`: Toggle fullscreen
- `s-[arrow]`: Navigate windows

### Development
- `C-c l`: LSP/Eglot commands prefix
- `C-c g`: Magit status
- `C-c p`: Project commands
- `C-c s`: Search commands (consult-ripgrep)
- `C-n` / `C-p`: Navigate completion candidates (Corfu)
- `TAB`: Complete selection (Corfu)

### Notes & Organization
- `C-c o`: Org-mode commands
- `C-c v`: Claude-code (AI) commands
- `C-c P`: Password-store (pass) commands

## Configuration Structure

- `init.el`: Main configuration file
- `early-init.el`: Performance optimizations and early settings
- `CLAUDE.md`: Project-specific AI assistant instructions
- `~/.tmp/`: Centralized temporary files directory

## Performance Features

- Garbage collection optimization via `gcmh`
- Deferred package loading
- Native compilation with aggressive optimization
- Smart file handler management

## Customization

The configuration follows functional programming principles and GNU coding standards. Key customization points:

1. **Theme**: Modify the `ef-themes-to-toggle` list in init.el
2. **Fonts**: Adjust font settings in the font configuration section
3. **Temporary files**: All temp files use the `my/tmp-dir` constant

## License

Copyright (C) 2024 William Theesfeld <william@theesfeld.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.

---

<div align="center">
  <a href="https://www.gnu.org/">
    <img src="https://www.gnu.org/graphics/gnu-head-sm.jpg" alt="GNU" height="80">
  </a>
  &nbsp;&nbsp;&nbsp;&nbsp;
  <a href="https://www.fsf.org/">
    <img src="https://static.fsf.org/nosvn/associate/crm/5002438.png" alt="FSF Member" height="80">
  </a>
</div>
