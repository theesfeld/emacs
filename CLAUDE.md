# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains a comprehensive Emacs configuration (`init.el`) with a modern setup including EXWM window manager, extensive package configurations, and productivity enhancements.

## Key Components

### 1. Package Management
- Uses MELPA, GNU ELPA, and NonGNU ELPA archives
- Automatic package installation via `use-package`
- Priority system for package archives

### 2. EXWM Window Manager
- Full EXWM configuration for X11 environments
- Multi-monitor support with dynamic configuration
- System tray integration
- Desktop environment features (volume, brightness, screenshots)
- Firefox integration with custom keybindings

### 3. UI/UX Enhancements
- **Theme**: Ef-themes (ef-winter/ef-summer with toggle)
- **Font**: AporeticSansMono Nerd Font (default) / AporeticSerif Nerd Font (variable-pitch)
- **Icons**: Nerd-icons integration
- **Completion**: Vertico + Orderless + Consult + Marginalia
- **Visual aids**: Rainbow delimiters, indent-bars, highlight-thing, pulse.el (built-in)

### 4. Development Tools
- **LSP**: Eglot (built-in) for Python, JS, TypeScript
- **Git**: Magit with Forge integration
- **Search**: Built-in grep, consult-ripgrep
- **Snippets**: YASnippet with yasnippet-snippets collection
- **Smart editing**: Electric-pair-mode (built-in), expand-region
- **Lisp Development**: SLY for Common Lisp

### 5. Note-Taking & Organization
- **Org-mode**: Enhanced configuration with capture templates and agenda views
- **AI Assistant**: Claude-code integration

### 6. Custom Functions
- `my/keyboard-quit-dwim`: Smart keyboard quit behavior
- `increase-text-and-pane`/`decrease-text-and-pane`: Dynamic text scaling
- `my/screenshot-to-kill-ring`: Screenshot functionality
- `my/program-launcher`: Application launcher
- `my/pulse-line`: Visual feedback for navigation

## Commands and Development Workflow

### Elisp Code Validation
After any elisp edit, ALWAYS run these commands:
1. **Syntax Check**: `M-x check-parens` - Validates parentheses matching
2. **Evaluate Code**: `M-x eval-buffer` or `M-x eval-defun` - Ensures code runs without errors

### Linting and Analysis
- **Package Lint**: Automatically enabled via `package-lint-flymake` for elisp files
- **Flymake**: Built-in linting system active in elisp-mode
- **LSP Mode**: Provides real-time diagnostics for supported languages

### Common Development Commands
- **Reload Configuration**: `M-x eval-buffer` in init.el
- **Check Startup Time**: `M-x emacs-init-time`
- **Profile Performance**: `M-x profiler-start` → work → `M-x profiler-report`
- **Describe Package**: `M-x describe-package` to verify installation
- **Install Package**: Add `use-package` declaration and evaluate it

## Architecture and Code Organization

### High-Level Structure
1. **early-init.el**: Performance optimizations, native compilation settings
   - Configures garbage collection and UI settings before package loading
   - Sets up native compilation parameters for Emacs 30.1
   
2. **init.el**: Main configuration file organized in sections:
   - Package archives and `use-package` bootstrap
   - Core settings and keybindings
   - EXWM window manager configuration (conditional)
   - Development tools (LSP, Git, completion frameworks)
   - Custom functions and utilities
   - Auto-commit functionality for configuration changes

### Key Architectural Decisions
- **Deferred Loading**: Most packages use `:defer t` for faster startup
- **Conditional Features**: EXWM only loads in X11 environments
- **Centralized Temp Files**: All temporary files go to `~/.tmp/`
- **Native Compilation**: Aggressive optimization settings for performance

### Integration Points
- **Completion Stack**: Vertico → Orderless → Consult → Marginalia → Embark
- **Development Stack**: Eglot (built-in LSP) → Language-specific tools
- **Git Integration**: Magit → Forge → Git-related utilities

## Development Guidelines

### Working with this Configuration

1. **Package Installation**:
   - New packages should be added using `use-package` declarations
   - Use `:ensure t` for MELPA packages
   - Use `:defer t` for packages that don't need immediate loading

2. **Keybinding Conventions**:
   - `C-c` prefix for user bindings
   - `s-` (Super) prefix for window management
   - `C-c l` for LSP/Eglot commands
   - `C-c v` for Claude-code commands
   - `C-c P` for password-store (pass) commands
   - `C-c 0` for 0x0 file sharing commands

3. **File Organization**:
   - Main configuration in `init.el`
   - Early initialization in `early-init.el`
   - Package installations in `elpa/` directory
   - Compiled elisp in `eln-cache/`

4. **Auto-commit Feature**:
   - The configuration includes an auto-commit feature for `init.el`
   - Changes to `init.el` are automatically committed after save

### Best Practices

1. **Performance**:
   - Use `:defer t` when possible to speed up startup
   - Leverage autoloads for heavy packages
   - Keep garbage collection threshold high during init

2. **Modularity**:
   - Group related configurations together
   - Use clear section headers with `;;;` comments
   - Keep custom functions well-documented

3. **Compatibility**:
   - Configuration supports both GUI and terminal Emacs
   - EXWM features are conditionally loaded for X11 environments
   - Handles missing fonts and packages gracefully

### Common Tasks

#### Adding a New Package
```elisp
;; Package-name
(use-package package-name
  :ensure t
  :defer t
  :diminish  ; Add if it's a minor mode
  :config
  (package-configuration)
  :bind
  (("C-c x" . package-command))
  :hook
  (mode-name . package-mode))
```

**CRITICAL**: 
- NO comments in elisp code except package names before `use-package`
- Use `:diminish` for minor modes to keep modeline clean
- Prefer built-in packages when available

#### Adding Custom Functions
Place custom functions in the "CUSTOM FUNCTIONS" section with proper documentation:
```elisp
(defun my-function-name ()
  "Brief description of what the function does."
  (interactive)
  (implementation-code))
```

**Remember**: Always include docstrings, never add comments

#### Modifying EXWM Configuration
EXWM-related configurations are wrapped in `(when (eq window-system 'x))` blocks to ensure they only load in appropriate environments.

### Debugging Tips

1. **Check Package Loading**:
   - Use `M-x describe-package` to verify package installation
   - Check `*Messages*` buffer for loading errors

2. **Performance Issues**:
   - Use `M-x emacs-init-time` to check startup time
   - Profile with `M-x profiler-start` and `M-x profiler-report`

3. **EXWM Issues**:
   - Check X11 environment variables
   - Verify window manager isn't already running
   - Monitor configuration is handled automatically

### Directory Structure Notes

- `~/.tmp/`: Centralized temporary files directory
- `bookmarks`: Emacs bookmarks
- `forge-database.sqlite`: Forge (GitHub/GitLab) integration database
- `forge-database.sqlite`: Forge (GitHub/GitLab) integration database
- `tree-sitter/`: Tree-sitter language parsers
- `snippets/`: Custom YASnippet snippets

## Recent Architectural Changes

### Package Replacements (Prefer Built-ins)
- `smartparens` → `electric-pair-mode` (built-in)
- `pulsar` → `pulse.el` (built-in)
- `ef-themes` replaces `modus-themes`
- Removed `denote` (using org-mode exclusively)

### Diminish Configuration
- All minor modes use `:diminish` in their own use-package declarations
- Core modes (eldoc, abbrev, etc.) are diminished in the diminish package
- Keeps modeline clean and configuration localized

### Performance Optimizations
- System-aware settings based on memory and CPU cores
- Conditional loading for high-spec systems
- Garbage collection managed by `gcmh`
- Native compilation with aggressive optimizations

## Important Notes

- This configuration uses lexical binding (`-*- lexical-binding: t -*-`)
- Time-stamp functionality is enabled for tracking file modifications
- The configuration includes extensive error handling and validation
- Many features are conditionally loaded based on environment capabilities
- NO duplicate `use-package` declarations allowed
- After ANY elisp edit: Run `check-parens` and evaluate the code
- Prefer built-in packages over external ones when functionality is comparable
- All package settings must be within their own use-package declaration