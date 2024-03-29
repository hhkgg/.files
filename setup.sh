#!/bin/zsh

# Install Brew
echo "Installing Brew..."
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

# required dependencies (Doom Emacs)
brew install git ripgrep
# optional dependencies
brew install coreutils fd
# Installs clang
xcode-select --install

# Brew Taps
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app

# Brew Formulae
brew install stow
brew install jq
brew install mas
brew install koekeishiya/formulae/yabai
brew install koekeishiya/formulae/skhd
brew install cmake
brew install aspell

# for prewview-latex
brew install ghostscript
# org roam dependency
brew install git ripgrep

# Brew Casks
echo "Installing Brew Casks..."
brew install --cask mactex-no-gui
# dvipng for org-preview-latex
sudo tlmgr update --self && tlmgr install dvipng
# brew install --cask miniconda
brew install --cask google-chrome
brew install --cask alfred
brew install --cask karabiner-elements
brew install --cask skim

# Emacs MacOS compatibility
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
ln -s /opt/homebrew/Cellar/emacs-mac/emacs-28.2-mac-9.1/Emacs.app /Applications/Emacs.app

# Mac App Store Apps
echo "Installing Mac App Store Apps..."
mas install 823766827 #OneDrive
mas install 462058435 #microsoft-excel

echo "Changing macOS defaults..."
defaults write -g ApplePressAndHoldEnabled -bool false
defaults write NSGlobalDomain InitialKeyRepeat -int 10
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write com.apple.dock orientation left
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock "mru-spaces" -bool "false"
defaults write com.apple.dock "tilesize" -int "22"
defaults write com.apple.dock autohide-time-modifier -float 0.15
defaults write com.apple.dock "show-recents" -bool "false"
defaults write com.apple.dock "mineffect" -string "scale"
defaults write com.apple.dock "static-only" -bool "true"
defaults write com.apple.dock "autohide-delay" -float "0" && killall Dock
defaults write NSGlobalDomain "AppleShowAllExtensions" -bool "true"
defaults write com.apple.finder "AppleShowAllFiles" -bool "true"
defaults write com.apple.finder "FXPreferredViewStyle" -string "clmv"
defaults write com.apple.finder "FXEnableExtensionChangeWarning" -bool "false" && killall Finder
defaults write com.apple.universalaccess "showWindowTitlebarIcons" -bool "true"
defaults write com.apple.menuextra.clock "DateFormat" -string "\"EEE d MMM HH:mm:ss\""
defaults write NSGlobalDomain "ApplePressAndHoldEnabled" -bool "false"
defaults write com.apple.finder "ShowPathbar" -bool "true" && killall Finder

# stow configuration files
echo "Stowing Configuration Files..."
git clone https://github.com/hhkgg/.files.git
cd .files
make all

# start Services
echo "Starting Services (grant permissions)..."
# start yabai
yabai --start-service
skhd --start-service

csrutil status
echo "Don't forget to disable SIP."
echo "Add sudoer manually:\n '$(whoami) ALL = (root) NOPASSWD: sha256:$(shasum -a 256 $(which yabai) | awk "{print \$1;}") $(which yabai) --load-sa' to '/private/etc/sudoers.d/yabai'"
echo "Installation complete... Restart computer"
