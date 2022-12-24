#!/bin/zsh

# Install Brew
echo "Installing Brew..."
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

# Brew Taps
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font

# Brew Formulae
brew install stow
brew install jq
brew install mas
brew install koekeishiya/formulae/yabai
brew install koekeishiya/formulae/skhd
brew install cmake
brew install sf-symbols
# org roam dependency
brew install git ripgrep

# Brew Casks
echo "Installing Brew Casks..."
# brew install --cask mactex-no-gui
brew install --cask basictex
brew install --cask miniconda
brew install --cask google-chrome
brew install --cask alfred
brew install --cask karabiner-elements
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
ln -s /opt/homebrew/Cellar/emacs-mac/emacs-28.2-mac-9.1/Emacs.app /Applications/Emacs.app

# Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
doom sync

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
# defaults delete com.apple.dock persistent-apps
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

# zsh plugins
# oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# powerlevel10k
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
# autosuggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# stow configuration files
echo "Stowing Configuration Files..."
git clone https://github.com/hhkgg/.files.git
cd .files
make all

# Start Services
echo "Starting Services (grant permissions)..."
brew services start skhd
brew services start yabai

softwareupdate --install-rosetta

csrutil status
echo "Do not for var in stuff; do

doneget to disable SIP."
echo "Add sudoer manually:\n '$(whoami) ALL = (root) NOPASSWD: sha256:$(shasum -a 256 $(which yabai) | awk "{print \$1;}") $(which yabai) --load-sa' to '/private/etc/sudoers.d/yabai'"
echo "Installation complete..."
