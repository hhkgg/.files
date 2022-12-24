all:
	stow --verbose --target=$$HOME --adopt */

delete:
	stow --verbose --target=$$HOME --delete */
