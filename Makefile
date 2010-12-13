all:
	cabal configure
	cabal build
	cabal install
	@echo "\nplease type make install\n"
install:
	@echo "\nInstalling Spirit-Core\n"
	mv $(HOME)/.cabal/bin/spiritcore .
	cabal clean
	@echo "\nsuccessful have fun :-)\n"
uninstall:
	@echo "Uninstalling spiritcore\n"
	rm spiritcore
	@echo "\nUninstall successful\n"

