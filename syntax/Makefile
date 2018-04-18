all:
	happy -gca ParStretch.y
	alex -g LexStretch.x
	ghc --make TestStretch.hs -o TestStretch

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocStretch.* LexStretch.* ParStretch.* LayoutStretch.* SkelStretch.* PrintStretch.* TestStretch.* AbsStretch.* TestStretch ErrM.* SharedString.* ComposOp.* stretch.dtd XMLStretch.* Makefile*
	
