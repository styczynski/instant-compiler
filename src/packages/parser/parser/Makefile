all:
	happy -gca ParSyntax.y
	alex -g LexSyntax.x
	ghc --make TestSyntax.hs -o TestSyntax

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocSyntax.* LexSyntax.* ParSyntax.* LayoutSyntax.* SkelSyntax.* PrintSyntax.* TestSyntax.* AbsSyntax.* TestSyntax ErrM.* SharedString.* ComposOp.* syntax.dtd XMLSyntax.* Makefile*
	
