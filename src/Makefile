.PHONY: KLambda/Lexer.hs

run: KLambda/Lexer.hs
	ghci KLambda/Parser.hs

KLambda/Lexer.hs:
	alex KLambda/Lexer.x

clean:
	rm KLambda/Lexer.hs
