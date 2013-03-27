run:
	cabal build && cd K\ Lambda && rlwrap ./../dist/build/KLambda.hs/KLambda.hs toplevel.kl core.kl sys.kl sequent.kl yacc.kl reader.kl prolog.kl track.kl load.kl writer.kl macros.kl declarations.kl
