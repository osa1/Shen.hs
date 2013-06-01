run:
	cabal build && cd K\ Lambda && rlwrap ./../dist/build/Shen.hs/Shen.hs toplevel.kl core.kl sys.kl sequent.kl yacc.kl reader.kl prolog.kl track.kl load.kl writer.kl macros.kl declarations.kl t-star.kl types.kl
