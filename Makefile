all:
	stack install --only-dependencies
	stack install shake
	stack exec -- shake