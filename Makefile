haddock:
	cabal haddock
	weasyprint ./dist/doc/html/haskzure/Cloud-Haskzure.html ./docs/haddock/Haskzure.pdf
	weasyprint ./dist/doc/html/haskzure/Cloud-Haskzure-Core.html ./docs/haddock/Core.pdf
	weasyprint ./dist/doc/html/haskzure/Cloud-Haskzure-Gen.html ./docs/haddock/Gen.pdf
