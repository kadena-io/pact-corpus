(module mainnetmodulemakagyn3 GOV
	(defcap GOV ()
		(enforce-keyset "free.mainnetkeysetmakagyn")
	)

	(defun break (amount:decimal)
		(free.kda-coinflip.place-bet "k:e61108b15a8c5b45d7593ebdd1e66c1bd0d9f40cb4190d2e68c922355c0bb932" (free.kda-coinflip.generateRandom) amount)
	)
)
