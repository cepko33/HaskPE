amts = [1,2,5,10,20,50,100,200]

coinCombo :: Int -> Int
coinCombo tot
	| dif == 0 = 1
	| dif < 0 = 0
	| dif <= 1 = coinCombo (tot + 1)
	| dif <= 2 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..1]]
	| dif <= 5 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..2]]
	| dif <= 10 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..3]]
	| dif <= 20 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..4]]
	| dif <= 50 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..5]]
	| dif <= 100 = sum [coinCombo (tot  + (amts !! n)) | n <- [0..6]]
	where dif = 200 - tot

