numRec w h iw ih
	| w == iw && h == ih = 1
	| w == iw = (h - ih + 1) + numRec w h 1 (ih + 1)
	| otherwise = ((h - ih + 1) * (w - iw + 1)) + numRec w h (iw + 1) ih

pe85 w h margin num
	| amt < margin = pe85 (w+1) (h-1) amt (w*h)
	| rect > 2000000 = pe85 (w+1) (h-1) margin num
	| rect < 2000000 = pe85 (w+1) (h+1) margin num
	| h == 0 = num
	where 
	rect = numRec w h 1 1
	amt = abs (2000000 - rect)
