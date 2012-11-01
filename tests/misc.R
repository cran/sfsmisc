require("sfsmisc")

options(warn=2)
AsciiToInt(LETTERS) # gave '.. embedded nul ..' warning

## just for fun -- typically shows "iso-latin1 charset
cat(chars8bit(1:255),"\"\n")

## Checking the new 'ndigits' default argument for digitsBase():
ee <- 0:30
for(base in 2:64)
    stopifnot((be <- base^ee) > 0, any(ok <- be < 2^52),
	      ee == floor(1e-9+ log(be, base)),
	      be[ok] == as.integer(digitsBase(be[ok], base=base)))
## failed, e.g. for 3^5, in sfsmisc <= 1.0-22
