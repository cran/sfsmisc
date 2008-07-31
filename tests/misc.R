require("sfsmisc")

options(warn=2)
AsciiToInt(LETTERS) # gave '.. embedded nul ..' warning

## just for fun -- typically shows "iso-latin1 charset
cat(chars8bit(1:255),"\"\n")
