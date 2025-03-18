library(bibtex)
library(rcrossref)

# To retrieve bibtex Library from DOI
cat(rcrossref::cr_cn("10.1007/s11357-022-00541-3", format = "bibtex"))
