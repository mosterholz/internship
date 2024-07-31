
library(locuszoomr)



Regina_12 <- Regina %>%
  filter(CHROM == 12)%>%
  rename(P=p.value)
Regina_12slim <- slimmer(Regina_12, threshold = 0.001, fraction = 0.1)



if(require(EnsDb.Hsapiens.v75)) {
  loc <- locus(data = Regina_12slim, seqname = 12, xrange=c(69412565,71412565), flank = 1e9, chrom = "CHROM", pos = "GENPOS", p="P", labs="ID", 
               ens_db = "EnsDb.Hsapiens.v75")
  summary(loc)
  locus_plot(loc)
}
loc <- link_LD(loc, token = "e0c6a1199e98")
