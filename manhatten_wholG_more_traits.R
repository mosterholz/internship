
color_by_threshold <- function(value, color_above, color_below) {
  ifelse(value > threshold, color_above, color_below)
}

threshold <- 5*10^(-8)

datatoplot_trait_2001_slim <- slimmer(datatoplot_trait_2001, threshold = 0.00001, fraction = 0.1)
datatoplot_trait_2002_slim <- slimmer(datatoplot_trait_2002, threshold = 0.00001, fraction = 0.1)
datatoplot_trait_2003_slim <- slimmer(datatoplot_trait_2003, threshold = 0.00001, fraction = 0.1)
datatoplot_trait_2004_slim <- slimmer(datatoplot_trait_2004, threshold = 0.00001, fraction = 0.1)


all_traits <- rbind(datatoplot_trait_2001_slim,datatoplot_trait_2002_slim,datatoplot_trait_2003_slim,datatoplot_trait_2004_slim)

colorpattener <- function(datatoplot_trait_2001_slim, name="sig_trait_01"){
datatoplot_trait_2001_slim <- datatoplot_trait_2001_slim%>%
  mutate(colorpattern=case_when(
    P < threshold ~ name,
    P >= threshold & CHROM %% 2 == 0 ~ "chrom_even",
    P >= threshold & CHROM %% 2 == 1 ~ "chrom_odd"
  ))}
datatoplot_trait_2002_slim <- colorpattener(datatoplot_trait_2002_slim, name="sig_trait_02")
datatoplot_trait_2003_slim <- colorpattener(datatoplot_trait_2003_slim, name="sig_trait_03")
datatoplot_trait_2004_slim <- colorpattener(datatoplot_trait_2004_slim, name="sig_trait_04")

sig_all <- all_traits%>%
  filter(P<threshold)
    


ggplot()+
  geom_hline(
    yintercept = -log10(5*10^(-8)),
    linetype="dashed")+
  geom_point(
    data = datatoplot_trait_2001_slim, 
    mapping = aes( x=cumGENPOS, y=-log10(P), color =colorpattern))+
  geom_point(
    data = datatoplot_trait_2002_slim, 
    mapping = aes( x=cumGENPOS, y=-log10(P), color =colorpattern))+
  geom_point(
    data = datatoplot_trait_2003_slim, 
    mapping = aes( x=cumGENPOS, y=-log10(P), color = colorpattern))+
  geom_point(
    data = datatoplot_trait_2004_slim, 
    mapping = aes( x=cumGENPOS, y=-log10(P), color = colorpattern))+
  scale_colour_manual(
    values = c(sig_trait_01 = "blue", sig_trait_02= "red", sig_trait_03="green4", sig_trait_04="orange", chrom_odd="grey60",chrom_even="grey70"),
    breaks = c("sig_trait_01", "sig_trait_02", "sig_trait_03", "sig_trait_04"))+
  geom_text_repel(data = sig_all, aes(x=cumGENPOS, y=-log10(P),label = paste(ID)))+
  scale_x_continuous(
    label = as.factor(lables_axisX$CHROM),
    breaks = axisX$medGENPOS)+
  theme_classic()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 8, vjust = 0.5))+
  labs(
    x = "Chromosome",
    y = expression("-log"[10]*"(p)"))
