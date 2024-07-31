#Manhattan Plot whole Genom second try
manhattanplot_wholegenom <- function(data, unifier=TRUE){
library(dplyr)
library(ggplot2)

  if(unifier){
    original_data <- plink_regina_unifier(data)
  }else{original_data <- data}
original_data_slim <- slimmer(original_data, threshold = 0.0001, fraction=0.3)

# nach Chromoson organisieren

cumCHROM <- original_data_slim%>%
  group_by(CHROM)%>%
  arrange(CHROM)%>%
  summarise(maxGENPOS = max(GENPOS))%>%
  mutate(addGENPOS = lag(cumsum(as.numeric(maxGENPOS)), default =0))%>%
  select(CHROM, addGENPOS)

datatoplot <- original_data_slim%>%
  inner_join(cumCHROM, by= "CHROM") %>%
  mutate(cumGENPOS= GENPOS+addGENPOS)
  

axisX <- datatoplot%>%
  group_by(CHROM)%>%
  arrange(CHROM)%>%
  summarise(maxGENPOS = max(cumGENPOS), minGENPOS= min(cumGENPOS))%>%
  rowwise()%>%
  mutate(medGENPOS= median(c(minGENPOS,maxGENPOS)))
  
lables_axisX <- cumCHROM%>%
  select(CHROM)%>%
  mutate(CHROM = ifelse(CHROM == "23", "X", CHROM))



ggplot(data = datatoplot, mapping = aes( x=cumGENPOS, y=logP, color = as.factor(CHROM)))+
  geom_hline(
    yintercept = -log10(5*10^(-8)),
    linetype="dashed"
  )+
  geom_point()+
    scale_x_continuous(
      label = as.factor(lables_axisX$CHROM),
      breaks = axisX$medGENPOS)+
    scale_color_manual(values = rep(
      c("black", "gray39"),
      unique(length(axisX$CHROM))
    ))+
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 8, vjust = 0.5)
  )+
  labs(
    x = "Chromosome",
    y = expression("-log"[10]*"(p)")
  ) }

    