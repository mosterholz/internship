gw_manhattanplot <- function(path){
  library(ggplot2)
  library(dplyr)
  library(ggthemes)
  
  #Import the Data
  
  origninal_data<- read.csv(path, sep="")
  
  #Preparing Data
  ##slimming the data
  
  sig_data = origninal_data %>%
    subset(p.value < 0.05)
  
  notsig_data = origninal_data %>%
    subset(p.value >= 0.05) %>%
    group_by(CHROM) %>%
    sample_frac(0.15)
  
  shrunk_data<-rbind(sig_data,notsig_data)
  
  ##Preparing the x axse
  data_cum <- shrunk_data %>%
    arrange(CHROM) %>%
    group_by(CHROM) %>%
    summarise(max_GENPOS = max(GENPOS)) %>%
    mutate(GENPOS_add = lag(cumsum(as.numeric(max_GENPOS)),default=0)) %>%
    select(CHROM, GENPOS_add)
  
  shrunk_data <- shrunk_data %>%
    inner_join(data_cum, by= "CHROM") %>%
    mutate(GENPOS_cum= GENPOS+GENPOS_add)
  
  axis_set <-  shrunk_data %>%
    group_by(CHROM) %>%
    summarise(CENTER=median(GENPOS_cum))

  
  #Plotting Data
  
  ggplot(shrunk_data, aes(
    x=GENPOS_cum, y=-log10(p.value),
    color= as.factor(CHROM)
  ))+
    geom_point()+
    scale_x_continuous(
      label = as.factor(axis_set$CHROM),
      breaks = axis_set$CENTER)+
    scale_color_manual(values = rep(
      c("#006600", "#003300"),
      unique(length(axis_set$CHROM))
    )) +
    geom_hline(
      yintercept = -log10(5*10^(-8)), color = "grey40",
      linetype = "dashed")+
      labs(
        x = "Chromosome",
        y = expression("-log"[10]*"(p)")
        ) +
    theme_classic()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 60, size = 8, vjust = 0.5))
}
