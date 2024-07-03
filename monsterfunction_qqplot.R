# now build the qq-plot "~/Desktop/Praxis Modul/playground/learn_enviroment/antiTNF_bioNaive_7pc_sex_regenie_firth_clinicalRemission_UC_plainpvalue.regenie.gz"
qq_plot <- function(path){
library(dplyr)
library(ggplot2)
library(ggthemes)

original_data <- read.csv(path, sep="")


sortet_observed_p_values <- sort(original_data$p.value)

n= length(sortet_observed_p_values)
expectet_p_values= (1:n)/(n+1)

a = 0.05

o_confi= qbeta( a/2, 1:n, rev(1:n))
u_confi= qbeta( 1-a/2, 1:n, rev(1:n))

qqplot_data <- data.frame(Observed=sortet_observed_p_values, Expectet=expectet_p_values, Oberes_C=o_confi, Unteres_C=u_confi)

sig_data = qqplot_data %>%
  subset(Observed < 0.05)

notsig_data = qqplot_data %>%
  subset(Observed >= 0.05) %>%
  sample_frac(0.15)

qqplot_data <-rbind(sig_data,notsig_data)


ggplot(qqplot_data, aes(-log10(Expectet),-log10(Observed)))+
  geom_point()+
  geom_ribbon(aes(ymin = -log10(Oberes_C), ymax = -log10(Unteres_C)),fill="grey70",alpha=0.5) +
  geom_abline(slope=1, intercept= 0, color="grey60",alpha=0.6)+
  theme_classic()
}

