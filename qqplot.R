#qq-plot

qqplot <- function(data, theoretical_line= TRUE, show_regression=TRUE ){
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  #1. GWAS daten einlesen
  
  plink_regina_unifier <- function(inputfile_path){
    library(dplyr)
    
    if (is.data.frame(inputfile_path)){
      original_data <- inputfile_path
    }else {original_data <- read.csv(inputfile_path, sep="")}
    
    
    second_head <- colnames(original_data)[2]
    
    if (second_head == "POS"){
      data_plot <- original_data %>%
        select(CHROM="#CHROM", GENPOS=POS,ID, p.value=P)
      data_plot <- data_plot%>%
        filter(p.value <= 1)
      data_plot <- data_plot %>%
        mutate(CHROM = ifelse(CHROM %in% c("X", "Y", "MT", "0"), c("23","24","25","26"), CHROM))
    }else if (second_head == "GENPOS"){
      #print("Moin i am a regenie")
      data_plot <- original_data %>%
        select(CHROM, GENPOS,ID, p.value)
      data_plot <- data_plot%>%
        filter(p.value <= 1)
    }else{
      print("I dont know,pleas figur out what i am")
    }
    return(data_plot)}
  original_data <- plink_regina_unifier(data) 
  
  #2. Berechnen von QQ plot daten
  #beobachtete p-werte nach größe sortiert
  sortet_observed_p_values <- sort(original_data$p.value)
  
  #erwarteten p-werte
  n= length(sortet_observed_p_values)
  expectet_p_values= (1:n)/(n+1)
  
  #confidencintervall 
  a = 0.05
  
  o_confi= qbeta( a/2, 1:n, rev(1:n))
  u_confi= qbeta( 1-a/2, 1:n, rev(1:n))

  
  #3. daten Frame erstellen
  qqplot_data <- data.frame(Observed=sortet_observed_p_values, Expectet=expectet_p_values, Oberes_C=o_confi, Unteres_C=u_confi)
  #(3.)referenz lienie durch lineare regression
  fit <- lm(Observed ~ Expectet, data = qqplot_data)
  
  slope <- coef(fit)[2]
  intercept <- coef(fit)[1]
  
  #4.(datenmenge verkleinern)

    sig_data = qqplot_data %>%
      subset(Observed < threshold)
    
    notsig_data = qqplot_data %>%
      subset(Observed >= threshold) %>%
      sample_frac(fraction)
    
    slim_data <-rbind(sig_data,notsig_data)

  
 qq_plot <- ggplot(qqplot_data, aes(-log10(Expectet),-log10(Observed)))+
    geom_ribbon(aes(ymin = -log10(Oberes_C), ymax = -log10(Unteres_C)),fill="grey70",alpha=0.5) +
    geom_point()+
    theme_classic()
 if (theoretical_line){
    qq_plot <- qq_plot + geom_abline(slope = 1, intercept = 0, color = "darkred", alpha=0.6)
 if (show_regression) {
      qq_plot <- qq_plot + geom_abline(slope = slope, intercept = intercept, color = "darkblue", alpha=0.6)
    }
 }
 print(qq_plot)
 return(qq_plot)
}

