#qq-plot

qqplot <- function(data, theoretical_line= TRUE, show_regression=FALSE, unifier = TRUE){
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  #1. GWAS daten einlesen
  
  if(unifier){
    original_data <- plink_regina_unifier(data)
    }else{original_data <- data}
  
  
  #2. Berechnen von QQ plot daten
  #beobachtete p-werte nach größe sortiert
  sortet_observed_p_values <- sort(original_data$P)
  
  #erwarteten p-werte
  n= length(sortet_observed_p_values)
  expectet_p_values= (1:n)/(n+1)
  
  #confidencintervall 
  a = 0.05
  
  o_confi= qbeta( a/2, 1:n, rev(1:n))
  u_confi= qbeta( 1-a/2, 1:n, rev(1:n))

  
  #3. daten Frame erstellen
  qqplot_data <- data.frame(P=sortet_observed_p_values, expectetP=expectet_p_values, Oberes_C=o_confi, Unteres_C=u_confi)
  #(3.)referenz lienie durch lineare regression
  fit <- lm(P ~ expectetP, data = qqplot_data)
  
  slope <- coef(fit)[2]
 intercept <- coef(fit)[1]
  
  #4.(datenmenge verkleinern)

qqplot_data <- slimmer(qqplot_data, notqqplot = FALSE)

  
 qq_plot <- ggplot(qqplot_data, aes(-log10(expectetP),-log10(P)))+
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

