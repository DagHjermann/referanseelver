
### Functions for plotting parameters
#
# Based on "C:\Data\Referanseelver_2018\Chemistry\05_plot_chemistry_tukeyplots.Rmd"
# 
# Changes since that version:
#   1) Using emmeans instead of lsmeans
#   2) USE 'VALUE', not 'VALUE_JMO'
#   3) Set SAMPLE_NO to 2, not 'Part_sample_LF'
# https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
#

# Makes a new data frame for the Tukey results
# The 'y_nudge' parameter tells ggplot how far up the "tukey letters" should be plotted (distance above the max. value)



factor2char <- function(var) { levels(var)[var] }

make_tukeydf <- function(data, y_nudge = 1){
  model <- lm(VALUE ~ Rapportnavn, data = data)        # change
  leastsquare = emmeans(model, "Rapportnavn", adjust="tukey")
  CLD = CLD(leastsquare,
            alpha=0.05,
            Letters=letters,
            adjust="tukey")
  
  
  ###  Remove spaces in .group  
  
  CLD$.group=gsub(" ", "", CLD$.group)
  
  # change
  cld_position <- data %>%
    group_by(Rapportnavn) %>%
    summarise(VALUE = max(VALUE, na.rm = TRUE) + y_nudge)        # change
  cld_position$SAMPLE_NO <- 2     # just for plotting letter in middle
  
  # str(CLD)
  CLD$Rapportnavn <- factor2char(CLD$Rapportnavn)
  CLD <- left_join(CLD, cld_position, by = "Rapportnavn")

  ### Order the levels for printing
  CLD$Rapportnavn = factor(CLD$Rapportnavn, levels = levels(factor(data$Rapportnavn)))
  
  CLD
}

# debugonce(make_tukeydf)
# test2 <- make_tukeydf(test)

# NOTE: deprecated! Use instead make_tukeyplot_ordinary() for a Tukey plot on "non-log" scale
make_tukeyplot <- function(df1, df2, xlab = "Prøvenummer", ylab = "") {
  ggplot(df1, aes(SAMPLE_NO, VALUE, colour = as.factor(SAMPLE_NO))) + 
    geom_point() +
    geom_text(data = df2, aes(label = .group), color = "black") +
    facet_grid(.~Rapportnavn) +
    scale_color_hue("Prøvenr") +
    scale_x_continuous(breaks = c(1,2,3)) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white"))
}
# df_select <- subset(df_chem, NAME %in% "Fettinnhold")
# tukeydf <- make_tukeydf(df_select)
# gg1 <- make_tukeyplot(df_select, tukeydf)

plot_tukey <- function(param, ylab, y_nudge = 1, data = df_chem){
  df1 <- subset(data, NAME %in% param)
  df2 <- make_tukeydf(df1, y_nudge = y_nudge)
  make_tukeyplot(df1, df2, ylab = ylab)
}
# test
# plot_tukey("Fettinnhold", "Fettinnhold (%)")



# Analyse and plot log-values
# Log-transforms before running Tukey, then back-transforms
# Also plots a summery of data, plus the EQS (for setting y axis breaks later)
perform_logtukeytest <- function(param, print = TRUE, y_nudge = 0.1, A = 0, data = df_chem, eqsdata = df_eqs){
  df1 <- subset(data, NAME %in% param)
  df1 <- subset(df1, !is.na(VALUE))
  df1$VALUE <- log10(df1$VALUE + A)    # log-transform original data
  df2 <- make_tukeydf(df1, y_nudge = y_nudge)  # perform Tukey on log-transformed data
  df1$VALUE <- 10^df1$VALUE - A        # back-transform both datasets (original + result of Tukey)
  df2$VALUE <- 10^df2$VALUE - A
  eqsdata <- subset(eqsdata, NAME %in% param)
  if (print){
    cat("\nEQS =", eqsdata$EQS, "\n")
    print(quantile(df1$VALUE, na.rm = TRUE))
  }
  list(df1=df1, df2=df2, df_eqs=eqsdata)
}

# Note: assuming that
perform_tukeytest <- function(param, print = TRUE, y_nudge = 0.1, A = 0, data = df_chem, eqsdata = df_eqs){
  df1 <- subset(data, NAME %in% param)
  df1 <- subset(df1, !is.na(VALUE))
  df2 <- make_tukeydf(df1, y_nudge = y_nudge)  # perform Tukey on log-transformed data
  eqsdata <- subset(eqsdata, NAME %in% param)
  if (print){
    cat("\nEQS =", eqsdata$EQS, "\n")
    print(quantile(df1$VALUE, na.rm = TRUE))
  }
  list(df1=df1, df2=df2, df_eqs=eqsdata)
}

debugonce(perform_tukeytest)
# testlist <- perform_tukeytest("Sum PCB 7", y_nudge = 0.5, data = df_chem)
# testlist <- perform_logtukeytest("Sum PCB 7", y_nudge = 0.5)



# https://stackoverflow.com/questions/38722202/how-do-i-change-the-number-of-decimal-places-on-axis-labels-in-ggplot2
no_extra_digits <- function(x) sprintf("%g", x)
# test
# no_extra_digits(c(0.01, 0.1, 1, 10, 100))

#
# Main function used
# letterposition = 0 prints "Tukey letters" above the highest point in each group
# letterposition = -1 prints "Tukey letters" above the overall highest point
# letterposition = some number over 0 gives y position of the "Tukey letters"
#
make_tukeyplot_log <- function(datalist, xlab = "Prøvenummer", ylab = "", ybreaks = NULL, extra_limit = NA,
                               letterposition = 0, linecolors = c("#e31a1c", "#ff7f00")) {
  df1 <- datalist[[1]]
  df2 <- datalist[[2]]
  if (letterposition %in% -1){
    df2$VALUE <- max(df2$VALUE, na.rm = TRUE)
  } else if (letterposition > 0) {
    df2$VALUE <- letterposition
  }
  limits <- datalist[[3]]$EQS
  #if (!is.na(extra_limit))
  #  limits <- c(limits, extra_limit)
  gg <- ggplot(df1, aes(SAMPLE_NO, VALUE, fill = as.factor(SAMPLE_NO),
                        shape = as.factor(SAMPLE_NO))) +
    geom_point(size = rel(3)) +
    geom_text(data = df2, aes(label = .group), color = "black") +
    geom_hline(yintercept = limits, colour = linecolors[1], linetype = 2, size = 1) +
    facet_grid(.~Rapportnavn) +
    scale_shape_manual("Prøvenr", values = c(24,22,25)) +
    scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
    scale_x_continuous(breaks = c(1,2,3), limits = c(0.5,3.5)) +
    scale_y_log10(breaks = ybreaks, labels = no_extra_digits) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white")) +
    theme(legend.position = "none")
  if (!is.na(extra_limit))
    gg <- gg + geom_hline(yintercept = extra_limit, colour = linecolors[2], linetype = 2, size = 1)
  gg
}


# make_tukeyplot_log(testlist, ylab = "Sum PCB 7", ybreaks = c(0.0002, 0.001, 0.01))
# make_tukeyplot_log(testlist, ylab = "Sum PCB 7", ybreaks = c(0.0002, 0.001, 0.01), letterposition = -1)
# make_tukeyplot_log(testlist, ylab = "Sum PCB 7", ybreaks = c(0.0002, 0.001, 0.01), letterposition = 0.05)
# make_tukeyplot_log(testlist, ylab = "Sum PCB 7", ybreaks = c(0.0002, 0.001, 0.01), extra_limit = 0.001)

# For PFAS: shape = Prøvevekt instead of shape = SAMPLE_NO
make_tukeyplot_log_pfas <- function(datalist, xlab = "Prøvenummer", ylab = "", ybreaks = NULL, extra_limit = NA,
                                    letterposition = 0, linecolors = c("#e31a1c", "#ff7f00")) {
  df1 <- datalist[[1]]
  df2 <- datalist[[2]]
  if (letterposition %in% -1){
    df2$VALUE <- max(df2$VALUE, na.rm = TRUE)
  } else if (letterposition > 0) {
    df2$VALUE <- letterposition
  }
  limits <- datalist[[3]]$EQS
  #if (!is.na(extra_limit))
  #  limits <- c(limits, extra_limit)
  df1 <- df1 %>%
    mutate(Prøvevekt = case_when(
      Sample_weight < 0.2 ~ "Prøvevekt < 0.2 g",       # cross = symbol 4
      Sample_weight < 0.3 ~ "Prøvevekt 0.2 - 0.3 g",   # open square = symbol 0
      Sample_weight >= 0.3 ~ "Prøvevekt >= 0.3 g"))    # filled square = symbol 16
  gg <- ggplot(df1, aes(SAMPLE_NO, VALUE, fill = as.factor(SAMPLE_NO))) +
    geom_point(aes(shape = Prøvevekt), size = rel(3)) +       # special for PFAS
    geom_text(data = df2, aes(label = .group), color = "black") +
    geom_hline(yintercept = limits, colour = linecolors[1], linetype = 2, size = 1) +
    facet_grid(.~Rapportnavn) +
    scale_shape_manual("Prøvevekt", values = c(4,16,0)) +   # see symbol overview above
    scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
    scale_x_continuous(breaks = c(1,2,3), limits = c(0.5,3.5)) +
    scale_y_log10(breaks = ybreaks, labels = no_extra_digits) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white")) +
    theme(legend.position = "none")
  if (!is.na(extra_limit))
    gg <- gg + geom_hline(yintercept = extra_limit, colour = linecolors[2], linetype = 2, size = 1)
  gg
}


make_tukeyplot_ordinary <- function(datalist, xlab = "Prøvenummer", ylab = "", ybreaks = NULL, extra_limit = NA,
                                    letterposition = 0) {
  df1 <- datalist[[1]]
  df2 <- datalist[[2]]
  if (letterposition %in% -1){
    df2$VALUE <- max(df2$VALUE, na.rm = TRUE)
  } else if (letterposition > 0) {
    df2$VALUE <- letterposition
  }
  limits <- datalist[[3]]$EQS
  if (!is.na(extra_limit))
    limits <- c(limits, extra_limit)
  ggplot(df1, aes(SAMPLE_NO, VALUE, fill = as.factor(SAMPLE_NO), 
                  shape = as.factor(SAMPLE_NO))) +
    geom_point(size = rel(3)) +
    geom_text(data = df2, aes(label = .group), color = "black") +
    geom_hline(yintercept = limits, color = "red", linetype = 2) +
    facet_grid(.~Rapportnavn) +
    scale_shape_manual("Prøvenr", values = c(24,22,25)) +   # see symbol overview above
    scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
    scale_x_continuous(breaks = c(1,2,3), limits = c(0.5,3.5)) +
    scale_y_continuous(breaks = ybreaks) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white")) +
    theme(legend.position = "none")
}



make_tukeyplot_pah_log <- function(datalist, xlab = "Prøvenummer", ylab = "", ybreaks = NULL, extra_limit = NA,
                               letterposition = 0, linecolors = c("#e31a1c", "#ff7f00")) {
  df1 <- datalist[[1]]
  df2 <- datalist[[2]]
  if (letterposition %in% -1){
    df2$VALUE <- max(df2$VALUE, na.rm = TRUE)
  } else if (letterposition > 0) {
    df2$VALUE <- letterposition
  }
  limits <- datalist[[3]]$EQS
  #if (!is.na(extra_limit))
  #  limits <- c(limits, extra_limit)
  gg <- ggplot(df1 %>% arrange(desc(SAMPLE_NO)), aes(SAMPLE_NO, VALUE, fill = SAMPLE_NO)) +
    geom_point(size = rel(3), pch = 21) +
    geom_text(data = df2, aes(label = .group), color = "black") +
    geom_hline(yintercept = limits, colour = linecolors[1], linetype = 2, size = 1) +
    facet_grid(.~Rapportnavn) +
    scale_y_log10(breaks = ybreaks, labels = no_extra_digits) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white")) +
    theme(legend.position = "none")
  if (!is.na(extra_limit))
    gg <- gg + geom_hline(yintercept = extra_limit, colour = linecolors[2], linetype = 2, size = 1)
  gg
}

