#Clear workspace and load packages
rm(list = ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(rstatix)
library(ggpubr)

#This script is designed to test mixed data with both within- and between- subjects factors
  #It is OK if columns have names/headers or do not; 
  #column names will be added/changed to work with the functions

  #File format column order matters:
  #   |---animal---|---condition---|---drug---|---measure---|
  # e.g., Two "condition" ~ SNI/sham, Two "Drug" ~ saline/DAMGO

#choose datafile, format data
path <- rstudioapi::selectFile(caption = "Select CSV File",
                               filter = "CSV Files (*.csv)",
                               existing = TRUE)
all_data <- readr::read_csv(path)
names(all_data)[1] <- "animal"
names(all_data)[2] <- "condition"
names(all_data)[3] <- "drug"
names(all_data)[4] <- "measure"
head(all_data, 5)
condit_labels <- unique(all_data[2])
drug_labels <- unique(all_data[3])

#Get means, SD, SEM, and number of samples per condition
all_data %>% 
  group_by(condition, drug) %>%
  summarise(means = mean(measure))
all_data %>%
  group_by(condition, drug) %>%
  summarise(SD = sd(measure))
all_data %>%
  group_by(condition, drug) %>%
  summarise(SEM = sd(measure)/sqrt(length(measure)))
all_data %>%
  group_by(condition, drug) %>%
  summarise(num_samples = length(measure))

#Generate violins grouped by drug and condition for visual check
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
init_distrib <- ggviolin(all_data, x = "condition", y = "measure", fill = "drug", 
                         palette = c("#00AFBB", "#E7B800", "#FC4E07"))
add_summary(init_distrib, group = "drug", "mean_se")

#Check for outliers
all_data %>%
  group_by(drug, condition) %>%
  identify_outliers(measure)

#Test the normality assumption with the Shapiro Test
#if p < 0.05 distributions are not normal
all_data %>%
  group_by(drug, condition) %>%
  shapiro_test(measure)

#Make a graph to inspect the residuals
#Dots off the lines violate the normality assumption
ggqqplot(all_data, "measure", ggtheme = theme_bw()) + facet_grid(condition ~ drug)

#Test the homogeneity of variance condition
#Levene's test: p < 0.05 violates the condition
all_data %>%
  group_by(condition) %>%
  levene_test(measure ~ drug)
#Or, use the Bartlett test if there is a strong expectation that the data are normally
#distributed. Data have homogeneity of variance if p < 0.05
bartlett_result = bartlett.test(measure ~ interaction(condition, drug), data = all_data)
print(bartlett_result)
#If a non-parametric test is more appropriate, use Fligner-Killeen to test for 
#homogeneity of variance, based on ranks. If p < 0.05 variances cannot be considered =
fligner_result = fligner.test(measure ~ interaction(condition,drug), data = all_data)
print(fligner_result)

#If the dataset has passed the conditions for parametric testing
#(normality and homogeneity of variance)
#perform the mixed ANOVA
res.aov <- anova_test(
  data = all_data, dv = measure, wid = animal,
  between = condition, within = drug)
get_anova_table(res.aov)

#Simple main effect post hoc test, across conditions
one.way <- all_data %>%
  group_by (drug) %>%
  anova_test(dv = measure, wid = animal, between = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

#Post hoc pairwise comparisons with Bonferroni, within "condition" comparisons
pwc <- all_data %>%
  group_by(condition) %>%
  pairwise_t_test(measure ~ drug, p.adjust.method = "bonferroni")
pwc

#Pairwise comparisons if there is no significant two-way interaction
all_data %>%
  pairwise_t_test(
    measure ~ drug, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

#And on the condition variable
all_data %>%
  pairwise_t_test(
    measure ~ condition, p.adjust.method = "bonferroni"
  )

#Non-parametric tests, if necessary
#For Wilcoxon paired samples test, group data by condition first, 
#then run within subject comparisons
conditiononedata <- filter(all_data, condition == toString(condit_labels[1,])) #condit_labels[1,])
conditiontwodata <- filter(all_data, condition == toString(condit_labels[2,]))
conditoneresult <- wilcox.test(measure ~ drug, data = conditiononedata, paired = TRUE)
condittworesult <- wilcox.test(measure ~ drug, data = conditiontwodata, paired = TRUE)
print(condit_labels[1,])
conditoneresult
print(condit_labels[2,])
condittworesult

#Make violin graphs with PLOTLY
rm(grouped_violins)
grouped_violins <- all_data %>%
  plot_ly(type = 'violin')
grouped_violins <- grouped_violins %>%
  add_trace(
    x = ~condition[all_data$drug == toString(drug_labels[1,])],
    y = ~measure[all_data$drug == toString(drug_labels[1,])],
    legendgroup = toString(drug_labels[1,]),
    scalegroup = toString(drug_labels[1,]),
    name = toString(drug_labels[1,]),
    meanline = list(
      visible = T
    ),
    color = I("red")
  )
grouped_violins <- grouped_violins %>%
  add_trace(
    x = ~condition[all_data$drug == toString(drug_labels[2,])],
    y = ~measure[all_data$drug == toString(drug_labels[2,])],
    legendgroup = toString(drug_labels[2,]),
    scalegroup = toString(drug_labels[2,]),
    name = toString(drug_labels[2,]),
    meanline = list(
      visible = T
    ),
    color = I("grey")
  )
yaxisproperties <- list(
  showgrid = FALSE,
  showline = TRUE,
  linewidth = 2,
  ticks = 'outside',
  tick0 = 0,
  ticklen = 8,
  tickwidth = 2,
  title = "Measurement"
)
grouped_violins <- grouped_violins %>%
  layout(
    violinmode = 'group',
    yaxis = yaxisproperties,
    xaxis = list(title = "Group")
  )
grouped_violins
#if the graph looks OK in the Viewer, you can ignore the warnings
#if you redraw the graph (re-run the commands) you must first run line 134 to clear 
#the previous version of the plotly object

#edit the file name and location for the saved violin plot here:
orca(grouped_violins, "myviolins.svg")

#get the data range to set the parallel axes ranges to be equal
ymax <- max(all_data['measure'])
yaxismax <- signif(ymax, digits = 2)
ymin <- min(all_data["measure"])
yaxismin <- signif(ymin, digits = 2)

#Make parallel pre- post- plots with ggplot
#Then "Export" this output from the Plots tab to a PDF
prepost_conditone <- ggplot(data = conditiononedata, aes(x=drug, y=measure, group=animal)) +
  geom_line() +
  geom_point(size = 5)
prepost_condittwo <- ggplot(data = conditiontwodata, aes(x=drug, y=measure, group=animal)) +
  geom_line() +
  geom_point(size = 5)
plot1 <- prepost_conditone + theme_classic() +
  ylim(yaxismin, yaxismax) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(name=toString(condit_labels[1,]), limits=c(toString(drug_labels[1,]), toString(drug_labels[2,])))
plot2 <- prepost_condittwo + theme_classic() +
  ylim(yaxismin, yaxismax) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(name=toString(condit_labels[2,]), limits=c(toString(drug_labels[1,]), toString(drug_labels[2,])))
temp_fig <- grid.arrange(plot1, plot2, ncol=2)

