## Lessons Lost - ERDF & LIFE Data Analysis
# Figure 2 - Bubble Plot

install.packages('hrbrthemes')
install.packages('viridis')
install.packages('ggrepel')
install.packages('gapminder')
install.packages('ggnewscale')
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(ggrepel)
library(gapminder)
library(ggnewscale)

getwd()
setwd('C:/Users/hert7250/Desktop/Lessons_Lost')

Bubble_DF <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "1.1 Bubble-Plot Analysis (R)")

Bubble_DF$Stratification_Group = factor(Bubble_DF$Stratification_Group, levels=c("Group 1: €6,950 - €15,644", 
                                                                                 "Group 2: €15,645 - €23,104", 
                                                                                 "Group 3: €23,105 - €36,199", 
                                                                                 "Group 4: €36,200 - €84,490"))
# ERDF Analysis
        ERDF_Bubble <- Bubble_DF %>%
                arrange(desc(Projects_ERDF)) %>%
                mutate(country = factor(Country, Country)) %>%
                ggplot(aes(x=GDP/1000, y=Funding_ERDF/1000000, size=Projects_ERDF, color=Stratification_Group)) +
                geom_point(alpha=0.5) +
                scale_size(range = c(0.1, 24), name="Total Projects") +
                ggrepel::geom_text_repel(aes(label = Country), size = 11 / .pt, color = 'black') +
                scale_color_brewer(palette = 'Dark2') +
                xlab("Real GDP per capita (€ Thousands)") +
                ylab("Funding (€ Millions)") +
                scale_y_continuous(breaks = seq(from = 0, to = 225, by = 25), limits = c(0,225)) +
                scale_x_continuous(breaks = seq(from = 0, to = 90, by = 10), limits = c(0,90)) +
                labs(colour="Stratification Groups") +
                theme(axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5)) +
                theme(plot.title = element_text(size = 11, hjust = 0.5)) +
                theme_bw(base_size=11) +
                theme(plot.title = element_text(size = 11, face = "bold"),
                legend.title=element_text(size=11), 
                legend.text=element_text(size=11)) #+
                #theme(legend.position = "none") #Version without legend generated as base figure for photoshop
                #Another Figure was generated with the legend then copied onto the base figure for the final result. 
        

# LIFE Analysis
        LIFE_Bubble <- Bubble_DF %>%
                arrange(desc(Projects_LIFE)) %>%
                mutate(Country = factor(Country, Country)) %>%
                ggplot(aes(x=GDP/1000, y=Funding_LIFE/1000000, size=Projects_LIFE, color=Stratification_Group)) +
                geom_point(alpha=0.5) +
                scale_size(range = c(0.1, 24), limits = c(1, 400), name="Total Projects") +
                ggrepel::geom_text_repel(aes(label = Country), size = 11 / .pt, color = 'black') +
                scale_color_brewer(palette = 'Dark2') +
                xlab("Real GDP per capita (€ Thousands)") +
                ylab("Funding (€ Millions)") +
                scale_y_continuous(breaks = seq(from = 0, to = 225, by = 25), limits = c(0,225)) +
                scale_x_continuous(breaks = seq(from = 0, to = 90, by = 10), limits = c(0,90)) +
                labs(colour="Stratification Groups") +
                theme_bw(base_size=11) +
                theme(axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5)) +
                theme(plot.title = element_text(size = 11, hjust = 0.5)) +
                theme(legend.position = "none")

# Merged
# Exported as 1000px x 1000px.  Legend then centered through photoshop.
Figure_2 <- LIFE_Bubble + ERDF_Bubble + plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 1)
