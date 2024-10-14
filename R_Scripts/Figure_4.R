## Lessons Lost - ERDF & LIFE Data Analysis
# Figure 4 - Bar Charts

library(ggpattern)
library(ggplot2)
library(readxl)
library(devtools)
library(patchwork)
library(magrittr)
library(ggpmisc)
library(dplyr)
library(ggpubr)
library(tidyr)

getwd()
setwd('C:/Users/hert7250/Desktop/Lessons_Lost')

# Documentation (A)
        ERDF <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.1 ERDF Dataset")
        LIFE <- read_excel("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.2 LIFE Dataset")
        
        ERDF_Documents <- data.frame(ERDF$`What type of documentation was available (if any)?`)
        ERDF_Documents_Mutated <- ERDF_Documents %>% 
                mutate(ERDF..What.type.of.documentation.was.available..if.any... = strsplit(as.character(ERDF..What.type.of.documentation.was.available..if.any...), ";")) %>% 
                unnest(ERDF..What.type.of.documentation.was.available..if.any...)
        
        
        ERDF_Document_Frequency <- data.frame(table(ERDF_Documents_Mutated$ERDF..What.type.of.documentation.was.available..if.any..., useNA = "ifany"))
        
        # Short Summary Website = 78
        # Project Description/Plans = 38
        # Monitoring Report = 8
        # Scientific Papers = 5
        # Project Conclusion Documents = 7
        # No Data Available = 243
        # In-depth website = 9
        
        LIFE_Documents <- data.frame(LIFE$`What type of documentation was available (if any)?`)
        LIFE_Documents_Mutated <- LIFE_Documents %>% 
                mutate(LIFE..What.type.of.documentation.was.available..if.any... = strsplit(as.character(LIFE..What.type.of.documentation.was.available..if.any...), ";")) %>% 
                unnest(LIFE..What.type.of.documentation.was.available..if.any...)
        
        LIFE_Document_Frequency <- data.frame(table(LIFE_Documents_Mutated$LIFE..What.type.of.documentation.was.available..if.any..., useNA = "ifany"))
        # Short Summary/Mention Website = 9
        # Project Description/Plans = 71
        # Monitoring Report = 41
        # Scientific Papers = 65
        # Project Conclusion Documents = 56
        # No Data Available = 5
        # In-depth website = 94
        
        Document_Type <-c("Short summary website", "Short summary website",
                          "In-depth website", "In-depth website",
                          "Monitoring reports", "Monitoring reports",
                          "No data available", "No data available",
                          "Project conclusion documents", "Project conclusion documents",
                          "Project description/plans", "Project description/plans",
                          "Peer-reviewed papers", "Peer-reviewed papers")
        
        Funding <- c("ERDF", "LIFE",
                     "ERDF", "LIFE",
                     "ERDF", "LIFE",
                     "ERDF", "LIFE",
                     "ERDF", "LIFE",
                     "ERDF", "LIFE",
                     "ERDF", "LIFE")
        
        # Manually converted into percentages
        Count_2 <- c(78/345, 9/109,
                     9/345, 94/109,
                     8/345, 40/109,
                     243/345, 5/109,
                     7/345, 56/109,
                     38/345, 71/109,
                     5/345, 65/109)
        
        
        Document_DF <- data.frame(Document_Type, Funding, Count_2) 
        Document_DF$majortype<-sub("_.+$", "", Document_DF$Funding) 
        Document_DF<- with(Document_DF, Document_DF[order(Document_Type, Funding, Count_2),])
        Document_DF$Document_Type_Factor = factor(Document_DF$Document_Type , levels=c("Project description/plans",
                                                                                       "Monitoring reports",
                                                                                       "Project conclusion documents",
                                                                                       "Peer-reviewed papers",
                                                                                       "In-depth website",
                                                                                       "Short summary website",
                                                                                       "No data available"))
        
        Document_Results <- ggplot(Document_DF, aes(fill= Funding, y = Count_2, x= Document_Type_Factor, group=forcats::fct_rev(majortype))) +
                geom_col(position="dodge",alpha=0.8, width=0.8) +
                scale_fill_manual(values = c("#295E11","#58094F"), labels=c("ERDF","LIFE")) +
                scale_y_continuous(labels = scales::percent, breaks = seq(from = 0, to = 0.9, by = 0.10), limits = c(0,0.9)) +
                theme_bw()+
                ylab("Proportion of projects for which technical documents are available") +
                guides(fill=guide_legend(title='')) +
                theme(axis.text.x = element_text(size = 11, angle = 68, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(legend.position = "none") +
                theme(axis.title.x = element_blank())+
                theme(plot.title = element_text(size = 11, hjust = 0.5))

        
# Evaluation (B)
        ERDF_Evaluation <- data.frame(ERDF$`If there was any form of evaluation, what type of study design was used?`)
        ERDF_Evaluation_Mutated <- ERDF_Evaluation %>% 
                mutate(ERDF..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used.. = strsplit(as.character(ERDF..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used..), ";")) %>% 
                unnest(ERDF..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used..)
        
        ERDF_Evaluation_Frequency <- data.frame(table(ERDF_Evaluation_Mutated$ERDF..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used.., useNA = "ifany"))
        # After only = 12
        # Before-after = 6
        # Control-impact = 0
        # Before-after control-impact = 1
        # Randomized control trial (RCT) = 0
        # No Evaluation = 43
        # N/A = 283
        
        LIFE_Evaluation <- data.frame(LIFE$`If there was any form of evaluation, what type of study design was used?`)
        LIFE_Evaluation_Mutated <- LIFE_Evaluation %>% 
                mutate(LIFE..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used.. = strsplit(as.character(LIFE..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used..), ";")) %>% 
                unnest(LIFE..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used..)
        
        LIFE_Evaluation_Frequency <- data.frame(table(LIFE_Evaluation_Mutated$LIFE..If.there.was.any.form.of.evaluation..what.type.of.study.design.was.used.., useNA = "ifany"))
        # After Only = 31
        # Before-After = 34
        # Control-Impact = 3
        # Before-After Control-Impact = 1
        # Randomized control trial (RCT) = 0
        # No Evaluation = 7
        # No Data Available = 33
        
        Evaluation_Type <-c("After only", "After only",
                            "Before-after (BA)", "Before-after (BA)",
                            "Control-impact (CI)", "Control-impact (CI)",
                            "Before-after control-impact (BACI)", "Before-after control-impact (BACI)",
                            "Randomized control trial (RCT)", "Randomized control trial (RCT)",
                            "No evaluation", "No evaluation",
                            "No data available", "No data available")
        
        Funding_Evalulation <- c("ERDF", "LIFE",
                                 "ERDF", "LIFE",
                                 "ERDF", "LIFE",
                                 "ERDF", "LIFE",
                                 "ERDF","LIFE",
                                 "ERDF","LIFE",
                                 "ERDF","LIFE")
        
        Count_3 <- c(12/345, 31/109,
                     6/345, 34/109,
                     0/345, 3/109,
                     1/345, 1/109,
                     0/345, 0/109,
                     43/345, 7/109,
                     283/345, 33/109)
        
        Evaluation_DF <- data.frame(Evaluation_Type, Funding_Evalulation, Count_3) 
        Evaluation_DF$majortype<-sub("_.+$", "", Evaluation_DF$Funding_Evalulation) 
        Evaluation_DF<- with(Evaluation_DF, Evaluation_DF[order(Evaluation_Type, Funding_Evalulation, Count_3),])
        Evaluation_DF$Eval_Type_Factor = factor(Evaluation_DF$Evaluation_Type , levels=c("After only",
                                                                                         "Before-after (BA)",
                                                                                         "Control-impact (CI)",
                                                                                         "Before-after control-impact (BACI)",
                                                                                         "Randomized control trial (RCT)",
                                                                                         "No evaluation",
                                                                                         "No data available"))
        
        Evaluation_Results <- ggplot(Evaluation_DF, aes(fill= Funding_Evalulation, y = Count_3, x= Eval_Type_Factor, group=forcats::fct_rev(majortype))) +
                geom_col(position="dodge",alpha=0.8, width=0.8) +
                scale_fill_manual(values = c("#295E11","#58094F"), labels=c("ERDF","LIFE")) +
                scale_y_continuous(labels = scales::percent, breaks = seq(from = 0, to = 0.90, by = 0.10), limits = c(0,0.90)) +
                theme_bw()+
                ylab("Proportion of projects which have produced impact evaluations") +
                guides(fill=guide_legend(title='', reverse=T)) +
                theme(axis.text.x = element_text(size = 11, angle = 68, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(axis.title.x = element_blank()) +
                theme(plot.title = element_text(size = 11, hjust = 0.5))


# Peer-Review Publication Plot
        ERDF <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.1 ERDF Dataset")
        LIFE <- read_excel("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.2 LIFE Dataset")
        
        ERDF_Publication_Box<- ERDF
        ERDF_Publication_Box[ ,c("Is the project’s name or grant code found on Google?", 
                                    "does the project have its own dedicated website?", 
                                    "What type of information is available online?",
                                    "Is the project’s name or grant code cited on Google Scholar?", 
                                    "Is there any technical documentation associated with the project’s name or grant code available online?", 
                                    "What type of documentation was available (if any)?",
                                    "Based on the data available in technical documentation online, did any form of evaluation occur?", 
                                    "If there was any form of evaluation, what type of study design was used?", 
                                    "Any relevant links?",
                                    "Any additional information?")] <- list(NULL)
        ERDF_Publication_Box <- replace(ERDF_Publication_Box, is.na(ERDF_Publication_Box), 0)
        
        ERDF_Publication_Box <- ERDF_Publication_Box %>%
          mutate(Label = "ERDF")
        ERDF_Publication_Box <- setNames(ERDF_Publication_Box, c("Project_ID", "Publications", "Label"))
        
        
        LIFE_Publication_Box <- LIFE
        LIFE_Publication_Box[ ,c("Is the project’s name or grant code found on Google?", 
                                    "does the project have its own dedicated website?",
                                    "What type of information is available online?",
                                    "Is the project’s name or grant code cited on Google Scholar?", 
                                    "Is there any technical documentation associated with the project’s name or grant code available online?", 
                                    "What type of documentation was available (if any)?",
                                    "Based on the data available in technical documentation online, did any form of evaluation occur?", 
                                    "If there was any form of evaluation, what type of study design was used?", 
                                    "Any relevant links?",
                                    "Any additional information?")] <- list(NULL)
        LIFE_Publication_Box <- replace(LIFE_Publication_Box, is.na(LIFE_Publication_Box), 0)
        LIFE_Publication_Box <- LIFE_Publication_Box %>%
          mutate(Label = "LIFE")
        LIFE_Publication_Box <- setNames(LIFE_Publication_Box, c("Project_ID", "Publications", "Label"))
        
        LIFE_ERDF_Box<-rbind(ERDF_Publication_Box, LIFE_Publication_Box)
        str(LIFE_ERDF_Box)
        LIFE_ERDF_Box <- setNames(LIFE_ERDF_Box, c("Project_ID", "Publications", "Label"))
      
        
        PublicationBoxPlot <-ggplot(LIFE_ERDF_Box, aes(x = forcats::fct_rev(Label), y = Publications, fill = Label)) +
          geom_boxplot(alpha = 0.6, width = 0.35, outlier.color=NA) + 
          coord_cartesian(ylim =  c(0, 14)) + 
          geom_point(position = position_jitter(seed = 1, width = 0.05, h = 0.15), alpha = 0.3) +
          theme(legend.position = "none")+
          scale_fill_manual(values = c("#295E11","#58094F"), labels=c("ERDF","LIFE"))+
          scale_y_continuous(breaks = seq(from = 0, to = 14, by = 1), limits = c(0,14)) +
          theme_bw()+ 
          ylab("Peer-reviewed papers published per project") +
          theme(legend.position="none") +
          theme(axis.title.x=element_blank())+
          theme(axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5)) +
          theme(plot.title = element_text(size = 11, hjust = 0.5))
        
      
        #IIQ Range calculation
        group_by(LIFE_ERDF_Box, Label) %>%
          summarise(
            count = n(),
            median = median(Publications, na.rm = TRUE),
            IQR = IQR(Publications, na.rm = TRUE)
          )
        
        tapply(LIFE_ERDF_Box$Publications, LIFE_ERDF_Box$Label, median)
        temp <- LIFE_ERDF_Box %>% group_by(Label) %>%
          summarize(first=quantile(Publications,probs=0.25),
                    second=quantile(Publications,probs=0.5),
                    third=quantile(Publications,probs=0.75))
        
        # Statistical significance between publication count ERDF vs LIFE, not used
        #Result_Wilcox <- wilcox.test(Publications ~ Label, data = LIFE_ERDF_Box,exact = FALSE)
        #Result_Wilcox$p.value 
            
# Merged
# Exported as 900px x 700px
Figure_4 <- Document_Results + PublicationBoxPlot + Evaluation_Results + plot_annotation(tag_levels = 'A') +
        plot_layout(ncol = 3)

