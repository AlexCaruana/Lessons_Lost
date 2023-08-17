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

# Documentation (A)
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
        
        Document_Results <- ggplot(Document_DF, aes(fill= Funding, y = Count_2, x= Document_Type_Factor, group=majortype)) +
                geom_col(position="dodge",alpha=0.8, width=0.8) +
                scale_fill_manual(values = c("#7395C1","#0D1821"), labels=c("ERDF","LIFE")) +
                scale_y_continuous(labels = scales::percent, breaks = seq(from = 0, to = 0.9, by = 0.10), limits = c(0,0.9)) +
                theme_bw()+
                ylab("Proportion of projects for which technical documents are available") +
                guides(fill=guide_legend(title='')) +
                theme(axis.text.x = element_text(size = 10, angle = 50, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(legend.position = "none") +
                theme(axis.title.x = element_blank())+
                theme(plot.title = element_text(size = 10, hjust = 0.5))

        
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
        # After Only = 34
        # Before-After = 33
        # Control-Impact = 5
        # Before-After Control-Impact = 1
        # Randomized control trial (RCT) = 0
        # No Evaluation = 8
        # No Data Available = 34
        
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
        
        Count_3 <- c(12/345, 34/109,
                     6/345, 33/109,
                     0/345, 5/109,
                     1/345, 1/109,
                     0/345, 0/109,
                     43/345, 8/109,
                     283/345, 34/109)
        
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
        
        Evaluation_Results <- ggplot(Evaluation_DF, aes(fill= Funding_Evalulation, y = Count_3, x= Eval_Type_Factor, group=majortype)) +
                geom_col(position="dodge",alpha=0.8, width=0.8) +
                scale_fill_manual(values = c("#7395C1","#0D1821"), labels=c("ERDF","LIFE")) +
                scale_y_continuous(labels = scales::percent, breaks = seq(from = 0, to = 0.90, by = 0.10), limits = c(0,0.90)) +
                theme_bw()+
                ylab("Proportion of projects which have produced impact evaluations") +
                guides(fill=guide_legend(title='')) +
                theme(axis.text.x = element_text(size = 10, angle = 50, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(axis.title.x = element_blank()) +
                theme(plot.title = element_text(size = 10, hjust = 0.5))


# Peer-reviewed Papers
        ERDF_Publication_Frequency <- data.frame(table(ERDF$`If there were any linked scientific papers, how many scientific papers were there?`, useNA = "ifany"))
        LIFE_Publication_Frequency <- data.frame(table(LIFE$`If there were any linked scientific papers, how many scientific papers were there?`, useNA = "ifany"))
        
        Publication_Count <-c("1", "1",
                              "2", "2",
                              "3", "3",
                              "4", "4",
                              "5", "5",
                              "6", "6",
                              "7", "7",
                              "8", "8",
                              "10", "10",
                              "11", "11",
                              "12", "12",
                              "13", "13",
                              "24", "24")
        
        Publication_Group <- c("ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE",
                               "ERDF", "LIFE")
        
        Count_4 <- c(3,18,
                     1,14,
                     0,6,
                     0,5,
                     0,6,
                     0,3,
                     0,3,
                     0,3,
                     0,1,
                     0,1,
                     0,1,
                     0,2,
                     0,1)
        
        Publication_DF <- data.frame(Publication_Count, Publication_Group, Count_4) 
        Publication_DF$majortype<-sub("_.+$", "", Publication_DF$Publication_Group) 
        Publication_DF<- with(Publication_DF, Publication_DF[order(Publication_Count, Publication_Group, Count_4),])
        Publication_DF$order = factor(Publication_DF$Publication_Count , levels=c("1", "2", "3", "4","5","6","7","8","10","11","12","13","24"))
        
        Publication_Results <- ggplot(Publication_DF, aes(fill= Publication_Group, y = Count_4, x= order, group=majortype)) +
                geom_col(position="dodge",alpha=0.8, width=0.8) +
                scale_fill_manual(values = c("#7395C1","#0D1821"), labels=c("ERDF","LIFE")) +
                theme_bw()+
                ylab("Peer-reviewed papers published per project") +
                scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2), limits = c(0,20)) +
                guides(fill=guide_legend(title='')) +
                theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(axis.title.x = element_blank()) +
                theme(legend.position = "none") +
                theme(plot.title = element_text(size = 10, hjust = 0.5))

# Merged
# Exported as 1100px x 650px
Figure_4 <- Document_Results + Publication_Results + Evaluation_Results + plot_annotation(tag_levels = 'A') +
        plot_layout(ncol = 3)
