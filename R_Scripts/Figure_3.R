## Lessons Lost - ERDF & LIFE Data Analysis
# Figure 3 - 100% stacked bar plot

install.packages('ggpubr')
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
setwd('C:/Users/Alex/Desktop/Masters Thesis/Final Sorted Datasets - Github')

# ERDF Analysis
        ERDF <- read_xlsx("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.1 ERDF Dataset")
        
        table(ERDF['Is the project’s name or grant code found on Google?'])
        # No = 233
        # Yes = 112
        
        table(ERDF$'What type of information is available online?', useNA = "ifany")
        # In-Depth = 9 
        # Short Summary Website = 78 
        # Unknown - Offline = 3 
        # No Data Available (22) + NA (233) = 255
        
        table(ERDF$'Is the project’s name or grant code cited on Google Scholar?', useNA = "ifany")
        # No = 341 
        # Yes = 4
        
        table(ERDF$'Is there any technical documentation associated with the project’s name or grant code available online?', useNA = "ifany")
        # No = 255
        # Yes = 90
        
        table(ERDF$'Based on the data available in technical documentation online, did any form of evaluation occur?', useNA = "ifany")
        # No = 67
        # Yes = 18
        # Ongoing = 31
        # No Data Available = 229
        
        ERDF_Question <-c("Is the project’s name or grant code found on Google?","Is the project’s name or grant code found on Google?",
                          "Is the project’s name or grant code cited on Google Scholar?","Is the project’s name or grant code cited on Google Scholar?",
                          "What type of information is available online?","What type of information is available online?","What type of information is available online?","What type of information is available online?",
                          "Is there any technical documentation associated with the project’s name or grant code available online?","Is there any technical documentation associated with the project’s name or grant code available online?",
                          "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?")
        
        ERDF_Classification <- c("Yes","No",
                                 "Yes","No",
                                 "In-Depth","Short Summary Website","Unknown - Offline","No Data Available",
                                 "Yes","No",
                                 "Yes","No","Ongoing","No Data Available")
        
        ERDF_Count <- c(112,233,
                        4,341,
                        9,78,3,255,
                        90,255,
                        18,67,31,229)
        
        ERDF_Frequency <- c(112/345,233/345,
                            4/345,341/345,
                            9/345,78/345,3/345,255/345,
                            90/345,255/345,
                            18/345,67/345,31/345,229/345)
        
        ERDF_Statistics <- data.frame(ERDF_Question, ERDF_Classification, ERDF_Count, ERDF_Frequency) 
        ERDF_Statistics$Frequency <- round(ERDF_Statistics$ERDF_Frequency, 2)
        ERDF_Statistics$GraphOrder = factor(ERDF_Statistics$ERDF_Classification, levels=c("Yes","No","Ongoing", "In-Depth",
                                                                                          "Short Summary Website","Unknown - Offline", "No Data Available"))
        
        ERDF_Statistics$QuestionOrder = factor(ERDF_Statistics$ERDF_Question, levels=c("Based on the data available in technical documentation online, did any form of evaluation occur?",
                                                                                       "Is there any technical documentation associated with the project’s name or grant code available online?",
                                                                                       "What type of information is available online?",
                                                                                       "Is the project’s name or grant code cited on Google Scholar?",
                                                                                       "Is the project’s name or grant code found on Google?"))
        
        # Original ("#1A5D1A","#630000","#ba7411", "#001E6C","#035397","#72797a", "#000000")
        # Original (Green, Red, Orange, Dark Blue, Light Blue, Light Grey, Black)
        # Paul Toll Library ("#2E2585","#337538","#DCCD7D", "#7E2954","#C26A77","#72797a", "#000000")
        # Okabe Library ("#1A5D1A","#D55E00","#E69F00", "#009E73","#CC79A7","#72797a", "#000000")
        # IBM Library ("#0072B2","#FF5F00","#FFB00D", "#2E2585","#DD217D","#72797a", "#000000")

        
        ERDF_Review_Fig <- ERDF_Statistics %>%
                ggplot(aes(fill=GraphOrder, x = QuestionOrder, y = ERDF_Frequency)) + 
                geom_bar(position = position_stack(reverse = TRUE), stat="identity", alpha=0.7) + 
                scale_y_continuous(labels = scales::percent) +
                scale_fill_manual(values = c("#1A5D1A","#D55E00","#E69F00", "#052955","#A7B8F8","#72797a", "#000000")) +
                coord_flip() +
                theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+ 
                theme_bw() +
                theme(legend.position = "none")+
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()) +
                ggtitle("ERDF") +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.margin = margin(5.5, 1.5, 5.5, 5.5)) +
                theme(axis.text.y = element_blank())

# LIFE Analysis
        LIFE <- read_excel("ERDF & LIFE Extracted Datasets 1.0.xlsx", sheet = "2.2 LIFE Dataset")
        table(LIFE['Is the project’s name or grant code found on Google?'])
        # No = 5
        # Yes = 104
        
        table(LIFE$'What type of information is available online?', useNA = "ifany")
        # In-Depth = 94
        # Short Summary = 9
        # Unknown - Offline = 4 
        # No Data Available = 2
        
        table(LIFE$'Is the project’s name or grant code cited on Google Scholar?', useNA = "ifany")
        # No = 42
        # Yes = 67
        
        table(LIFE$'Is there any technical documentation associated with the project’s name or grant code available online?', useNA = "ifany")
        # No = 12
        # Yes = 97
        
        table(LIFE$'Based on the data available in technical documentation online, did any form of evaluation occur?', useNA = "ifany")
        # No = 7
        # Yes = 67
        # Ongoing = 32
        # No Data Available = 3
        
        LIFE_Question <-c("Is the project’s name or grant code found on Google?","Is the project’s name or grant code found on Google?",
                          "Is the project’s name or grant code cited on Google Scholar?","Is the project’s name or grant code cited on Google Scholar?",
                          "What type of information is available online?","What type of information is available online?","What type of information is available online?","What type of information is available online?",
                          "Is there any technical documentation associated with the project’s name or grant code available online?","Is there any technical documentation associated with the project’s name or grant code available online?",
                          "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?", "Based on the data available in technical documentation online, did any form of evaluation occur?")
        
        LIFE_Classification <- c("Yes","No",
                                 "Yes","No",
                                 "In-Depth","Short Summary","Unknown - Offline","No Data Available",
                                 "Yes","No",
                                 "Yes","No","Ongoing","No Data Available")
        
        LIFE_Count <- c(104,5,
                        67,42,
                        94,9,4,2,
                        97,12,
                        65,10,31,3)
        
        LIFE_Frequency <- c(104/109,5/109,
                            67/109,42/109,
                            94/109,9/109,1/109,4/109,
                            97/109,12/109,
                            67/109,7/109,32/109,3/109)
        
        LIFE_Statistics <- data.frame(LIFE_Question, LIFE_Classification, LIFE_Count, LIFE_Frequency) 
        LIFE_Statistics$LIFE_Frequency <- round(LIFE_Statistics$LIFE_Frequency, 2)
        LIFE_Statistics$GraphOrder = factor(LIFE_Statistics$LIFE_Classification, levels=c("Yes","No","Ongoing","In-Depth",
                                                                                          "Short Summary","Unknown - Offline", "No Data Available"))
        
        LIFE_Statistics$QuestionOrder = factor(LIFE_Statistics$LIFE_Question, levels=c("Based on the data available in technical documentation online, did any form of evaluation occur?",
                                                                                       "Is there any technical documentation associated with the project’s name or grant code available online?",
                                                                                       "What type of information is available online?",
                                                                                       "Is the project’s name or grant code cited on Google Scholar?",
                                                                                       "Is the project’s name or grant code found on Google?"))
        LIFE_Review_Fig <-LIFE_Statistics %>%
                ggplot(aes(fill=GraphOrder, x = QuestionOrder, y = LIFE_Frequency)) + 
                geom_bar(position = position_stack(reverse = TRUE), stat="identity", alpha=0.7) + 
                scale_y_continuous(labels = scales::percent) + 
                scale_fill_manual(values = c("#1A5D1A","#D55E00","#E69F00", "#052955","#A7B8F8","#72797a", "#000000")) +
                coord_flip() +
                theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+ 
                theme_bw() +
                theme(legend.position="bottom") +
                theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())+
                theme(legend.position = "none")+
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()) +
                ggtitle("LIFE") +
                theme(plot.title = element_text(hjust = 0.5),
                      plot.margin = margin(5.5, 0, 5.5, 5.5))
# Merged
# Exported as 1000px x 600px.  Legend and side annotation was added in photoshop.
Figure_3 <- ERDF_Review_Fig + LIFE_Review_Fig + plot_annotation(tag_levels = 'A')

