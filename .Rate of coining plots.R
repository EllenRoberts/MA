#R SCRIPTS FOR PLOTTING MA GRAPHS
#Ellen_Roberts_2020

##-----------------IMPORT ALL REQUIRED PACKAGES-------------------
require(readr)
require(magrittr)
require(tidyr)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(forcats)
require(tibble)
require(fitdistrplus)
require(ggpubr)
#requires RStudio to be set up with Working directory at script file location - NOT A DEFAULT!
#setwd("~/Uni/MA/MA_repo/ER_R_code_plots/Scripts")
##-----------------IMPORT ALL REQUIRED DATA INTO DATAFRAMES-------------------
lang_data <- read_tsv("../Data_files/langs_clean_count.csv") #separate file for lang data
data <- read_tsv("../Data_files/all_data_for_analysis_clean.csv") #general data from OED
subset <- read_csv("../Data_files/subset_data_clean.csv") #subsetted data from OED
letter_data <- read_tsv("../Data_files/letter_data.csv") #separate file for alphabetic data
genre_data <- read_csv("../Data_files/Milton_texts_classifier.csv") #separate file for genre/sub-genre data
affix_data <- read_csv("../Data_files/affixation_all_data_clean_final.csv") #general data from OED - built on the cleaned data
#combine all data to make one csv sorted by WORD ID
affix_data <- merge(affix_data, lang_data, by = "Word_ID")
rate_data <- read_csv("../Data_files/Rate_of_neo_milton.csv") #general data from OED - built on the cleaned data
#combine all data to make one csv sorted by WORD ID
all_data <- merge(data, lang_data, by="Word_ID") 
all_data <- merge(all_data, letter_data, by = "Word_ID")
#make copy with changed variables for true/false plotting of update dates (required later)
all_data_copy <- all_data
all_data_copy$Update_dates <- revalue(all_data_copy$Update_dates, c("2000" ="True", "2001" ="True", "2002" ="True", "2003" ="True", "2004" ="True", "2005" ="True", 
                                                                    "2006" ="True", "2007" ="True", "2008" ="True", "2009" ="True", "2010" ="True", "2011" ="True", 
                                                                    "2012" ="True", "2013" ="True", "2014" ="True", "2015" ="True", "2016" ="True", "2017" ="True", 
                                                                    "2018" ="True", "None" ="False"))
milton_data <- subset(subset, Author == 'J. Milton')
contemp_data <- subset(subset, Author != 'J. Milton')
contemp_data_browne <- subset(subset, Author != 'Sir T. Browne')
all_data_no_milton <- subset(all_data, Author != 'J. Milton' & Author != 'Milton')

milton_data$genre<-with(genre_data, genre[match(milton_data$text, text)]) #add genre categories to Milton data
milton_data$sub_genre<-with(genre_data, sub_genre[match(milton_data$text, text)]) # add sub-genre categories to Milton data

#tidy workspace by removing variables no longer needed
rm(lang_data, data, letter_data)

cols <- c("backformation" = "#000000", "borrowing" = "#D55E00", "borrowingHybrid" = "#56B4E9", 
          "compound" = "#009E73", "conversion" = "#0072B2", "derivative" = "#E69F00", "None" = "#CC79A7", "properName" = "#999999", 
          "properNameHybrid" = "#910a60", "variant" = "#4ca3dd", "NN" = "#009E73", "RB" = "#0072B2", "VB" = "#E69F00","JJ" = "#D55E00", "Types" = "#009E73",
          "Tokens" = "#E69F00")

##### RATE OF NEOS DENSITY ####

colnames(rate_data)[3] <- "tokens_word"
colnames(rate_data)[4] <- "types_word"
colnames(rate_data)[7] <- "neos_text"
rate_data['TTR'] <- rate_data['types_word']/rate_data['tokens_word']

rate_data$OED_title <- factor(rate_data$OED_title, levels = rate_data$OED_title[order(rate_data$tokens_word)])
x <- order(rate_data$tokens_word)
type_neo <- ggplot(data = rate_data, aes(x = types_word, y = neos_text)) +
  geom_point() +
  geom_text(label=(data = rate_data$OED_title), nudge_y = 0.25, check_overlap = F)

rate_data['density'] <- rate_data['neos_text']/rate_data['types_word']
pdf("../Plots/RoC/rate_coining_density.pdf")
neo_density <- ggplot(data = rate_data, aes(x = OED_title, y = density)) +
  geom_bar(stat = 'identity', position = position_dodge())
neo_density + labs(y = "Proposed Density Measure", x = "Text") + theme(axis.text.x = element_text(angle = 90, hjust = 0))
dev.off()

rate_data['corns_density'] <- rate_data['neos_text']/rate_data['tokens_word']
pdf("../Plots/RoC/rate_coining_density_corns.pdf")
neo_density <- ggplot(data = rate_data) +
  geom_bar(aes(x = OED_title, y = corns_density), stat = 'identity', position = position_dodge()) 
neo_density + labs(y = "Corns' Density Measure", x = "Text") + theme(axis.text.x = element_text(angle = 90, hjust = 0))
dev.off()

##### EXPLORING COMUS

#### WORD FORMATION AND POS FOR ROC ####

comus_data <- all_data %>% filter(text == 'Comus' & Author == 'Milton')
doctrine_data <- all_data %>% filter(Author == 'Milton' & text == 'Doctr. Divorce')
nativity_data <- all_data %>% filter(Author == 'Milton' & text == "On Christ's Nativity: Hymn")
paradise_lost_data <- all_data %>% filter(Author == 'Milton' & text == 'Paradise Lost')
milton_data_2 <- all_data %>% filter(Author == 'Milton' & text != 'Paradise Lost')
milton_data <- all_data %>% filter(Author == 'Milton')

x <- nrow(milton_data)

milton_POS <- milton_data %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
milton_POS['sum'] = x
milton_POS['proportional_freq'] = milton_POS['n']/milton_POS['sum']

milton_WF <- milton_data %>%
  group_by(WF) %>%
  tally(sort = TRUE)
milton_WF['sum'] = x
milton_WF['proportional_freq'] = milton_WF['n']/milton_WF['sum']

milton_wf_pos <- milton_data %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
milton_wf_pos['sum'] = x
milton_wf_pos['proportional_freq'] = milton_wf_pos['n']/milton_wf_pos['sum']

x1 <- nrow(milton_data_2)

milton_POS2 <- milton_data_2 %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
milton_POS2['sum'] = x1
milton_POS2['proportional_freq'] = milton_POS2['n']/milton_POS2['sum']

milton_WF2 <- milton_data_2 %>%
  group_by(WF) %>%
  tally(sort = TRUE)
milton_WF2['sum'] = x1
milton_WF2['proportional_freq'] = milton_WF2['n']/milton_WF2['sum']

milton_wf_pos2 <- milton_data_2 %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
milton_wf_pos2['sum'] = x1
milton_wf_pos2['proportional_freq'] = milton_wf_pos2['n']/milton_wf_pos2['sum']

y <- nrow(comus_data)
comus_POS <- comus_data %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
comus_POS['sum'] = y
comus_POS['proportional_freq'] = comus_POS['n']/comus_POS['sum']

comus_WF <- comus_data %>%
  group_by(WF) %>%
  tally(sort = TRUE)
comus_WF['sum'] = y
comus_WF['proportional_freq'] = comus_WF['n']/comus_WF['sum']

comus_wf_pos <- comus_data %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
comus_wf_pos['sum'] = y
comus_wf_pos['proportional_freq'] = comus_wf_pos['n']/comus_wf_pos['sum']

z <- nrow(nativity_data)
nativity_POS <- nativity_data %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
nativity_POS['sum'] = z
nativity_POS['proportional_freq'] = nativity_POS['n']/nativity_POS['sum']

nativity_WF <- nativity_data %>%
  group_by(WF) %>%
  tally(sort = TRUE)
nativity_WF['sum'] = z
nativity_WF['proportional_freq'] = nativity_WF['n']/nativity_WF['sum']

nativity_wf_pos <- nativity_data %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
nativity_wf_pos['sum'] = z
nativity_wf_pos['proportional_freq'] = nativity_wf_pos['n']/nativity_wf_pos['sum']

a <- nrow(paradise_lost_data)
paradise_lost_POS <- paradise_lost_data %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
paradise_lost_POS['sum'] = a
paradise_lost_POS['proportional_freq'] = paradise_lost_POS['n']/paradise_lost_POS['sum']

paradise_lost_WF <- paradise_lost_data %>%
  group_by(WF) %>%
  tally(sort = TRUE)
paradise_lost_WF['sum'] = a
paradise_lost_WF['proportional_freq'] = paradise_lost_WF['n']/paradise_lost_WF['sum']

paradise_lost_wf_pos <- paradise_lost_data %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
paradise_lost_wf_pos['sum'] = a
paradise_lost_wf_pos['proportional_freq'] = paradise_lost_wf_pos['n']/paradise_lost_wf_pos['sum']

b <- nrow(doctrine_data)
doctrine_POS <- doctrine_data %>%
  group_by(PoS) %>%
  tally(sort = TRUE)
doctrine_POS['sum'] = b
doctrine_POS['proportional_freq'] = doctrine_POS['n']/doctrine_POS['sum']

doctrine_WF <- doctrine_data %>%
  group_by(WF) %>%
  tally(sort = TRUE)
doctrine_WF['sum'] = b
doctrine_WF['proportional_freq'] = doctrine_WF['n']/doctrine_WF['sum']

doctrine_wf_pos <- doctrine_data %>%
  group_by(PoS, WF) %>%
  tally(sort = TRUE)
doctrine_wf_pos['sum'] = b
doctrine_wf_pos['proportional_freq'] = doctrine_wf_pos['n']/doctrine_wf_pos['sum']

milton_pos_wf1 <- ggplot(data = milton_wf_pos) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
milton_pos_wf1 <- milton_pos_wf1 + labs(y = "Proportional frequency", x = "Part of Speech") 

milton_pos_wf2 <- ggplot(data = milton_wf_pos2) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
milton_pos_wf2 <- milton_pos_wf2 + labs(y = "Proportional frequency", x = "Part of Speech") 

comus_pos_wf1 <- ggplot(data = comus_wf_pos) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
comus_pos_wf1 <- comus_pos_wf1 + labs(y = "Proportional frequency", x = "Part of Speech")

doctrine_pos_wf1 <- ggplot(data = doctrine_wf_pos) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
doctrine_pos_wf1 <- doctrine_pos_wf1 + labs(y = "Proportional frequency", x = "Part of Speech")

nativity_pos_wf1 <- ggplot(data = nativity_wf_pos) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
nativity_pos_wf1 <- nativity_pos_wf1 + labs(y = "Proportional frequency", x = "Part of Speech") 

paradise_lost_pos_wf1 <- ggplot(data = paradise_lost_wf_pos) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.8)) 
paradise_lost_pos_wf1 <- paradise_lost_pos_wf1 + labs(y = "Proportional frequency", x = "Part of Speech") 

pdf("../Plots/RoC/wf_pos_ROC_milton.pdf")
pos_wf_fig <- ggarrange(milton_pos_wf1, comus_pos_wf1, doctrine_pos_wf1, nativity_pos_wf1, paradise_lost_pos_wf1, milton_pos_wf2,
          labels = c("A", "C.", "D", "N", "P", "A1"),
          ncol = 3, nrow = 2, common.legend = TRUE)
annotate_figure(pos_wf_fig)
dev.off()

#### PART OF SPEECH FOR ROC ####
milton_pos <- ggplot(data = milton_POS) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = PoS), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.9)) 
milton_pos <- milton_pos + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.position="none") 

comus_pos <- ggplot(data = comus_POS) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = PoS), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.9)) 
comus_pos <- comus_pos + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.position="none") 

doctrine_pos <- ggplot(data = doctrine_POS) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = PoS), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.9)) 
doctrine_pos <- doctrine_pos + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.position="none") 

nativity_pos <- ggplot(data = nativity_POS) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = PoS), stat = 'identity') + scale_fill_manual(values=cols)+ coord_cartesian(ylim = c(0, 0.9))
nativity_pos <- nativity_pos + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.position="none") 

paradise_lost_pos <- ggplot(data = paradise_lost_POS) +
  geom_bar(aes(x = PoS, y = proportional_freq, fill = PoS), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.9)) 
paradise_lost_pos <- paradise_lost_pos + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.position="none") 

pdf("../Plots/RoC/pos_ROC_milton.pdf")
pos_fig <- ggarrange(milton_pos, comus_pos, doctrine_pos, nativity_pos, paradise_lost_pos,
                        labels = c("A", "C.", "D", "N", "P"),
                        ncol = 3, nrow = 2)
annotate_figure(pos_fig)
dev.off()

#### WORD FORMATION FOR ROC ####
milton_wf1 <- ggplot(data = milton_WF) +
  geom_bar(aes(x = WF, y = proportional_freq, fill = WF), stat = 'identity') + scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.7))
milton_wf1 <- milton_wf1 + labs(y = "Proportional frequency", x = "Formation process") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.position="none") 

comus_wf1 <- ggplot(data = comus_WF) +
  geom_bar(aes(x = WF, y = proportional_freq, fill = WF), stat = 'identity') + coord_cartesian(ylim = c(0, 0.7)) + scale_fill_manual(values=cols)
comus_wf1 <- comus_wf1 + labs(y = "Proportional frequency", x = "Formation process") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.position="none") 

doctrine_wf1 <- ggplot(data = doctrine_WF) +
  geom_bar(aes(x = WF, y = proportional_freq, fill = WF), stat = 'identity') + coord_cartesian(ylim = c(0, 0.7)) + scale_fill_manual(values=cols)
doctrine_wf1 <- doctrine_wf1 + labs(y = "Proportional frequency", x = "Formation process") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.position="none") 

nativity_wf1 <- ggplot(data = nativity_WF) +
  geom_bar(aes(x = WF, y = proportional_freq, fill = WF), stat = 'identity') + coord_cartesian(ylim = c(0, 0.7)) + scale_fill_manual(values=cols)
nativity_wf1 <- nativity_wf1 + labs(y = "Proportional frequency", x = "Formation process") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.position="none") 

paradise_lost_wf1 <- ggplot(data = paradise_lost_WF) +
  geom_bar(aes(x = WF, y = proportional_freq, fill = WF), stat = 'identity') + coord_cartesian(ylim = c(0, 0.7)) + scale_fill_manual(values=cols)
paradise_lost_wf1 <- paradise_lost_wf1 + labs(y = "Proportional frequency", x = "Formation process") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.position="none") 
pdf("../Plots/RoC/WF_ROC_milton.pdf")
wf_fig <- ggarrange(milton_wf1, comus_wf1, doctrine_wf1, nativity_wf1, paradise_lost_wf1,
                     labels = c("A", "C.", "D", "N", "P"),
                     ncol = 3, nrow = 2)
annotate_figure(wf_fig)
dev.off()