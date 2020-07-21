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
rate_data <- read_csv("../Data_files/Rate_of_neo_milton.csv")
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


cols <- c("Milton English" = "#000000", "Contemp. English" = "#D55E00", "Milton Latin" = "#56B4E9", 
          "absent" = "#009E73", "x" = "#009E73", "Poetry" = "#0072B2", "present" = "#E69F00", "Contemp. Latin" = "#E69F00", "a" = "#CC79A7", "properName" = "#999999", 
          "Prose" = "#910a60", "arch-" = "#4ca3dd", "un-" = "#009E73", "-en" = "#0072B2", "-ly" = "#E69F00", "self-" = "#910a60", "Contemp. Prose" ="#E69F00", "Milton Prose" = "#4ca3dd", 
          "Faerie Queene" = "#910a60", "Milton Poetry" = "#0072B2" , "Paradise Lost" = "#009E73")

##### MILTON NEOS BY GENRE RAW FREQ #####
genre_neos <- milton_data %>%
  group_by(genre) %>%
  summarise(total = n())

milton_data <- filter(milton_data, genre != 'None') #remove None from data
pdf("../Plots/Genre/raw_freq_neos_genre.pdf")
a <- ggplot(milton_data, aes(x = genre)) +
  geom_bar(aes(fill = genre)) +  scale_fill_manual(values=cols)
a + labs(x = "Genre", y = "Raw Frequency") + theme(legend.position = "none")
dev.off()

#count neos for each text with genre info
text_genre_neos <- milton_data %>%
  group_by(text, genre) %>%
  summarise(counts = n())

text_genre_neos <- filter(text_genre_neos, genre != 'None') #remove None from data
poetry_neos <- filter(text_genre_neos, genre == 'Poetry')
prose_neos <- filter(text_genre_neos, genre == 'Prose')

c <- ggplot(text_genre_neos, aes(x = genre, y = counts)) + 
  geom_boxplot() + coord_flip(ylim = c(0, 160))
c <- c + labs(x = "Genre", y = "Raw Frequency") + theme(legend.position = "none")

d <- ggplot(poetry_neos, aes(x = counts, fill = genre))+
  geom_density() +  scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.1), xlim = c(0, 160))
d <- d + labs(x = "Raw Frequency", y = "Frequency Density") + theme(legend.position = "none")
e <- ggplot(prose_neos, aes(x = counts, fill = genre)) +
  geom_density() +  scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.1), xlim = c(0, 160))
e <- e + labs(x = "Raw Frequency", y = "Frequency Density")+ theme(legend.position = "none")

pdf("../Plots/Genre/raw_freq_neos_boxplot_genre.pdf")
genre_density <- ggarrange(c, e, d, 
                          labels = c("A", "B", "C"),
                          nrow = 3)
annotate_figure(genre_density)
dev.off()

poetry_mean <- filter(text_genre_neos, genre == "Poetry")
poetry_mean_stats <- summary(poetry_mean$counts)
poetry_mean_stats

prose_mean <- filter(text_genre_neos, genre == "Prose")
prose_mean_stats <- summary(prose_mean$counts)
prose_mean_stats

#Uneven distribution of neos taken from poetry compared to prose - prose has a large spread taken from each text (IQR), whereas poetry has a few outlying texts contributing the majority of the poetry neologisms (Paradise Lost and Comus)

##### WF & POS #########
pos_genre_all <- milton_data %>%
  group_by(genre, PoS) %>%
  tally()

pos_genre_all <- na.omit(pos_genre_all)
pos_genre_all <- merge(pos_genre_all, genre_neos, by = 'genre')
pos_genre_all['prop'] <- pos_genre_all['n']/pos_genre_all['total']

pdf("../Plots/Genre/genre_pos.pdf")
Pos_genre <- ggplot(pos_genre_all) +
  geom_bar(aes(PoS, n, fill = genre), stat = "identity", position = "dodge") + scale_fill_manual(values=cols)
Pos_genre + labs(y = "Proportional Frequency", x = "Part of Speech") + theme(legend.title =  element_blank()) 
dev.off()

milton_data$WF <- revalue(milton_data$WF, c("variant" = "Other", "properNameHybrid" = "Other", "blend" = "Other", "shortening" = "Other", "uncertain" = "Other", "properName" = "Other","initialism" = "Other", "imitative" = "Other", "unknown" = "Other","None" = "Other","backformation"= "Other","inherited"= "Other","arbitrary" = "Other"))

genre_wf_all <- milton_data %>%
  group_by(genre, WF) %>%
  tally()
genre_wf_all <- na.omit(genre_wf_all)
genre_wf_all <- merge(genre_wf_all, genre_neos, by = 'genre')
genre_wf_all['prop'] <- genre_wf_all['n']/genre_wf_all['total']

pdf("../Plots/Genre/genre_WF.pdf")
WF_genre <- ggplot(genre_wf_all) +
  geom_bar(aes(WF, prop, fill = genre), stat = 'identity', position = 'dodge') + scale_fill_manual(values=cols)
WF_genre + labs(y = "Proportional Frequency", x = "Word Formation") + theme(axis.text.x = element_text(angle = 90, hjust = 0), legend.title =  element_blank()) 
dev.off()