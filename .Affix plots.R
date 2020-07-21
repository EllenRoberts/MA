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
require(gridExtra)
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

milton_affix <- affix_data[, c('Word_ID', 'affix', 'affixation')]
milton_affix <- merge(milton_data, milton_affix, by = 'Word_ID')

contemp_affix <- affix_data[, c('Word_ID', 'affix', 'affixation')]
contemp_affix <- merge(contemp_data, contemp_affix, by = 'Word_ID')

cols <- c("Milton English" = "#000000", "Contemp. English" = "#D55E00", "Milton Latin" = "#56B4E9", 
          "absent" = "#009E73", "x" = "#009E73", "Poetry" = "#0072B2", "present" = "#E69F00", "Contemp. Latin" = "#E69F00", "a" = "#CC79A7", "properName" = "#999999", 
          "Prose" = "#910a60", "arch-" = "#4ca3dd", "un-" = "#009E73", "-en" = "#0072B2", "-ly" = "#E69F00","self-" = "#910a60", "Milton" = "#E69F00", "Contemporaries" = "#009E73")

############### AFFIXES #######

neos_text <- milton_data %>%
  group_by(text) %>%
  tally(sort = TRUE)
#all_data <- filter(all_data, affix != '-ing')

word_IDs <- affix_data %>%
  group_by(Word_ID) %>%
  tally(sort = TRUE)

#plotting raw data
affixes_year <- affix_data %>%
  group_by(affix, year) %>%
  tally(sort = TRUE)

affixes_year <- na.omit(affixes_year)

a <- ggplot(data = affixes_year, aes(x = year, y = n)) +
  geom_point(aes(colour = affix))+  scale_colour_manual(values=cols) +
  geom_smooth(aes(colour = affix), se =FALSE) 
a <- a + labs(y = "Raw frequency", x = "Year")

#plotting averaged frequency
years <- affix_data %>%
  group_by(year) %>%
  tally(sort = TRUE)

affixes_year <- affix_data %>%
  group_by(affix, year) %>%
  tally(sort = TRUE)

affixes_year <- na.omit(affixes_year)

averages <- merge(years, affixes_year, by = 'year')
averages['average'] <- averages['n.y'] / averages['n.x']


b <- ggplot(data = averages, aes(x = year, y = average)) +
  geom_point(aes(colour = affix)) +
  geom_smooth(aes(colour = affix), method = "loess", se=FALSE) +  scale_colour_manual(values=cols)
b <- b + labs(y = "Proportional frequency", x = "Year")
#save output to pdf
pdf("../Plots/Affix/affixes_oed_frequency.pdf")
oed_affix_fig <- ggarrange(a, b,
                           labels = c("Raw", "Prop"),
                           ncol = 2, nrow = 1, common.legend = TRUE)
annotate_figure(oed_affix_fig)
dev.off()

####### Miltonic plots ############
#plotting raw data
m_neo_year <- milton_data %>%
  group_by(year) %>%
  tally(sort = TRUE)

affixes_m_year <- milton_affix %>%
  group_by(affix, year) %>%
  tally(sort = TRUE)

affixes_m_year <- na.omit(affixes_m_year)

affixes_m_year <- merge(affixes_m_year, m_neo_year, by = 'year')
affixes_m_year['average'] <- affixes_m_year['n.x']/affixes_m_year['n.y']

pdf("../Plots/Affix/affixes_m_chronological.pdf")
a <- ggplot(data = affixes_m_year, aes(x = year, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity') +  scale_fill_manual(values=cols)
a + labs(y = "Proportional frequency", x = "Year") 
#save output to pdf
dev.off()

affixes_m <- milton_affix %>%
  group_by(affix) %>%
  tally(sort = TRUE)

affixes_m <- na.omit(affixes_m)

affixes_m['average']<- affixes_m['n']/867

a <- ggplot(data = affixes_m, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity') +  scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.1))
a <- a + labs(y = "Proportional frequency", x = "Affix")

affixes_c <- contemp_affix %>%
  group_by(affix) %>%
  tally(sort = TRUE)

affixes_c <- na.omit(affixes_c)

affixes_c['average']<- affixes_c['n']/16883

b <- ggplot(data = affixes_c, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity') +  scale_fill_manual(values=cols) + coord_cartesian(ylim = c(0, 0.1))
b <- b + labs(y = "Proportional frequency", x = "Affix")
#save output to pdf
pdf("../Plots/Affix/affixes_milton_contemp_prop_freq.pdf")
m_c_affix_fig <- ggarrange(a, b, labels = c("M.", "C."), 
                           ncol = 2, nrow = 1, common.legend = TRUE)
annotate_figure(m_c_affix_fig)
dev.off()

##### Milton pos affixes

affix_pos <- milton_affix %>%
  group_by(affix, PoS) %>%
  tally(sort = TRUE)

milton_pos <- milton_affix %>%
  group_by(PoS) %>%
  tally(sort = TRUE)

affix_pos <- merge(affix_pos, milton_pos, by = 'PoS')
affix_pos['average'] <- affix_pos['n.x']/affix_pos['n.y']
affix_pos <- na.omit(affix_pos)

JJ_plot <- subset(affix_pos, PoS == 'JJ')

JJ <- ggplot(data = JJ_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
mylegend<-get_legend(JJ)
JJ <- JJ + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

NN_plot <- subset(affix_pos, PoS == 'NN')

NN <- ggplot(data = NN_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
NN <- NN + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

RB_plot <- subset(affix_pos, PoS == 'RB')

RB <- ggplot(data = RB_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.8)) + scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
RB <- RB + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

VB_plot <- subset(affix_pos, PoS == 'VB')

VB <- ggplot(data = VB_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.8)) + scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
VB <- VB + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

pdf("../Plots/Affix/affixes_m_pos.pdf")
pos_affix_fig <- ggarrange(JJ, NN, RB, VB,
                             labels = c("JJ", "NN", "RB", "VB"),
                             ncol = 2, nrow = 2,legend = "bottom", common.legend = TRUE)
annotate_figure(pos_affix_fig)
#save output to pdf
dev.off()

#####contemporary pos affixes

affix_c_pos <- contemp_affix %>%
  group_by(affix, PoS) %>%
  tally(sort = TRUE)

c_pos <- contemp_affix %>%
  group_by(PoS) %>%
  tally(sort = TRUE)

c_affix_pos <- merge(affix_c_pos, c_pos, by = 'PoS')
c_affix_pos['average'] <- c_affix_pos['n.x']/c_affix_pos['n.y']

c_affix_pos <- na.omit(c_affix_pos)

c_JJ_plot <- subset(c_affix_pos, PoS == 'JJ')

c_JJ <- ggplot(data = c_JJ_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
c_JJ <- c_JJ + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

c_NN_plot <- subset(c_affix_pos, PoS == 'NN')

c_NN <- ggplot(data = c_NN_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
c_NN <- c_NN + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

c_RB_plot <- subset(c_affix_pos, PoS == 'RB')

c_RB <- ggplot(data = c_RB_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.8)) + scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
c_RB <- c_RB + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none") 

c_VB_plot <- subset(c_affix_pos, PoS == 'VB')

c_VB <- ggplot(data = c_VB_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.8)) + scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
c_VB <- c_VB + labs(y = "Proportional frequency", x = "Affix") + theme(legend.position="none")

pdf("../Plots/Affix/affixes_c_pos.pdf")
c_pos_affix_fig <- ggarrange(c_JJ, c_NN, c_RB, c_VB, labels = c("JJ", "NN", "RB", "VB"),
                           ncol = 2, nrow = 2, legend = 'bottom', common.legend = TRUE)
annotate_figure(c_pos_affix_fig)
dev.off()

#### genre affixes

genre_affix <- milton_affix %>%
  group_by(affix, genre) %>%
  tally(sort = TRUE)

genre_m <- milton_affix %>%
  group_by(genre) %>%
  tally(sort = TRUE)

genre_affix <- merge(genre_affix, genre_m, by = 'genre')
genre_affix['average'] <- genre_affix['n.x']/genre_affix['n.y']
genre_affix <- na.omit(genre_affix)

prose_plot <- subset(genre_affix, genre == 'Prose')

prose <- ggplot(data = prose_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
prose <- prose + labs(y = "Proportional frequency", x = "Affix") + theme(text = element_text(size=15)) 

poetry_plot <- subset(genre_affix, genre == 'Poetry')

poetry <- ggplot(data = poetry_plot, aes(x = affix, y = average)) +
  geom_bar(aes(fill = affix), stat = 'identity', position = 'dodge') + coord_cartesian(ylim = c(0, 0.1))+ scale_fill_manual(values=cols) #+  geom_smooth(aes(colour = affix))
poetry <- poetry + labs(y = "Proportional frequency", x = "Affix") + theme(text = element_text(size=15)) 

pdf("../Plots/Affix/affixes_m_genre.pdf")
genre_affix_fig <- ggarrange(poetry, prose,
                           labels = c("Poetry", "Prose"),
                           ncol = 2, nrow = 1, common.legend = TRUE)
annotate_figure(genre_affix_fig)
#save output to pdf
dev.off()