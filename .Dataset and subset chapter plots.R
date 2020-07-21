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

cols <- c("R. Cotgrave" = "#009E73", "R. Holme" = "#D55E00", "R. Huloet" = "#CC79A7", "Sir T. Browne" = "#999999", "Prose" = "#910a60", "P. Holland" = "#aad9f4", "H. Cockeram" = "#4ca3dd", "H. Lyte" = "#000000", "J. Florio" = "#0072B2", "J. Gerard" = "#E69F00","J. Palsgrave" = "#910a60", "T. Blount" = "#9370DB")

##-----------------PLOTS FOR DATASET AND SUBSET CHAPTER-------------------

#create variable of Authors, texts and years from entire dataset with neologism frequency
author_text <- all_data %>%
  group_by(Author, text, year) %>%
  tally(sort = TRUE)
#create graph and variable for neologism frequency in descending order, based on chosen min freq
author_desc_freq_all <- subset(author_text, n >5)
author_desc_freq_all <- author_desc_freq_all[order(author_desc_freq_all$n, decreasing=TRUE),]
#plot graph for min freq
pdf("../Plots/D_S/desc_freq_neo_all.pdf")
a <- ggplot(author_desc_freq_all, aes(x = reorder(desc(n), text), y = n)) +
  geom_bar(aes(), stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 2100))
a + labs(x = "Text", y = "Frequency") + theme(legend.position = "none", axis.text.x = element_blank())
dev.off()

#create graph and variable for neologism frequency in descending order, for top 12 texts
author_desc_freq <- subset(author_text, n >500)
author_desc_freq <- author_desc_freq[order(author_desc_freq$n, decreasing=TRUE),]
#plot graph for top 12
pdf("../Plots/D_S/desc_freq_neo_12.pdf")
b <- ggplot(author_desc_freq, aes(x = reorder(text, desc(n)), y = n)) +
  geom_bar(aes(fill = Author), stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 2100)) +  scale_fill_manual(values=cols)
b + labs(x = "Text", y = "Frequency") + theme(axis.text.x = element_text(angle = 90, vjust = 0.1))
dev.off()

#create variable of neologism frequency per year
year_freq <- all_data %>%
  group_by(year) %>%
  summarise(counts = n())
#create variable of neologism freq per text with year data
year_text_freq <- all_data %>%
  group_by(text, year) %>%
  summarise(counts = n())
#create variable of number of texts per year
text_freq <- year_text_freq %>%
  group_by(year) %>%
  summarise(counts = n())
#merge number of texts and number of neologisms per year (counts.y is neos, counts.x is texts)
year_counts <- merge(text_freq, year_freq, by = 'year')
year_counts['neo_per_text'] <- year_counts$counts.y/year_counts$counts.x
#plot a graph for number of neos over period
pdf("../Plots/D_S/raw_neo_freq.pdf")
d <- ggplot(year_counts, aes(x = year, y = counts.y)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE, aes(color = "red")) 
d + labs(x = "Year", y = "Raw Frequency") + theme(legend.position = "none")
dev.off()

#plot a graph for ratio of neologism per text over period
pdf("../Plots/D_S/ratio_neo_text.pdf")
f <- ggplot(year_counts, aes(x = year, y = neo_per_text)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE, aes(color = "red")) 
f +  labs(x = "Year", y = "Ratio of Neologism per text (Neo:Text)") + theme(legend.position = "none")
dev.off()

#remove all variables for this chapter
rm(a, author_desc_freq, author_desc_freq_all, author_text, b, d, f, text_freq, year_counts, year_freq, year_text_freq)

#-------------subset-------------#
author_meta_data <- read_csv("../Data_files/Shortlist_of_authors.csv")
milton_meta_data <- subset(author_meta_data, Author == 'J. Milton')

# Reorder following the value of another column and plot a chronological birth date graph:
pdf("../Plots/D_S/author_timeline.pdf")
author_meta_data %>%
  mutate(Author = fct_reorder(Author, Birth)) %>%
  ggplot(aes()) + 
  geom_segment(aes(x=Birth, xend=Death, y=Author, yend=Author)) +
  geom_segment(data = milton_meta_data, aes(x=Birth, xend=Death, y=Author, yend=Author, colour = 'red'), show.legend = FALSE) +
  geom_point(aes(x = Birth, y = Author)) +
  geom_point(aes(x = Death, y = Author)) +
  geom_point(data = milton_meta_data, aes(x = Birth, y = Author, colour = 'red'), show.legend = FALSE) +
  geom_point(data = milton_meta_data, aes(x = Death, y = Author, colour = 'red'), show.legend = FALSE) +
  xlab("Year")
dev.off()

#plot frequency of neos by birth date:
pdf("../Plots/D_S/author_neo_freq.pdf")
author_meta_data %>%
  mutate(Author = fct_reorder(Author, Birth)) %>%
  ggplot(aes(x = Author, y = Freq)) +
  geom_bar(aes(), stat = "identity") +
  geom_bar(data = milton_meta_data, aes(x = Author, y = Freq, fill = 'red'), stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.01))
dev.off()

#remove all variables for this chapter
rm(author_meta_data, milton_meta_data)