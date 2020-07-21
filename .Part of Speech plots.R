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

cols <- c("UH" = "#000000", "Contemp. English" = "#D55E00", "Milton Latin" = "#56B4E9", "absent" = "#009E73", "Contemporaries" = "#009E73", "Poetry" = "#0072B2", "Milton" = "#E69F00", "present" = "#E69F00", "Contemp. Latin" = "#E69F00", "a" = "#CC79A7", "properName" = "#999999", "Prose" = "#910a60", "Browne" = "#56B4E9", "arch-" = "#4ca3dd", "un-" = "#009E73", "-en" = "#0072B2", "-ly" = "#E69F00","None" = "#910a60", "UH" = "#4ca3dd", "NN" = "#009E73", "RB" = "#0072B2", "VB" = "#E69F00","JJ" = "#D55E00")

#************************POS*******************************
#calculate no. of entries for Milton and contemps
x1 = nrow(contemp_data)
x2 = nrow(milton_data)
#count POS for contemporaries
PoS_contemp <- contemp_data %>%
  group_by(PoS) %>%
  summarise(counts = n()/x1)
#count Pos for Milton (raw freq / no. of entries)
PoS_Milton <- milton_data %>%
  group_by(PoS) %>%
  summarise(counts = n()/x2)
#combine two data frames for plotting, renaming auto names to Milton and Contemporaries
all_PoS <- merge(PoS_contemp, PoS_Milton, by = 'PoS', all = TRUE)
all_PoS <- melt(all_PoS, na.rm = FALSE, value.name ="Counts")
all_PoS$variable <- revalue(all_PoS$variable, c("counts.x"="Contemporaries"))
all_PoS$variable <- revalue(all_PoS$variable, c("counts.y"="Milton"))
#plot bar chart for Pos
pdf("../Plots/POS/bar_pos.pdf")
d <- ggplot(data = subset(all_PoS, Counts > 0.01), aes(x = PoS, y = Counts)) +
  geom_bar(aes(fill = variable),
           stat = "identity", position = position_dodge())+  scale_fill_manual(values=cols)
d + labs(y = "Proportional Frequency") + theme(legend.title = element_blank())
dev.off()

pos_by_author <- contemp_data %>%
  group_by(PoS, Author) %>%
  summarise(counts = n())

pos_author <- contemp_data %>%
  group_by(Author) %>%
  summarise(counts = n())

pos_by_author_average <- merge(pos_author, pos_by_author, by = 'Author')
pos_by_author_average['average'] <- pos_by_author_average["counts.y"]/pos_by_author_average["counts.x"]
pos_by_author_average <- subset(pos_by_author_average, select = -c(counts.y, counts.x))
pos_by_author_average['contemp'] <- 'Contemporaries'

PoS_Milton['Milton'] <- 'Milton'

pdf("../Plots/POS/bar_pos_contemp_milton.pdf")
e <- ggplot() +
  geom_boxplot(data = subset(pos_by_author_average, average > 0.01), aes(x=average, y=PoS, colour = contemp) ) +
  geom_point(data = PoS_Milton, aes(x = counts, y = PoS, colour = Milton), shape=18, size = 3 ) +  scale_colour_manual(values=cols)
e + labs(y = "Part of Speech", x = "Proportional Frequency") + theme(legend.title=element_blank())
dev.off()