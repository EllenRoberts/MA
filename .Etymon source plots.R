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
browne_data <- subset(subset, Author == 'Sir T. Browne')
shakespeare_data <- subset(subset, Author == 'W. Shakespeare')
contemp_data_browne <- subset(subset, Author != 'Sir T. Browne')
all_data_no_milton <- subset(all_data, Author != 'J. Milton' & Author != 'Milton')

milton_data$genre<-with(genre_data, genre[match(milton_data$text, text)]) #add genre categories to Milton data
milton_data$sub_genre<-with(genre_data, sub_genre[match(milton_data$text, text)]) # add sub-genre categories to Milton data

cols <- c("Milton English" = "#000000", "Contemp. English" = "#D55E00", "Milton Latin" = "#56B4E9", 
          "absent" = "#009E73", "x" = "#009E73", "Poetry" = "#0072B2", "present" = "#E69F00", "Contemp. Latin" = "#E69F00", "a" = "#CC79A7", "properName" = "#999999", 
          "Prose" = "#910a60", "x" = "#4ca3dd", "Contemporaries" = "#009E73", "Milton" = "#E69F00")

cols2 <- c("present" = "#910a60", "absent" = "#0072B2")

#tidy workspace by removing variables no longer needed
rm(lang_data, data, letter_data)

#************** ETYMON PLOTTING - ABSENCE AND PRESENCE***********************
milton_data_etymon_presence_absence <- milton_data
milton_data_etymon_presence_absence2 <- subset(milton_data, text != 'Paradise Lost')
contemp_data_etymon_presence_absence <- contemp_data
browne_data_etymon_presence_absence <- browne_data
shakespeare_data_etymon_presence_absence <- shakespeare_data

milton_data_etymon_presence_absence$English <- mapvalues(milton_data_etymon_presence_absence$English, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))
milton_data_etymon_presence_absence$Latin <- mapvalues(milton_data_etymon_presence_absence$Latin, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))

milton_data_etymon_presence_absence2$English <- mapvalues(milton_data_etymon_presence_absence2$English, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))
milton_data_etymon_presence_absence2$Latin <- mapvalues(milton_data_etymon_presence_absence2$Latin, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))

contemp_data_etymon_presence_absence$English <- mapvalues(contemp_data_etymon_presence_absence$English, from = c(2, 3, 4, 5, 6, 7), to = c(1, 1, 1, 1, 1, 1))
contemp_data_etymon_presence_absence$Latin <- mapvalues(contemp_data_etymon_presence_absence$Latin, from = c(2, 3, 4, 5, 6, 7), to = c(1, 1, 1, 1, 1, 1))

browne_data_etymon_presence_absence$English <- mapvalues(browne_data_etymon_presence_absence$English, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))
browne_data_etymon_presence_absence$Latin <- mapvalues(browne_data_etymon_presence_absence$Latin, from = c(2, 3, 4, 5, 6, 7), to = c(1, 1, 1, 1, 1, 1))

shakespeare_data_etymon_presence_absence$English <- mapvalues(shakespeare_data_etymon_presence_absence$English, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))
shakespeare_data_etymon_presence_absence$Latin <- mapvalues(shakespeare_data_etymon_presence_absence$Latin, from = c(2, 3, 4, 5, 6), to = c(1, 1, 1, 1, 1))

Latin_m <- milton_data_etymon_presence_absence %>%
  group_by(Latin) %>%
  tally(sort = TRUE)

Latin_c <- contemp_data_etymon_presence_absence %>%
  group_by(Latin) %>%
  tally(sort = TRUE)

Latin_b <- browne_data_etymon_presence_absence %>%
  group_by(Latin) %>%
  tally(sort = TRUE)

Latin_s <- shakespeare_data_etymon_presence_absence %>%
  group_by(Latin) %>%
  tally(sort = TRUE)

English_m <- milton_data_etymon_presence_absence %>%
  group_by(English) %>%
  tally(sort = TRUE)

English_c <- contemp_data_etymon_presence_absence %>%
  group_by(English) %>%
  tally(sort = TRUE)

English_b <- browne_data_etymon_presence_absence %>%
  group_by(English) %>%
  tally(sort = TRUE)

English_s <- shakespeare_data_etymon_presence_absence %>%
  group_by(English) %>%
  tally(sort = TRUE)

Latin_m['prop'] <- Latin_m['n'] / (nrow(milton_data))
Latin_m['n'] <- 'Milton'
Latin_c['prop'] <- Latin_c['n'] / (nrow(contemp_data))
Latin_c['n'] <- 'Contemporaries'
Latin_b['prop'] <- Latin_b['n'] / (nrow(browne_data))
Latin_b['n'] <- 'Browne'
Latin_s['prop'] <- Latin_s['n'] / (nrow(shakespeare_data))
Latin_s['n'] <- 'Shakespeare'

all_latin <- rbind(Latin_m, Latin_c, Latin_b, Latin_s)
all_latin$Latin <- mapvalues(all_latin$Latin, from = c(0, 1), to = c("absent", "present"))
pdf("../Plots/Ety/stacked_latin_abs_pres.pdf")
Latin_p_a <- ggplot(all_latin, aes(prop, n, fill = Latin)) + scale_fill_manual(values=cols) +
  geom_bar(stat = 'identity')
Latin_p_a + labs(y = "Author", x = "Proportional Frequency") 
#save output to pdf
dev.off()

English_m['prop'] <- English_m['n'] / (nrow(milton_data))
English_m['n'] <- 'Milton'
English_c['prop'] <- English_c['n'] / (nrow(contemp_data))
English_c['n'] <- 'Contemporaries'
English_b['prop'] <- English_b['n'] / (nrow(browne_data))
English_b['n'] <- 'Browne'
English_s['prop'] <- English_s['n'] / (nrow(shakespeare_data))
English_s['n'] <- 'Shakespeare'

all_eng <- rbind(English_m, English_c, English_b, English_s)
all_eng$English <- mapvalues(all_eng$English, from = c(0, 1), to = c("absent", "present"))
pdf("../Plots/Ety/stacked_eng_abs_pres.pdf")
English_p_a <- ggplot(all_eng, aes(prop, n, fill = English)) + scale_fill_manual(values=cols2) +
  geom_bar(stat = 'identity')
English_p_a + labs(y = "Author", x = "Proportional Frequency") 
#save output to pdf
dev.off()

Latin_author_c <- contemp_data_etymon_presence_absence %>%
  group_by(Latin, Author) %>%
  tally(sort = TRUE)

Author_neos <- contemp_data_etymon_presence_absence %>%
  group_by(Author) %>%
  tally(sort = TRUE)

Latin_neo_authors <- merge(Latin_author_c, Author_neos, by = 'Author')
Latin_neo_authors['prop'] <- Latin_neo_authors['n.x']/Latin_neo_authors['n.y']
Latin_neo_authors <- subset(Latin_neo_authors, select = -c(n.x, n.y))
Latin_neo_authors['contemp'] <- 'Contemporaries'
Latin_neo_authors$Latin <- as.character(gsub(1, "present", gsub(0, "absent", Latin_neo_authors$Latin)))
Latin_m$Latin <- as.character(gsub(1, "present", gsub(0, "absent", Latin_m$Latin)))

pdf("../Plots/Ety/boxplot_latin_abs_pres.pdf")
e <- ggplot() +
  geom_boxplot(data = Latin_neo_authors, aes(x=prop, y=Latin, colour = contemp)) +
  geom_point(data = Latin_m, aes(x = prop, y = Latin, colour = n), shape=18, size = 3 ) +  scale_colour_manual(values=cols)+ coord_cartesian(xlim = c(0, 1))
e + labs(y = "Latin etymons", x = "Proportional Frequency") + theme(legend.title=element_blank(), legend.position = "bottom")
dev.off()

English_author_c <- contemp_data_etymon_presence_absence %>%
  group_by(English, Author) %>%
  tally(sort = TRUE)

English_neo_authors <- merge(English_author_c, Author_neos, by = 'Author')
English_neo_authors['prop'] <- English_neo_authors['n.x']/English_neo_authors['n.y']
English_neo_authors <- subset(English_neo_authors, select = -c(n.x, n.y))
English_neo_authors['contemp'] <- 'Contemporaries'
English_neo_authors$English <- as.character(gsub(1, "present", gsub(0, "absent", English_neo_authors$English)))
English_m$English <- as.character(gsub(1, "present", gsub(0, "absent", English_m$English)))

pdf("../Plots/Ety/boxplot_eng_abs_pres.pdf")
e <- ggplot() +
  geom_boxplot(data = English_neo_authors, aes(x=prop, y=English, colour = contemp)) +
  geom_point(data = English_m, aes(x = prop, y = English, colour = n), shape=18, size = 3 ) +  scale_colour_manual(values=cols) + coord_cartesian(xlim = c(0, 1))
e + labs(y = "English etymons", x = "Proportional Frequency") + theme(legend.title=element_blank(), legend.position = "bottom")
dev.off()

### Chronological absence and presence ###
Latin_ety_M <- milton_data_etymon_presence_absence %>%
  group_by(year, Latin) %>%
  tally(sort = TRUE)
English_ety_M <- milton_data_etymon_presence_absence %>%
  group_by(year, English) %>%
  tally(sort = TRUE)
M_year_neos <- milton_data_etymon_presence_absence %>%
  group_by(year) %>%
  tally(sort = TRUE)

Latin_ety_C <- contemp_data_etymon_presence_absence %>%
  group_by(year, Latin) %>%
  tally(sort = TRUE)
English_ety_C <- contemp_data_etymon_presence_absence %>%
  group_by(year, English) %>%
  tally(sort = TRUE)
C_year_neos <- contemp_data_etymon_presence_absence %>%
  group_by(year) %>%
  tally(sort = TRUE)

Latin_ety_b <- browne_data_etymon_presence_absence %>%
  group_by(year, Latin) %>%
  tally(sort = TRUE)
English_ety_b <- browne_data_etymon_presence_absence %>%
  group_by(year, English) %>%
  tally(sort = TRUE)
B_year_neos <- browne_data_etymon_presence_absence %>%
  group_by(year) %>%
  tally(sort = TRUE)

Latin_ety_S <- shakespeare_data_etymon_presence_absence %>%
  group_by(year, Latin) %>%
  tally(sort = TRUE)
English_ety_S <- shakespeare_data_etymon_presence_absence %>%
  group_by(year, English) %>%
  tally(sort = TRUE)
S_year_neos <- shakespeare_data_etymon_presence_absence %>%
  group_by(year) %>%
  tally(sort = TRUE)

m_latin_neos <- merge(Latin_ety_M, M_year_neos, by = "year")
m_latin_neos['proportion'] <- m_latin_neos['n.x']/m_latin_neos['n.y']
m_latin_neos$n.y <- NULL 
m_latin_neos$n.x <- NULL
m_latin_neos$Latin <- mapvalues(m_latin_neos$Latin, from = c(0, 1), to = c("absent", "present"))
milton_latin_p_a <- ggplot(m_latin_neos, aes(year, proportion, fill = Latin)) + coord_cartesian(xlim = c(1630, 1675))+ scale_fill_manual(values=cols) +
  geom_bar(stat = 'identity')
milton_latin_p_a <- milton_latin_p_a + labs(y = "Proportional Frequency", x = "Year")  

m_eng_neos <- merge(English_ety_M, M_year_neos, by = "year")
m_eng_neos['proportion'] <- m_eng_neos['n.x']/m_eng_neos['n.y']
m_eng_neos$n.y <- NULL 
m_eng_neos$n.x <- NULL
m_eng_neos$English <- mapvalues(m_eng_neos$English, from = c(0, 1), to = c("absent", "present"))
milton_eng_p_a <- ggplot(m_eng_neos, aes(year, proportion, fill = English)) + coord_cartesian(xlim = c(1630, 1675))+ scale_fill_manual(values=cols2) +
  geom_bar(stat = 'identity')
milton_eng_p_a <- milton_eng_p_a + labs(y = "Proportional Frequency", x = "Year") 

pdf("../Plots/Ety/stacked_English_Latin_Milton_abs_pres.pdf")
milton_etymon_fig <- ggarrange(milton_latin_p_a, milton_eng_p_a,
                               labels = c("L", "E"),
                               ncol = 1, nrow = 2)
annotate_figure(milton_etymon_fig)
dev.off()

c_latin_neos <- merge(Latin_ety_C, C_year_neos, by = "year")
c_latin_neos['proportion'] <- c_latin_neos['n.x']/c_latin_neos['n.y']
c_latin_neos$n.y <- NULL 
c_latin_neos$n.x <- NULL
c_latin_neos$Latin <- mapvalues(c_latin_neos$Latin, from = c(0, 1), to = c("absent", "present"))
contemp_latin_p_a <- ggplot(c_latin_neos, aes(year, proportion, fill = Latin)) + coord_cartesian(xlim = c(1630, 1675))+ scale_fill_manual(values=cols) +
  geom_bar(stat = 'identity')
contemp_latin_p_a <- contemp_latin_p_a + labs(y = "Proportional Frequency", x = "Year") 

c_eng_neos <- merge(English_ety_C, C_year_neos, by = "year")
c_eng_neos['proportion'] <- c_eng_neos['n.x']/c_eng_neos['n.y']
c_eng_neos$n.y <- NULL 
c_eng_neos$n.x <- NULL
c_eng_neos$English <- mapvalues(c_eng_neos$English, from = c(0, 1), to = c("absent", "present"))
contemp_eng_p_a <- ggplot(c_eng_neos, aes(year, proportion, fill = English))+ coord_cartesian(xlim = c(1630, 1675))+ scale_fill_manual(values=cols2) +
  geom_bar(stat = 'identity')
contemp_eng_p_a <- contemp_eng_p_a + labs(y = "Proportional Frequency", x = "Year") 

pdf("../Plots/Ety/stacked_English_Latin_contemp_abs_pres.pdf")
contemp_etymon_fig <- ggarrange(contemp_latin_p_a, contemp_eng_p_a,
                               labels = c("L", "E"),
                               ncol = 1, nrow = 2)
annotate_figure(contemp_etymon_fig)
dev.off()

b_latin_neos <- merge(Latin_ety_b, B_year_neos, by = "year")
b_latin_neos['proportion'] <- b_latin_neos['n.x']/b_latin_neos['n.y']
b_latin_neos$n.y <- NULL 
b_latin_neos$n.x <- NULL
b_latin_neos$Latin <- mapvalues(b_latin_neos$Latin, from = c(0, 1), to = c("absent", "present"))
browne_latin_p_a <- ggplot(b_latin_neos, aes(year, proportion, fill = Latin)) + scale_fill_manual(values=cols) +
  geom_bar(stat = 'identity')
browne_latin_p_a <- browne_latin_p_a + labs(y = "Proportional Frequency", x = "Year")

b_eng_neos <- merge(English_ety_b, B_year_neos, by = "year")
b_eng_neos['proportion'] <- b_eng_neos['n.x']/b_eng_neos['n.y']
b_eng_neos$n.y <- NULL 
b_eng_neos$n.x <- NULL
b_eng_neos$English <- mapvalues(b_eng_neos$English, from = c(0, 1), to = c("absent", "present"))
browne_eng_p_a <- ggplot(b_eng_neos, aes(year, proportion, fill = English)) + scale_fill_manual(values=cols2) +
  geom_bar(stat = 'identity')
browne_eng_p_a <- browne_eng_p_a + labs(y = "Proportional Frequency", x = "Year")

pdf("../Plots/Ety/stacked_English_Latin_browne_abs_pres.pdf")
browne_etymon_fig <- ggarrange(browne_latin_p_a, browne_eng_p_a,
                                labels = c("L", "E"),
                                ncol = 1, nrow = 2)
annotate_figure(browne_etymon_fig)
dev.off()

s_latin_neos <- merge(Latin_ety_S, S_year_neos, by = "year")
s_latin_neos['proportion'] <- s_latin_neos['n.x']/s_latin_neos['n.y']
s_latin_neos$n.y <- NULL 
s_latin_neos$n.x <- NULL
s_latin_neos$Latin <- mapvalues(s_latin_neos$Latin, from = c(0, 1), to = c("absent", "present"))
shakespeare_latin_p_a <- ggplot(s_latin_neos, aes(year, proportion, fill = Latin)) + scale_fill_manual(values=cols) +
  geom_bar(stat = 'identity')
shakespeare_latin_p_a + labs(y = "Proportional Frequency", x = "Year")

s_eng_neos <- merge(English_ety_S, S_year_neos, by = "year")
s_eng_neos['proportion'] <- s_eng_neos['n.x']/s_eng_neos['n.y']
s_eng_neos$n.y <- NULL 
s_eng_neos$n.x <- NULL
s_eng_neos$English <- mapvalues(s_eng_neos$English, from = c(0, 1), to = c("absent", "present"))
shakespeare_eng_p_a <- ggplot(s_eng_neos, aes(year, proportion, fill = English)) + scale_fill_manual(values=cols2) +
  geom_bar(stat = 'identity')
shakespeare_eng_p_a + labs(y = "Proportional Frequency", x = "Year") 
pdf("../Plots/Ety/stacked_English_Latin_shakes_abs_pres.pdf")
shakespeare_etymon_fig <- ggarrange(shakespeare_latin_p_a, shakespeare_eng_p_a,
                                labels = c("L", "E"),
                                ncol = 1, nrow = 2)
annotate_figure(shakespeare_etymon_fig)
dev.off()

##### genre plots for Milton ####

milton_data_genre <- milton_data_etymon_presence_absence2 %>%
  group_by(genre) %>%
  tally(sort = TRUE)
milton_data_genre <- na.omit(milton_data_genre) 

Latin_genre_M <- milton_data_etymon_presence_absence2 %>%
  group_by(genre, Latin) %>%
  tally(sort = TRUE)
Latin_genre_M$Latin <- mapvalues(Latin_genre_M$Latin, from = c(0, 1), to = c("absent", "present"))
Latin_genre_M <- na.omit(Latin_genre_M) 

Latin_genre_M <- merge(Latin_genre_M, milton_data_genre, by = 'genre')
Latin_genre_M['proportion'] <- Latin_genre_M['n.x']/Latin_genre_M['n.y']
Latin_genre <- ggplot(Latin_genre_M) +
  geom_bar(aes(genre, proportion, fill = Latin), stat = 'identity') + scale_fill_manual(values=cols)
Latin_genre <- Latin_genre + labs(y = "Proportional Frequency", x = "Genre") 

English_genre_M <- milton_data_etymon_presence_absence2 %>%
  group_by(genre, English) %>%
  tally(sort = TRUE)
English_genre_M$English <- mapvalues(English_genre_M$English, from = c(0, 1), to = c("absent", "present"))
English_genre_M <- na.omit(English_genre_M) 

English_genre_M <- merge(English_genre_M, milton_data_genre, by = 'genre')
English_genre_M['proportion'] <- English_genre_M['n.x']/English_genre_M['n.y']
English_genre <- ggplot(English_genre_M) +
  geom_bar(aes(genre, proportion, fill = English), stat = 'identity') + scale_fill_manual(values=cols2)
English_genre <- English_genre + labs(y = "Proportional Frequency", x = "Genre") 

pdf("../Plots/Ety/stacked_English_Latin_milton_genre_abs_pres.pdf")
genre_fig <- ggarrange(Latin_genre, English_genre,
                                labels = c("L", "E"),
                                ncol = 1, nrow = 2)
annotate_figure(genre_fig)
dev.off()