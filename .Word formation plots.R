#R SCRIPT FOR PLOTTING MA GRAPHS - FINALISED SCRIPTS BY CHAPTER
#Ellen_Roberts_2019

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

cols <- c("Milton English" = "#000000", "Contemp. English" = "#D55E00", "Milton Latin" = "#56B4E9", "absent" = "#009E73", "Contemporaries" = "#009E73", "Poetry" = "#0072B2", "Milton" = "#E69F00", "present" = "#E69F00", "Contemp. Latin" = "#E69F00", "a" = "#CC79A7", "Prose" = "#910a60", "Browne" = "#56B4E9", "arch-" = "#4ca3dd", "un-" = "#009E73", "-en" = "#0072B2", "-ly" = "#E69F00","self-" = "#910a60", "None" = "#910a60", "UH" = "#4ca3dd", "NN" = "#009E73", "RB" = "#0072B2", "VB" = "#E69F00","JJ" = "#D55E00", "borrowing" = "#D55E00", "borrowingHybrid" = "#56B4E9", "compound" = "#009E73", "conversion" = "#0072B2", "derivative" = "#E69F00", "Other" = "#CC79A7", "NN" = "#009E73", "RB" = "#0072B2", "VB" = "#E69F00","JJ" = "#D55E00", "Types" = "#009E73","Tokens" = "#E69F00")


#************************WORD FORMATION*******************************

#calculate no. of entries for Milton and contemps
x1 = nrow(contemp_data)
x2 = nrow(milton_data)
#count WF for contemporaries
WF_contemp <- contemp_data %>%
  group_by(WF) %>%
  summarise(counts = n()/x1)
#count WF for Milton
WF_Milton <- milton_data %>%
  group_by(WF) %>%
  summarise(counts = n()/x2)
#combine two data frames for plotting, renaming auto names to Milton and Contemporaries
all_WF <- merge(WF_contemp, WF_Milton, by = 'WF', all = TRUE)
all_WF <- melt(all_WF, na.rm = FALSE, value.name ="Counts")
all_WF$variable <- revalue(all_WF$variable, c("counts.x"="Contemporaries"))
all_WF$variable <- revalue(all_WF$variable, c("counts.y"="Milton"))
#plot chart for WF
pdf("../Plots/WF/bar_wf.pdf")
d <- ggplot(data = subset(all_WF, Counts > 0.02), aes(x = WF, y = Counts)) + #cut off freq at 0.2
  geom_bar(aes(fill = variable),
           stat = "identity", position = position_dodge()) +  scale_fill_manual(values=cols)
d + labs(y = "Proportional frequency") + theme(legend.title = element_blank()) 
dev.off()

wf_by_author <- contemp_data %>%
  group_by(WF, Author) %>%
  summarise(counts = n())

wf_author <- contemp_data %>%
  group_by(Author) %>%
  summarise(counts = n())

wf_by_author_average <- merge(wf_author, wf_by_author, by = 'Author')
wf_by_author_average['average'] <- wf_by_author_average["counts.y"]/wf_by_author_average["counts.x"]
wf_by_author_average <- subset(wf_by_author_average, select = -c(counts.y, counts.x))
wf_by_author_average['contemp'] <- 'Contemporaries'

WF_Milton['Milton'] <- 'Milton'

pdf("../Plots/WF/bar_wf_contemp_milton.pdf")
e <- ggplot() +
  geom_boxplot(data = subset(wf_by_author_average), aes(x=average, y=WF, colour = contemp) ) +
  geom_point(data = WF_Milton, aes(x = counts, y = WF, colour = Milton), shape=18, size = 3 ) +  scale_colour_manual(values=cols)
e + labs(y = "Word Formation", x = "Proportional Frequency") + theme(legend.title=element_blank())
dev.off()

#### BORROWING ######
m_borrowing <- subset(milton_data, WF == 'borrowing')# | WF == 'borrowingHybrid')
c_borrowing <- subset(contemp_data, WF == 'borrowing')# | WF == 'borrowingHybrid')

m_etymon_sums <- data.frame(lang = names(milton_data[,c(10:22)]), sums=colSums(milton_data[,c(10:22)]))
m_etymon_sums['total'] <- sum(m_etymon_sums$sums)
m_etymon_sums['average']<- m_etymon_sums['sums']/m_etymon_sums['total']
png("../Plots/WF/Milton_langs_all1.png", width = 6, height = 4, units = "in", res = 600)
milton_ety <- ggplot(data = m_etymon_sums, aes(x = lang, y = average)) + #cut off freq at 0.08
  geom_bar(stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 1))
milton_ety + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0)) 
dev.off()

c_etymon_sums <- data.frame(lang = names(contemp_data[,c(10:22)]), sums=colSums(contemp_data[,c(10:22)]))
c_etymon_sums['total'] <- sum(c_etymon_sums$sums)
c_etymon_sums['average']<- c_etymon_sums['sums']/c_etymon_sums['total']
png("../Plots/WF/Contemporary_langs_all1.png", width = 6, height = 4, units = "in", res = 600)
contemp_ety <- ggplot(data = c_etymon_sums, aes(x = lang, y = average)) + #cut off freq at 0.08
  geom_bar(stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 1))
contemp_ety + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0)) 
dev.off()

m_borrow_sums <- data.frame(lang = names(m_borrowing[,c(10:22)]), sums=colSums(m_borrowing[,c(10:22)]))
m_borrow_sums['total'] <- sum(m_borrow_sums$sums)
m_borrow_sums['average']<- m_borrow_sums['sums']/m_borrow_sums['total']
png("../Plots/WF/Milton_borrowing_langs1.png", width = 6, height = 4, units = "in", res = 600)
milton_bor <- ggplot(data = m_borrow_sums, aes(x = lang, y = average)) + #cut off freq at 0.08
  geom_bar(stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 1))
milton_bor + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0)) 
dev.off()

c_borrow_sums <- data.frame(lang = names(c_borrowing[,c(10:22)]), sums=colSums(c_borrowing[,c(10:22)]))
c_borrow_sums['total'] <- sum(c_borrow_sums$sums)
c_borrow_sums['average']<- c_borrow_sums['sums']/c_borrow_sums['total']
png("../Plots/WF/Contemporary_borrowing_langs1.png", width = 6, height = 4, units = "in", res = 600)
contemp_bor <- ggplot(data = c_borrow_sums, aes(x = lang, y = average)) + #cut off freq at 0.08
  geom_bar(stat = "identity", position = position_dodge()) + scale_y_continuous(limits = c(0, 1))
contemp_bor + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0)) 
dev.off()

#### WF plots #####
milton_data$WF <- revalue(milton_data$WF, c("variant" = "Other", "properNameHybrid" = "Other", "blend" = "Other", "shortening" = "Other", "uncertain" = "Other", "properName" = "Other","initialism" = "Other", "imitative" = "Other", "unknown" = "Other","None" = "Other","backformation"= "Other","inherited"= "Other","arbitrary" = "Other"))
m_etymon_WF <- milton_data %>%
  group_by(WF, English, French, Greek, Italian, Latin) %>%
  tally(sort = TRUE)

m_etymon_WF_2 <- m_etymon_WF['WF']
v <- m_etymon_WF['n']
m_etymon_WF_3 <- data.frame(mapply(`*`,m_etymon_WF[,c(2:6)],v))
m_etymon_WF_3['WF'] <- m_etymon_WF_2

m_etymon_WF <- m_etymon_WF_3 %>% 
  group_by(WF) %>% 
  summarise_all(funs(sum))

names <-  as.matrix(m_etymon_WF[,1])
df2 <- as.data.frame(as.matrix(t(m_etymon_WF[,-1])))
colnames(df2) <- names

df2['sum'] <- rowSums(df2)
v <- df2['sum']
df3 <- data.frame(mapply(`/`,df2[,c(1:6)],v))
names <- rownames(df2)
df3['Language'] <- names

df3 <- gather(df3, key = "WF", value = "prop", 1:6)
pdf("../Plots/WF/Milton_WF_langs.pdf")
milton_ety_WF <- ggplot(data = df3, aes(x = Language, y = prop)) + #cut off freq at 0.08
  geom_bar(aes( fill = WF), stat = "identity") + coord_cartesian(ylim = c(0, 1))+  scale_fill_manual(values=cols)
milton_ety_WF + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank()) 
dev.off()

contemp_data$WF <- revalue(contemp_data$WF, c("variant" = "Other", "properNameHybrid" = "Other", "blend" = "Other", "shortening" = "Other", "uncertain" = "Other", "properName" = "Other","initialism" = "Other", "imitative" = "Other", "unknown" = "Other","None" = "Other","backformation"= "Other","inherited"= "Other","arbitrary" = "Other"))

c_etymon_WF <- contemp_data %>%
  group_by(WF, English, French, Greek, Italian, Latin) %>%
  tally(sort = TRUE)

c_etymon_WF_2 <- c_etymon_WF['WF']
v <- c_etymon_WF['n']
c_etymon_WF_3 <- data.frame(mapply(`*`,c_etymon_WF[,c(2:6)],v))
c_etymon_WF_3['WF'] <- c_etymon_WF_2

c_etymon_WF <- c_etymon_WF_3 %>% 
  group_by(WF) %>% 
  summarise_all(funs(sum))

names <-  as.matrix(c_etymon_WF[,1])
c2 <- as.data.frame(as.matrix(t(c_etymon_WF[,-1])))
colnames(c2) <- names

c2['sum'] <- rowSums(c2)
v <- c2['sum']
c3 <- data.frame(mapply(`/`,c2[,c(1:6)],v))
names <- rownames(c2)
c3['Language'] <- names

c3 <- gather(c3, key = "WF", value = "prop", 1:6)
pdf("../Plots/WF/Contemporary_WF_langs.pdf")
contem_ety_WF <- ggplot(data = c3, aes(x = Language, y = prop)) + #cut off freq at 0.08
  geom_bar(aes( fill = WF), stat = "identity") + coord_cartesian(ylim = c(0, 1))+  scale_fill_manual(values=cols)
contem_ety_WF + labs(y = "Proportional frequency", x = "Language") + theme(legend.title = element_blank()) 
dev.off()

#######COMPOUNDING#######

m_compounds <- filter(milton_data, WF == 'compound')
m_comp_freq <- as.numeric(NROW(m_compounds))
c_compounds <- filter(contemp_data, WF == 'compound')
c_comp_freq <- as.numeric(NROW(c_compounds))

m_pos_genre <- milton_data %>%
  group_by(genre, PoS) %>%
  tally()
m_pos_genre <- na.omit(m_pos_genre)

m_pos_genre_comp <- m_compounds %>%
  group_by(PoS, genre) %>%
  tally(sort = TRUE)

m_pos_genre_comp <- merge(m_pos_genre, m_pos_genre_comp, by = c('PoS', 'genre'))
m_pos_genre_comp['prop'] <- m_pos_genre_comp['n.y']/m_pos_genre_comp['n.x']

pdf("../Plots/WF/compound_m_genre_pos.pdf")
compound_pos_genre <- ggplot(data = m_pos_genre_comp, aes(x = PoS, y = prop)) +
  geom_bar(aes(fill = genre), stat = "identity", position = 'dodge') +  scale_fill_manual(values=cols)
compound_pos_genre + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.title = element_blank()) 
dev.off()

text_freq <- milton_data %>%
  group_by(text) %>%
  tally()

jj_compound <- filter(m_compounds, PoS == 'JJ')
m_j_text <- jj_compound %>%
  group_by(text) %>%
  tally(sort = TRUE)

m_j_text <- merge(m_j_text, text_freq, by = 'text')
m_j_text['prop'] <- m_j_text['n.x']/m_j_text['n.y']

pdf("../Plots/WF/compound_jj_text.pdf")
compound_jj_text <- ggplot(data = m_j_text, aes(x = text, y = prop)) +
  geom_bar(stat = "identity", position = 'dodge') +  scale_fill_manual(values=cols)
compound_jj_text + labs(y = "Proportional frequency", x = "Text") + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) 
dev.off()

m_compounds <- filter(milton_data, WF == 'compound')
m_comp_freq <- as.numeric(NROW(m_compounds))
c_compounds <- filter(contemp_data, WF == 'compound')
c_comp_freq <- as.numeric(NROW(c_compounds))

m_pos_compound <- m_compounds %>%
  group_by(PoS) %>%
  summarise(counts = n())
m_pos_compound['prop'] <- m_pos_compound['counts']/m_comp_freq
m_pos_compound <- subset(m_pos_compound, select = -c(counts))
c_pos_compound <- c_compounds %>%
  group_by(PoS) %>%
  summarise(counts = n())
c_pos_compound['prop'] <- c_pos_compound['counts']/c_comp_freq
c_pos_compound <- subset(c_pos_compound, select = -c(counts))

all_pos_compound <- merge(m_pos_compound, c_pos_compound, by ='PoS', all = TRUE)
all_pos_compound <- melt(all_pos_compound, na.rm = FALSE, value.name ="Prop")
all_pos_compound$variable <- revalue(all_pos_compound$variable, c("prop.y"="Contemporaries"))
all_pos_compound$variable <- revalue(all_pos_compound$variable, c("prop.x"="Milton"))

pdf("../Plots/WF/compound_m_c.pdf")
compound_all_1 <- ggplot(data = all_pos_compound, aes(x = PoS, y = Prop)) +
  geom_bar(aes(fill = variable), stat = "identity", position = 'dodge') +  scale_fill_manual(values=cols)
compound_all_1 + labs(y = "Proportional frequency", x = "Part of Speech") + theme(legend.title = element_blank()) 
dev.off()

comp_contemp_author_pos <- c_compounds %>%
  group_by(PoS, Author) %>%
  summarise(counts = n())
comp_contemp_author <- c_compounds %>%
  group_by(Author) %>%
  tally(sort = TRUE)

comp_author_pos <- merge(comp_contemp_author_pos, comp_contemp_author, by ='Author')
comp_author_pos['prop'] <- comp_author_pos['counts']/comp_author_pos['n']
comp_author_pos <- subset(comp_author_pos, select = -c(counts, n ))
comp_author_pos['contemp'] <- 'Contemporaries'
m_pos_compound['Milton'] <- 'Milton'

pdf("../Plots/WF/bar_compounds_contemp_milton.pdf")
e <- ggplot() +
  geom_boxplot(data = comp_author_pos, aes(x=prop, y=PoS, colour = contemp) ) +
  geom_point(data = m_pos_compound, aes(x = prop, y = PoS, colour = Milton), shape=18, size = 3 ) +  scale_colour_manual(values=cols)
e + labs(y = "Part of Speech", x = "Proportional Frequency") + theme(legend.title=element_blank())
dev.off()