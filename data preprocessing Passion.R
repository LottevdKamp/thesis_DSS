## -----------------------------------------------------------------------------
## Measuring entrepreneurial passion -------------------------------------------

## Load packages ---------------------------------------------------------------

library(dplyr)
library(ggplot2)


## First load the data: --------------------------------------------------------

df_sp1 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/Surveys /Student Survey 2 - DEiAIII1920_SM1 - CSV Choice Text.csv", sep = ",")
df_sp2 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /2018-2019/Surveys/Student Survey 2 - Startups1819 - CSV Choice Text (1).csv", sep = ",")
df_sp3 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /2019-2020/Surveys/Student Survey 2 - Startups1920 - CSV Choice Text.csv", sep = ",")


## Remove rows from students that are not pitching -----------------------------
## and change the sequence to the sequence of pitches --------------------------

df_sp1 <- df_sp1[c(8,5,9,3,4),]
df_sp2 <- df_sp2[c(24,23,15,4,9,16,19),]
df_sp3 <- df_sp3[c(23,10,26,17,24,27,41,3,31,42,11,25,8),]

## Stack them together to create 1 data file ----------------------------------- 

df_sp1 <- df_sp1 %>%
  select(starts_with("EP") | starts_with("PDP"))

df_sp2 <- df_sp2 %>%
  select(starts_with("EP") | starts_with("PDP"))

df_sp3 <- df_sp3 %>%
  select(starts_with("EP") | starts_with("PDP"))

sp_df <- rbind(df_sp1, df_sp2)
sp_df <- rbind(sp_df, df_sp3)


## Preprocess the data ---------------------------------------------------------

#first, check for classes:
sapply(sp_df, class)

#change classes to characters, to be able to change them.

sp_df$EP1_1<- as.character(sp_df$EP1_1)
sp_df$EP1_2<- as.character(sp_df$EP1_2)
sp_df$EP1_3<- as.character(sp_df$EP1_3)
sp_df$EP1_4<- as.character(sp_df$EP1_4)
sp_df$EP1_5<- as.character(sp_df$EP1_5)
sp_df$EP2_1<- as.character(sp_df$EP2_1)
sp_df$EP2_2<- as.character(sp_df$EP2_2)
sp_df$EP2_3<- as.character(sp_df$EP2_3)
sp_df$EP2_4<- as.character(sp_df$EP2_4)
sp_df$EP2_5<- as.character(sp_df$EP2_5)
sp_df$EP3_1<- as.character(sp_df$EP3_1)
sp_df$EP3_2<- as.character(sp_df$EP3_2)
sp_df$EP3_3<- as.character(sp_df$EP3_3)

sp_df$EPA1_1<- as.character(sp_df$EPA1_1)

sp_df$PDP1_1<- as.character(sp_df$PDP1_1)
sp_df$PDP1_2<- as.character(sp_df$PDP1_2)
sp_df$PDP1_3<- as.character(sp_df$PDP1_3)
sp_df$PDP1_4<- as.character(sp_df$PDP1_4)

sapply(sp_df, class)

# check for missing values: 

sum(is.na(sp_df))

# Recode values to numeric values: 

sp_df[sp_df == "Strongly disagree"] <- 1
sp_df[sp_df == "Disagree"] <- 2
sp_df[sp_df == "Neither agree nor disagree"] <- 3
sp_df[sp_df == "Agree"] <- 4
sp_df[sp_df == "Strongly agree"] <- 5

# change the data type to numbers: 

sapply(sp_df, class)

sp_df$EP1_1<- as.numeric(sp_df$EP1_1)
sp_df$EP1_2<- as.numeric(sp_df$EP1_2)
sp_df$EP1_3<- as.numeric(sp_df$EP1_3)
sp_df$EP1_4<- as.numeric(sp_df$EP1_4)
sp_df$EP1_5<- as.numeric(sp_df$EP1_5)
sp_df$EP2_1<- as.numeric(sp_df$EP2_1)
sp_df$EP2_2<- as.numeric(sp_df$EP2_2)
sp_df$EP2_3<- as.numeric(sp_df$EP2_3)
sp_df$EP2_4<- as.numeric(sp_df$EP2_4)
sp_df$EP2_5<- as.numeric(sp_df$EP2_5)
sp_df$EP3_1<- as.numeric(sp_df$EP3_1)
sp_df$EP3_2<- as.numeric(sp_df$EP3_2)
sp_df$EP3_3<- as.numeric(sp_df$EP3_3)

sp_df$EPA1_1<- as.numeric(sp_df$EPA1_1)

sp_df$PDP1_1<- as.numeric(sp_df$PDP1_1)
sp_df$PDP1_2<- as.numeric(sp_df$PDP1_2)
sp_df$PDP1_3<- as.numeric(sp_df$PDP1_3)
sp_df$PDP1_4<- as.numeric(sp_df$PDP1_4)

sapply(sp_df, class)


## Calculate the means over the rows: ------------------------------------------

sp_df$Passion_mean <- rowMeans(sp_df)

# find the number of pitches where the students is not passionate: -------------

#first add a column which indicates the number of pitch:
sp_df$num_pitch <- c(1:25)

#add a column which states if a studentis passionate or not: 

sp_df$passion_yn <- if_else(sp_df$Passion_mean > 2.5, 1, 0)

#find the pitches were passion is not shown:
which(sp_df$passion_yn == 0)

## Create a barplot to visualise the findings: ---------------------------------

#first create a factor:
sp_df$passion_yn <- as.factor(sp_df$passion_yn)


#create and save the plot 

pdf("Data visualization:  passion")  
ggplot(data =sp_df, aes(x= num_pitch, y = Passion_mean, fill= passion_yn ))+
  scale_fill_manual(values=c("#FF0000", "#006400"), name = "Passion Yes/No",
                    labels = c( "Not Passionate", "Passionate")) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label= round(Passion_mean,2)), vjust=1.6, color = "white", size=1.8)+
  xlab("Number of Pitch") + ylab("Mean score passion")+
  ggtitle("Mean score passion of student per pitch")+
  theme_classic() +
  scale_x_continuous(n.breaks=25)
dev.off()

## only 14 is a non- passionate student. 


