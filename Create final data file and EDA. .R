## Within this code file, the final datafile is created, to export as a CSV
## And use in Python for the predictive modelling. 


##Create one datafile out of all datafiles with mimicry: -----------------------
## -----------------------------------------------------------------------------

library(dplyr)
library(gtools)
library(ggplot2)
library(readxl)


## Stack all the datafiles of the pitch under each other:----------------

df1 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch1_1.csv", sep = ",")
df2 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch2_1.csv", sep = ",")
df3 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch3_1.csv", sep = ",")
df4 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch4_1.csv", sep = ",")
df5 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch5_1.csv", sep = ",")
df6<- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch6_1.csv", sep = ",")
df7<- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch7_1.csv", sep = ",")
df8 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch8_1.csv", sep = ",")
df9 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch9_1.csv", sep = ",")
df10 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch10_1.csv", sep = ",")
df11 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch11_1.csv", sep = ",")
df12 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch12_1.csv", sep = ",")
df13 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch13_1.csv", sep = ",")
df14 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch14_1.csv", sep = ",")
df15 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch15_1.csv", sep = ",")
df16 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch16_1.csv", sep = ",")
df17 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch17_1.csv", sep = ",")
df18 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch18_1.csv", sep = ",")
df19 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch19_1.csv", sep = ",")
df20 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch20_1.csv", sep = ",")
df21 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch21_1.csv", sep = ",")
df22 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch22_1.csv", sep = ",")
df23 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch23_1.csv", sep = ",")
df24 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch24_1.csv", sep = ",")
df25 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch25_1.csv", sep = ",")


my_list <- list(df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
            df21,df22,df23,df24,df25)

final <- smartbind(df1, df2)

for(i in my_list){
  final <- smartbind(final, i)
}

##Check if it worked nicely: 
head(final,12)==smartbind(df1,df2,df3,df4)

## now add columns for Pitch number and number of row in general: --------------

#first remove first column:

final <- final[-1]
## create column pitch number: 

lijst <- list()
for(i in (1:25)){
  lijst <- append(lijst,i)
  lijst <- append(lijst,i)
  lijst <- append(lijst,i)
}

lijst <- unlist(lijst)

## Create list for number of rows: 

N_row <- (1:75)

df_pitches <- data.frame(N_row)

df_pitches$N_Pitches <- lijst


## now add column to final datafile with judges: -------------------------------

final <- cbind(df_pitches, final)

## add control variables to the final datafile: --------------------------------

## First open the datafiles with the control variables : -----------------------

df_cv1 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/Surveys /Student Survey 1 - DEiAIII1920_SM1 - CSV Numeric Values.csv", sep = ",")
df_cv2 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /2018-2019/Surveys/Student Survey 1 - Startups1819 - CSV Numeric Values.csv", sep = ",")
df_cv3 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /2019-2020/Surveys/Student Survey 1 - Startups1920 - CSV Numeric Values_exclTomvMeer.csv", sep = ",")

## Remove pitches that are not included: 

df_cv1 <- df_cv1 %>%
  slice(-c(1,2,5))  

## Change the sequence to be able to stack them in the same way as the pitches:-

df_cv1 <- df_cv1[c(4,3,5,1,2),]
df_cv2 <- df_cv2[c(27,11,17,24,7,20,8),]
df_cv3 <- df_cv3[c(24,42,43,49,29,17,35,5,52,11,15,31,55),]

variable.names(df_cv1)

#create list of control variables" 

control_var <- c("EE1.3_1", "EE1.3_2", "EE1.3_3", "EE4", "EE5",
                "D1", "D2", "D3", "D4", "D10")

##Subset datasets in only control variables: 
df_cv1 <- df_cv1 %>%
  select(all_of(control_var))

df_cv2 <- df_cv2 %>%
  select(all_of(control_var))

df_cv3 <- df_cv3 %>%
  select(all_of(control_var))

## stack them all together: 

control_df <- rbind(df_cv1, df_cv2)
control_df <- rbind(control_df, df_cv3)


## Every row 3 times to be able to stack then to final ds: 

control_df_new <- control_df%>%
  slice(rep(1:n(), each = 3)) 

## Now stack them to the final dataframe: 

final <- cbind(final, control_df_new) 

## Lastly, add the dependent variable ------------------------------------------

## First load the data: 

df_d1 <-  read_excel("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/Surveys /DEiAIII_rankings.xlsx")
df_d2 <-  read_excel("~/Documents/Lotte/M-Data science/Thesis/Data /2018-2019/Surveys/Startups1819_evaluations+rankings_allinvestors (1).xlsx")
df_d3 <-  read_excel("~/Documents/Lotte/M-Data science/Thesis/Data /2019-2020/Surveys/Startups1920_1_evaluations+rankings_allinvestors.xlsx")
df_d4 <-  read_excel("~/Documents/Lotte/M-Data science/Thesis/Data /2019-2020/Surveys/Startups1920_2_evaluations+rankings_allinvestors.xlsx")

##Remove first row: 
df_d1 <- df_d1 %>%
  slice(-c(1))  

df_d2 <- df_d2 %>%
  slice(-c(1))  

df_d3 <- df_d3 %>%
  slice(-c(1))  

df_d4 <- df_d4 %>%
  slice(-c(1))  

## Remove pitches that are not included: 

df_d3 <- df_d3 %>%
  filter(pitchid != 6)

df_d4 <- df_d4 %>%
  filter(pitchid != 5)

## Stack them together: 

d_df <- rbind(df_d1, df_d2)
d_df <- rbind(d_df, df_d4)
d_df <- rbind(d_df, df_d3)

##Only dependent variable (probinv):
d_df<- d_df %>%
  select(all_of("probinv"))

#Now add them to final file: 

final <- cbind(final, d_df) 

# Remove pitch where student is not passionate: 

final <- final
final <- final %>%
  filter(N_Pitches != 14)

## A little bit of EDA of the new dataset; -------------------------------------

sum(is.na(final)) ## No missing values. 

#check wether all values are numeric: 

sapply(final, class)

##Check control variables (Data exploration, visualisation and recode column 
## names)


##EE1.3_1:

levels(final$EE1.3_1)
summary(final$EE1.3_1)

sapply(final$EE1.3_1, class)
final$EE1.3_1 <- as.character(final$EE1.3_1)
final$EE1.3_1 <- as.numeric(final$EE1.3_1)
summary(final$EE1.3_1)
boxplot(final$EE1.3_1)

##EE1.3_2

levels(final$EE1.3_2)
summary(final$EE1.3_2)


sapply(final$EE1.3_2, class)
final$EE1.3_2 <- as.character(final$EE1.3_2)
final$EE1.3_2 <- as.numeric(final$EE1.3_2)
summary(final$EE1.3_2)
boxplot(final$EE1.3_2)

##EE1.3_3

levels(final$EE1.3_3)
summary(final$EE1.3_3)

sapply(final$EE1.3_3, class)
final$EE1.3_3 <- as.character(final$EE1.3_3)
final$EE1.3_3 <- as.numeric(final$EE1.3_3)
summary(final$EE1.3_3)
boxplot(final$EE1.3_3)

## EE4

levels(final$EE4)
summary(final$EE4)

final <- final %>% mutate(EE4 = replace(
  EE4, EE4 == "2nd board of DSA Pattern and co-founded the stichting of the association ", 2))

levels(final$EE4)
summary(final$EE4)

sapply(final$EE4, class)
final$EE4<- as.character(final$EE4)
final$EE4 <- as.numeric(final$EE4)
summary(final$EE4)
boxplot(final$EE4)

## EE5

levels(final$EE5)
summary(final$EE5)

sapply(final$EE5, class)
final$EE5<- as.character(final$EE5)
final$EE5 <- as.numeric(final$EE5)
summary(final$EE5)
boxplot(final$EE5)

## D1

levels(final$D1)
summary(final$D1)

sapply(final$D1, class)
final$D1<- as.character(final$D1)
final$D1 <- as.numeric(final$D1)
summary(final$D1)
boxplot(final$D1)

## D2

levels(final$D2)
summary(final$D2)

sapply(final$D2, class)

## D3

levels(final$D3)
summary(final$D3)

final <- final %>% mutate(D3 = replace(
  D3, D3 == "11 (highschool + uni)", 11))

sapply(final$D3, class)
final$D3<- as.character(final$D3)
final$D3 <- as.numeric(final$D3)
summary(final$D3)
boxplot(final$D3)

## D4

levels(final$D4)
summary(final$D4)

sapply(final$D4, class)
final$D4<- as.character(final$D4)
final$D4 <- as.numeric(final$D4)
summary(final$D4)
boxplot(final$D4)

## D10
?boxplot

levels(final$D10)
summary(final$D10)

sapply(final$D10, class)

## Binarize the dependent variable probinv: ------------------------------------

levels(final$probinv)
summary(final$probinv)

final$probinv <- as.numeric(final$probinv)
summary(final$probinv)
boxplot(final$probinv)



sum(final$probinv >= 50)
sum(final$probinv < 50)


#recode them: 
final$probinv <- if_else(final$probinv >= 50, 1, 0)



## Change the names of the control variables: ----------------------------------

final <- final %>% 
  rename (
    ex_startup = EE1.3_1, 
    ex_nm = EE1.3_2,
    ex_np = EE1.3_3,
    ven_founded = EE4,
    years_ex = EE5,
    age = D1,
    gender = D2,
    ft_edu = D3,
    ft_uni = D4,
    native_en = D10
  )


## Some EDA over the final dataset :--------------------------------------------

#Calculate majority  baseline scores : -----------------------------------------

# First accuraccy: 

sum(final$probinv==0)
sum(final$probinv==1)

bl_acc <- sum(final$probinv==0)/ NROW(final)*100


## Now some EDA of Recurrence Rates.--------------------------------------------

# let create a table of the min, max and mean values of Recurrence per AU:
table_RR <- as.data.frame(1:3)

table_RR$AU01 <- c(min(final$RR_AU01_P), max(final$RR_AU01_P),
                   mean(final$RR_AU01_P))
table_RR$AU02 <- c(min(final$RR_AU02_P), max(final$RR_AU02_P),
                   mean(final$RR_AU02_P))
table_RR$AU04 <- c(min(final$RR_AU04_P), max(final$RR_AU04_P),
                   mean(final$RR_AU04_P))
table_RR$AU05 <- c(min(final$RR_AU05_P), max(final$RR_AU05_P),
                   mean(final$RR_AU05_P))
table_RR$AU06 <- c(min(final$RR_AU06_P), max(final$RR_AU06_P),
                   mean(final$RR_AU06_P))
table_RR$AU07 <- c(min(final$RR_AU07_P), max(final$RR_AU07_P),
                   mean(final$RR_AU07_P))
table_RR$AU09 <- c(min(final$RR_AU09_P), max(final$RR_AU09_P),
                   mean(final$RR_AU09_P))
table_RR$AU10 <- c(min(final$RR_AU10_P), max(final$RR_AU10_P),
                   mean(final$RR_AU10_P))
table_RR$AU12 <- c(min(final$RR_AU12_P), max(final$RR_AU12_P),
                   mean(final$RR_AU12_P))
table_RR$AU14 <- c(min(final$RR_AU14_P), max(final$RR_AU14_P),
                   mean(final$RR_AU14_P))
table_RR$AU15 <- c(min(final$RR_AU15_P), max(final$RR_AU15_P),
                   mean(final$RR_AU15_P))
table_RR$AU17 <- c(min(final$RR_AU17_P), max(final$RR_AU17_P),
                   mean(final$RR_AU17_P))
table_RR$AU20 <- c(min(final$RR_AU20_P), max(final$RR_AU20_P),
                   mean(final$RR_AU20_P))
table_RR$AU23 <- c(min(final$RR_AU23_P), max(final$RR_AU23_P),
                   mean(final$RR_AU23_P))
table_RR$AU25 <- c(min(final$RR_AU25_P), max(final$RR_AU25_P),
                   mean(final$RR_AU25_P))
table_RR$AU26 <- c(min(final$RR_AU26_P), max(final$RR_AU26_P),
                   mean(final$RR_AU26_P))
table_RR$AU45 <- c(min(final$RR_AU45_P), max(final$RR_AU45_P),
                   mean(final$RR_AU45_P))



## Now data visualization: -----------------------------------------------------
## First create scatterplots: -------------------------------------------------

Plot_afct <- function(RR, title){
  plot <-   ggplot(final, aes(x = RR, y = probinv)) + geom_point() + 
    ggtitle(title, "and Probability to Invest") +
    theme(legend.position="none", plot.title = element_text(size = 16, face = "bold"))
  return(plot)
}

Plot_afct(final$RR_AU01_P, "AU01")


pdf("Data visualization: Scatterplots: Recurrence Rates and probinv without 14 mean probinv" )      
par(mfrow= c(2,2))
Plot_afct(final$RR_AU01_P, "AU01")
Plot_afct(final$RR_AU02_P, "AU02")
Plot_afct(final$RR_AU04_P, "AU04")
Plot_afct(final$RR_AU05_P, "AU05")
Plot_afct(final$RR_AU06_P, "AU06")
Plot_afct(final$RR_AU07_P, "AU07")
Plot_afct(final$RR_AU09_P, "AU09")
Plot_afct(final$RR_AU10_P, "AU10")
Plot_afct(final$RR_AU12_P, "AU12")
Plot_afct(final$RR_AU14_P, "AU14")
Plot_afct(final$RR_AU15_P, "AU15")
Plot_afct(final$RR_AU17_P, "AU17")
Plot_afct(final$RR_AU20_P, "AU20")
Plot_afct(final$RR_AU23_P, "AU23")
Plot_afct(final$RR_AU26_P, "AU26")
Plot_afct(final$RR_AU45_P, "AU45")
dev.off()


## Now lets try some boxplots : ----- ----- ----- ----- ----- ----- ----- ----- 

# first make the dependent variable a factor to be able to detect differences: 

final$probinv <- as.factor(final$probinv)

# Now plot them per AU and the mean of Recurrence rates to see if the mean 
# differs in probility to invest: 

library(reshape2)
RR <- final %>%
  select(starts_with("RR")) %>%
  rename( "1" = RR_AU01_P,
          "2" = RR_AU02_P,
          "4" = RR_AU04_P,
          "5" = RR_AU05_P,
          "6" = RR_AU06_P,
          "7" = RR_AU07_P,
          "9" = RR_AU09_P,
          "10" = RR_AU10_P,
          "12" = RR_AU12_P,
          "14" = RR_AU14_P,
          "15" = RR_AU15_P,
          "17" = RR_AU17_P,
          "20" = RR_AU20_P,
          "23"= RR_AU23_P,
          "25"= RR_AU25_P,
          "26" = RR_AU26_P,
          "45" = RR_AU45_P)
RR$probinv <- final$probinv

final.m <- melt(RR, id.var = "probinv")

pdf("Data visualization: Boxplot: Percentage of Mimicry, per AU, Per class of Likelihood to invest")   
  ggplot(data= final.m, aes (x=variable, y = value))+
  geom_boxplot(aes(fill = probinv))+
  xlab("Action Unit") + ylab("Percentage of Mimicry")+
  ggtitle("Percentage of Mimicry, per AU, Per class of Likelihood to invest") +
  scale_fill_manual(values=c("#FF0000", "#006400"),
                    name = "Likelihood to Invest",
                    labels = c( "0 (Not likely to invest)", "1 (Likely to invest)"))
  dev.off()
  

## univariate outliers: 
  
boxplot(final$RR_AU01_P)
boxplot(final$RR_AU02_P)
boxplot(final$RR_AU04_P)
boxplot(final$RR_AU05_P)
boxplot(final$RR_AU06_P)
boxplot(final$RR_AU07_P)
boxplot(final$RR_AU09_P)
boxplot(final$RR_AU10_P)
boxplot(final$RR_AU12_P)
boxplot(final$RR_AU14_P)
boxplot(final$RR_AU15_P)
boxplot(final$RR_AU17_P)
boxplot(final$RR_AU20_P)
boxplot(final$RR_AU23_P)
boxplot(final$RR_AU26_P)
boxplot(final$RR_AU45_P)


## Now save the final datafile as a .csv file: ---------------------------------

write.csv(final, file = "~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/final_dataset.csv" , row.names = FALSE )
