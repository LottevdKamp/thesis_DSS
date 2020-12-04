###FUNCTIONS: 

# Create function to subset the data , only the action units
## and frame: ------------------------------------------------------------------

Subset_dataset <- function(Pitch, AU, cutoff){
  #Subset the datasets in variables and length
  Pitch <-  Pitch %>%
    filter(frame < cutoff) %>%
    select(all_of(AU))
  return (Pitch)
}


## Create timeseries --------------------------------------------------------

ts_m <- function(Pitch){
  end = NROW(Pitch)
  return(ts(Pitch, start = 0, end = end, frequency = 1))
}

## Plot timeseries--------------------------------------------------------------

#create function for plots: 

plot_ts <- function(Student, Judge1, Judge2, Judge3){ 
  
  par(mfrow= c(4,2))
  ts.plot(Student, gpars = list(ylab= "Intensity AU", main = "Student"))
  ts.plot(Judge1, gpars = list(ylab= "Intensity AU", main = "Judge 1"))
  ts.plot(Judge2, gpars = list(ylab= "Intensity AU", main = "Judge 2"))
  ts.plot(Judge3, gpars = list(ylab= "Intensity AU", main = "Judge 3"))
  ts.plot(Student, Judge1, gpars = list(col = c("black", "red"), xlab= "time",
                                        ylab= "Intensity AU",
                                        main = "Student & Judge 1"))

  ts.plot(Student, Judge2, gpars = list(col = c("black", "red"), xlab= "time",
                                        ylab= "Intensity AU",
                                        main = "Student & Judge 2"))
  
  ts.plot(Student, Judge3, gpars = list(col = c("black", "red"), xlab= "time",
                                        ylab= "Intensity AU",
                                        main = "Student & Judge 3"))
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend =c('Student', 'Judge'), pch=16, pt.cex=3,
         cex=1.5, bty='n',
         col = c('black', 'red'))
  mtext("Legend:", at=0.2, cex=1.5)
}


## RUN CRQA (for plots)---------------------------------------------------------

## Run CRQA: 

run_crqa <- function(Student, Judge){
  return(crqa(Student, Judge, delay = 1, embed = 1, rescale = 0, radius = 0.001, 
              normalize = 0, mindiagline = 2, minvertline = 2,
              tw = 0, whiteline = FALSE,
              recpt = FALSE, side = "both", method = "crqa",
              metric = "euclidean", datatype = "categorical"))
}

### Plot CRP's -----------------------------------------------------------------
 

Plot_CRP1 <- function(RP1){
  mRP <- melt(as.matrix(RP1), varnames=c("TimeV1", "TimeV2"),
              value.name="Recurrence")
  
  #create the plots:
  ggplot(mRP, aes(x=as.numeric(TimeV1), y=as.numeric(TimeV2), 
                         fill=as.factor(Recurrence))) + 
    geom_raster() + 
    #theme(axis.line = element_blank())+
    scale_x_continuous(breaks=seq(0,maxr, by = 100)) +
    scale_y_continuous(n.breaks= 15) +
    coord_flip()+
    ggtitle("Binary Recurrence Plot") +
    scale_fill_manual(values=c("2c2f33","#FFFFFF"), 
                      breaks=c(TRUE, FALSE), name = "Recurrence yes/no",
                      labels = c("Recurrence", " No Recurrence"))+
    theme( plot.title = element_text(size = 16, face = "bold")) + 
    xlab("Timeseries Judge 1") + ylab("Timeseries Student") + 
    theme_classic() + 
    geom_abline(intercept = 0, slope = 1) + 
    geom_abline(intercept = 25, slope = 1) + 
    geom_abline(intercept = -25, slope = 1) + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(legend.background = element_rect(fill="lightgray"))
}

Plot_CRP2 <- function(RP1){
  mRP <- melt(as.matrix(RP1), varnames=c("TimeV1", "TimeV2"),
              value.name="Recurrence")
  
  #create the plots:
  ggplot(mRP, aes(x=as.numeric(TimeV1), y=as.numeric(TimeV2), 
                  fill=as.factor(Recurrence))) + 
    geom_raster() + 
    #theme(axis.line = element_blank())+
    scale_x_continuous(breaks=seq(0,maxr, by = 100)) +
    scale_y_continuous(n.breaks= 15) +
    coord_flip()+
    ggtitle("Binary Recurrence Plot") +
    scale_fill_manual(values=c("2c2f33","#FFFFFF"), 
                      breaks=c(TRUE, FALSE), name = "Recurrence yes/no",
                      labels = c("Recurrence", " No Recurrence"))+
    theme( plot.title = element_text(size = 16, face = "bold")) + 
    xlab("Timeseries Judge 2") + ylab("Timeseries Student") + 
    theme_classic() + 
    geom_abline(intercept = 0, slope = 1) + 
    geom_abline(intercept = 25, slope = 1) + 
    geom_abline(intercept = -25, slope = 1) + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(legend.background = element_rect(fill="lightgray"))
}

Plot_CRP3 <- function(RP1){
  mRP <- melt(as.matrix(RP1), varnames=c("TimeV1", "TimeV2"),
              value.name="Recurrence")
  
  #create the plots:
  ggplot(mRP, aes(x=as.numeric(TimeV1), y=as.numeric(TimeV2), 
                  fill=as.factor(Recurrence))) + 
    geom_raster() + 
    #theme(axis.line = element_blank())+
    scale_x_continuous(breaks=seq(0,maxr, by = 100)) +
    scale_y_continuous(n.breaks= 15) +
    coord_flip()+
    ggtitle("Binary Recurrence Plot") +
    scale_fill_manual(values=c("2c2f33","#FFFFFF"), 
                      breaks=c(TRUE, FALSE), name = "Recurrence yes/no",
                      labels = c("Recurrence", " No Recurrence"))+
    theme( plot.title = element_text(size = 16, face = "bold")) + 
    xlab("Timeseries Judge 3") + ylab("Timeseries Student") + 
    theme_classic() + 
    geom_abline(intercept = 0, slope = 1) + 
    geom_abline(intercept = 25, slope = 1) + 
    geom_abline(intercept = -25, slope = 1) + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(legend.background = element_rect(fill="lightgray"))
}



##CALCULATE RR over window of 25 frames: ---------------------------------------

N <- c("-25", "-24", "-23", "-22", "-21", "-20", "-19",
       "-18", "-17", "-16", "-15", "-14", "-13", "-12",
      "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4",
      "-3", "-2", "-1", "1","2","3", "4", "5", "6", "7", 
      "8", "9", "10",
       "11", "12", "13", "14", "15", "16", "17", "18",
       "19", "20", "21", "22", "23", "24", "25")

RR_Window <- function(N, profile, ts1){
  sum_rec = 0
  sum_tot = 0
  for(i in N){
    sum_rec = sum_rec + ((length(ts1)-abs(as.numeric(i)))*profile[i])
    sum_tot = sum_tot + (length(ts1)-(abs(as.numeric(i))))
  }
  return((sum_rec/sum_tot)*100)
}


#Diagnoal cross recurrence profiles and Recurrence Window: 

run_DCRP <- function(Student, Judge, N){
  drp <- drpfromts(Student, Judge, windowsize = 25,
                   radius = 0.001, delay = 1, embed = 1, rescale = 0, 
                   tw = 0, whiteline = FALSE, recpt = FALSE, side = "both",
                   method = "crqa", metric = "euclidean", datatype = "categorical")
  profile_drp <- drp$profile
  
  RR_win <- RR_Window(N, profile_drp, Student)
  return(RR_win)
}




