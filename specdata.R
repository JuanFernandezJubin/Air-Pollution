##pollutantmean <- function(directory,pollutant,id= 1:332){
      
    ##monitors <- sprintf("%03d",id);finalmeans <- 0;divisor <-0
    ##directory1 <- paste("C:/Users/Usuario/Desktop/Coursera/R Programming/TPS/Programming Assignment 1/",directory,"/",monitors,".csv",sep = "")
    ##print(monitors)
    ##print(directory1)
  
    ##file <- read.csv(directory1, head = TRUE,sep = ",")
    ##for (i in seq_along(directory1)) {
      #if (pollutant == "sulfate"){
       # divisor <- divisor +1
        #file <- read.csv(directory1[i], head = TRUE,sep = ",")
        #print(directory1[i])
        #means <-mean (file$sulfate, na.rm = TRUE)
        #print(means)
        #finalmeans<- finalmeans + means
      #}else if (pollutant == "nitrate"){
       # divisor <- divisor +1
        #file <- read.csv(directory1[i], head = TRUE,sep = ",")
        #print(directory1[i])
        #means <-mean(file$nitrate, na.rm = TRUE)
        #print(means)
        #finalmeans<- finalmeans + means
      #}
    #}
    #print(divisor)
#  return(finalmeans/divisor)
#}
pollutantmean <-function(directory, pollutant ,id = 1:332){
  file_list <- list.files(directory,full.names= TRUE);valores<-numeric()
  
  for(i in id){
    data <- read.csv(file_list[i])
    valores <- c(valores,data[[pollutant]])
  }
return(mean(valores,na.rm = TRUE))
}



complete<- function(directory,id=1:332){

  file_list <- list.files(directory,full.names = TRUE)
  df1 <- data.frame()
  
 for (i in id) {
   monitor <- read.csv(file_list[i])
   nobs <- sum(complete.cases(monitor))
   temp <- data.frame(i,nobs)
   df1 <-rbind(df1,temp)
 }
  names(df1)<- c("id","nobs")
  View(df1)
  return(df1[,"nobs"])
}

corr<- function(directory,threshold=0){
  file_list <- list.files(directory,full.names = TRUE);vec<-vector(mode = "numeric",length = 0)
  comp<-0;data2<-data.frame();
  View(file_list)
  
  for(i in 1:length(file_list)){
    monitor <-read.csv(file_list[i])
    comp<- sum(complete.cases(monitor[["nitrate"]]) & complete.cases(monitor[["sulfate"]]))
    #print(comp)
    #comp<-sum(monitor[["sulfate"]],monitor[["nitrate"]])
    if (comp > threshold){
      data2<-na.omit(monitor)
      corre <- cor(data2[["nitrate"]],data2[["sulfate"]])
      if(!is.na(corre)){
        vec<-c(vec,corre)
        #print(vec)
      }#else if(is.na(corre)){
        #vec <-c(vec,0)
      #}
    }
  }
return(vec)
}