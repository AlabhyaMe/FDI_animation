#We call this script to make a data set that has every year to all the district FDI came too
#This is important for the cumulative sum in the main script as cumulative sum miss value for the year with no FDI, or if no
#investment in a particular district
#This ensures that all year and invested district is summed

#Very Important, this script is NOT independent. Main script Industry animation has to be called before running this script

year <- c(2033:2078)
dist <- unique(NumberWise$DISTRICT)
data_set<- as.data.frame(matrix(data=NA,ncol = 3, byrow = T)) 
colnames(data_set) <- c("Year", "DISTRICT","COUNTRY") 
country <- c("INDIA", "CHINA", "Others")

r=1
for (i in 1:length(year)){
  for (j in 1:length(dist)){
    for (k in 1:3) {
    data_set[r,1] <- year[i]
    data_set[r,2] <- dist[j]
    data_set[r,3] <- country[k]
    r<-r+1
    }
  }
  
}

NumberWise$Year <- as.numeric(NumberWise$Year)
