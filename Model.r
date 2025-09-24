library(dplyr)

envs <- read.csv("env.csv")

envs <- envs[order(envs$Trap),]

unique(envs$Trap)

length(unique(envs$Trap))
#27

length(unique(envs$Date))

# SQM
plot(NULL, xlim = c(min(envs$After, na.rm=TRUE)-1, max(envs$After, na.rm=TRUE)+1),
ylim = c(min(envs$SQM, na.rm=TRUE)-1, max(envs$SQM, na.rm=TRUE)+1))

for (i in 1:length(unique(envs$Trap))){
  
  for (j in 1:length(unique(envs$Date))) {
    
    points(envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$After, 
         envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$SQM, 
         pch=envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Route+14,
         col=c("orange","red","darkred")[envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Site]
         )
    
    
  }
  
  
  
}

# Temperature
plot(NULL, xlim = c(min(envs$After, na.rm=TRUE)-1, max(envs$After, na.rm=TRUE)+1),
     ylim = c(min(envs$Temperature, na.rm=TRUE)-1, max(envs$Temperature, na.rm=TRUE)+1))

for (i in 1:length(unique(envs$Trap))){
  
  for (j in 1:length(unique(envs$Date))) {
    
    points(envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$After, 
           envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Temperature, 
           pch=envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}


# Relative_Humidity
plot(NULL, xlim = c(min(envs$After, na.rm=TRUE)-1, max(envs$After, na.rm=TRUE)+1),
     ylim = c(min(envs$Relative_Humidity, na.rm=TRUE)-1, max(envs$Relative_Humidity, na.rm=TRUE)+1))

for (i in 1:length(unique(envs$Trap))){
  
  for (j in 1:length(unique(envs$Date))) {
    
    points(envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$After, 
           envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Relative_Humidity, 
           pch=envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[envs[envs$Trap==unique(envs$Trap)[i] & envs$Date==unique(envs$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}


# Insect species count
rs <- read.csv("Renia_salusalis.csv")

rs <- rs %>% 
  add_count(Trap.ID, name="count")

rs_count <- distinct(rs, Trap.ID, .keep_all = TRUE)

# Count by DOY
plot(NULL, xlim = c(min(rs_count$DOY, na.rm=TRUE)-1, max(rs_count$DOY, na.rm=TRUE)+1),
     ylim = c(min(rs_count$count, na.rm=TRUE)-1, max(rs_count$count, na.rm=TRUE)+1))

for (i in 1:length(unique(rs_count$Trap))){
  
  for (j in 1:length(unique(rs_count$Date))) {
    
    points(rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$DOY, 
           rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$count, 
           pch=rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}

# Count by Time
plot(NULL, xlim = c(min(rs_count$After.sunset, na.rm=TRUE)-1, max(rs_count$After.sunset, na.rm=TRUE)+1),
     ylim = c(min(rs_count$count, na.rm=TRUE)-1, max(rs_count$count, na.rm=TRUE)+1))

for (i in 1:length(unique(rs_count$Trap))){
  
  for (j in 1:length(unique(rs_count$Date))) {
    
    points(rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$After.sunset, 
           rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$count, 
           pch=rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[rs_count[rs_count$Trap==unique(rs_count$Trap)[i] & rs_count$Date==unique(rs_count$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}



as <- read.csv("Acleris_semipurpurana.csv")

as <- as %>% 
  add_count(Trap.ID, name="count")

as_count <- distinct(as, Trap.ID, .keep_all = TRUE)

# Count by DOY
plot(NULL, xlim = c(min(as_count$DOY, na.rm=TRUE)-1, max(as_count$DOY, na.rm=TRUE)+1),
     ylim = c(min(as_count$count, na.rm=TRUE)-1, max(as_count$count, na.rm=TRUE)+1))

for (i in 1:length(unique(as_count$Trap))){
  
  for (j in 1:length(unique(as_count$Date))) {
    
    points(as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$DOY, 
           as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$count, 
           pch=as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}

# Count by Time
plot(NULL, xlim = c(min(as_count$After.sunset, na.rm=TRUE)-1, max(as_count$After.sunset, na.rm=TRUE)+1),
     ylim = c(min(as_count$count, na.rm=TRUE)-1, max(as_count$count, na.rm=TRUE)+1))

for (i in 1:length(unique(as_count$Trap))){
  
  for (j in 1:length(unique(as_count$Date))) {
    
    points(as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$After.sunset, 
           as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$count, 
           pch=as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$Route+14,
           col=c("orange","red","darkred")[as_count[as_count$Trap==unique(as_count$Trap)[i] & as_count$Date==unique(as_count$Date)[j],]$Site]
    )
    
    
  }
  
  
  
}


# Insect summary
data <- matrix(c(1140, 1473, 1511, 2013, 1897, 1558, 1907, 1268, 1313) , nrow=3)
rownames(data) <- c("1","2","3")
colnames(data) <- c("S","W","Z")

barplot(data, 
        col=c("orange","red","darkred") , 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="group", 
        font.lab=2)



# Moth summary
data <- matrix(c(23, 26, 18, 13, 27, 20, 12, 32, 31) , nrow=3)
rownames(data) <- c("1","2","3")
colnames(data) <- c("S","W","Z")

barplot(data, 
        col=c("orange","red","darkred") , 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="group", 
        font.lab=2)
