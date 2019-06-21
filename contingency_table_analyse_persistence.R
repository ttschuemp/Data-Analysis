###########################
### Contingency Table ###### 
###########################

rm(list = ls()) 

library(RTransProb)
library(plot3D)

xD = Data

for( i in 1:nrow(xD)){  #Rank all Returns
  xD[i,] = rank(xD[i,], na.last= "keep")
}

q = numeric(nrow(xD))

for(i in 1:nrow(xD)) {
  q[i]= ceiling(max(xD[i,1:12], na.rm = TRUE)/5) 
}

for( i in 1:nrow(xD)){  #make quantils 
  for(j in 1:ncol(xD)){
    
    
    xD[i,j]=replace(xD[i,j], xD[i,j] <= q[i], 1)
    xD[i,j]=replace(xD[i,j], xD[i,j] > q[i] && xD[i,j] <= 2*q[i] , 2)
    xD[i,j]=replace(xD[i,j], xD[i,j] > 2*q[i] && xD[i,j] <= 3*q[i], 3)
    xD[i,j]=replace(xD[i,j], xD[i,j] > 3*q[i] && xD[i,j] <= 4*q[i], 4)
    xD[i,j]=replace(xD[i,j], xD[i,j] > 4*q[i] && xD[i,j] <= 5*q[i], 5)
    
  }
  
}

rD=as.data.frame(matrix(data=NA, nrow=(nrow(xD)*ncol(xD)), ncol = 4)) #make new Df for the package

dates=rownames(xD)
e=1
for(j in 1:ncol(xD)) {
  for(i in 1:nrow(xD)){
    rD[e,1] = j
    rD[e,2] = dates[i]
    rD[e,4] = xD[i,j]
    e=e+1
  }
}

rD[,3]=rD[,4]

for(i in 1:nrow(rD)) {
  rD[i,3] = replace(rD[i,3], rD[i,3]==1, "E")
  rD[i,3] = replace(rD[i,3], rD[i,3]==2, "D")
  rD[i,3] = replace(rD[i,3], rD[i,3]==3, "C")
  rD[i,3] = replace(rD[i,3], rD[i,3]==4, "B")
  rD[i,3] = replace(rD[i,3], rD[i,3]==5, "A")
}

colnames(rD)= c("ID", "Date", "Rating", "Num_Ratings")
rD$Rating = as.factor(rD$Rating)


rD_new=na.omit(rD)

snapshots = 12
interval  = 0.5
startDate = "1998-01-30"
endDate   = "2018-12-31"
table  = TransitionProb(rD_new,startDate, endDate,'cohort', snapshots, interval)
tTable = table$transMat/100

hist3D (x = 1:5, y = 1:5, z = tTable, bty = "g", 
        col = "#0072B2", border = "black", shade = 0.8,
        ticktype = "detailed", d = 2)


