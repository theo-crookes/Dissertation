library(anomaly)
library(ggplot2)

setwd("~/STOR601")

#The toy algorithm: 
set.seed(0)
DecisionTreeAlg <- function(data,TRUEtheta1,TRUEtheta2,STARTtheta1,STARTtheta2){
  capaOutput = capa.uv(data)
  x = point_anomalies(capaOutput)
  contexts = x$strength
  #contexts = data
  T = length(contexts)
  theta1 = rep(0,T+1)
  theta1[1] = STARTtheta1
  theta2 = rep(0,T+1)
  theta2[1] = STARTtheta2
  rewards= rep(0,T+1)
  truelow = c()
  truemed = c()
  truehigh = c()
  armChoice = rep(0,T)
  for(i in 1:T){
    if(contexts[i]<theta1[i]){
      armChoice[i] = "low risk"
      if(contexts[i]<TRUEtheta1){
        rewards[i] = 1
      }
      if(rewards[i] == 1){
        truelow = c(truelow,contexts[i])
        theta1[i+1]=theta1[i]
        theta2[i+1] = theta2[i]
      }
      if(rewards[i]==0){
        if(length(truelow)==0){
          theta1[i+1] = contexts[i]
          theta2[i+1] = theta2[i]
        }
        if(length(truelow)!=0){
          theta1[i+1] = contexts[i]
          theta2[i+1] = theta2[i]
        }
      }
    }
    
    if(contexts[i]>theta2[i]){
      armChoice[i] = "high risk"
      if(contexts[i]>TRUEtheta2){
        rewards[i] = 1
      }
      if(rewards[i] == 1){
        truehigh = c(truehigh,contexts[i])
        theta2[i+1]=theta2[i]
        theta1[i+1]=theta1[i]
      }
      if(rewards[i]==0){
        if(length(truehigh)==0){
          theta2[i+1] = contexts[i]
          theta1[i+1]=theta1[i]
        }
        if(length(truehigh)!=0){
          theta2[i+1] = contexts[i]
          theta1[i+1]= theta1[i]
        }
      }
    }
    
    if(contexts[i]>theta1[i] & contexts[i]<theta2[i]){
      armChoice[i] = "medium risk"
      if(contexts[i]<TRUEtheta2 & contexts[i]>TRUEtheta1){
        rewards[i] = 1
      }
      if(rewards[i] == 1){
        truemed = c(truemed,contexts[i])
        theta1[i+1] = theta1[i]
        theta2[i+1] = theta2[i]
      }
      distTheta1 = contexts[i]-theta1[i]
      distTheta2 = theta2[i]-contexts[i]
      if(rewards[i]==0){
        if(distTheta1 < distTheta2){
          if(length(truemed)==0){
            theta1[i+1] = contexts[i]
            theta2[i+1] = theta2[i]
          }
          if(length(truemed)!=0){
            theta1[i+1] = contexts[i]
            theta2[i+1] = theta2[i]
          }
        }
        if(distTheta2 < distTheta1){
          if(length(truemed)==0){
            theta1[i+1] = theta1[i]
            theta2[i+1] = contexts[i]
          }
          if(length(truemed)!=0){
            theta2[i+1] = contexts[i]
            theta1[i+1] = theta1[i]
          }
        }
      }
    }
  }
  output = list(rewards=rewards, TRUEtheta1=TRUEtheta1,TRUEtheta2=TRUEtheta2,theta1=theta1,theta2=theta2,armChoice=armChoice,truelow=truelow,truehigh=truehigh,truemed=truemed,contexts = contexts)
}

# Experimental Analysis of Toy Algorithm

set.seed(0)
n=100000
m=500
l=150
u=200
tTheta1 = 25 #true theta1
tTheta2 = 85 #true theta 2
regret100 = c()
number_detected = c()
cumulative_regret_average = runif(m,0,0)
theta1_average = runif(m)
theta2_average = runif(m,0,0)
divide_vector = runif(m,0,0)
for(i in 1:1000){
  simData = c(rnorm(n,100,1))
  anomalous_gaps=c()
  for(i in 1:m){
    anomalous_gaps = c(anomalous_gaps,sample(c(l:u),1,replace=FALSE,prob=NULL))
  }
  anomalous_values = runif(m,0,85)
  p=0
  for(k in 1:m){
    p = p+anomalous_gaps[k]
    simData[p] = anomalous_values[k]
  }
  
  trial = DecisionTreeAlg(simData,tTheta1,tTheta2,50,51)
  
  number_detected = c(number_detected,length(trial$rewards))
  
  cumulative_regret_trial = c(1:length(trial$rewards))
  count = 0
  for(j in 1:length(trial$rewards)){
    cumulative_regret_trial[j] = count + (1-trial$rewards[j]) 
    count = count + (1-trial$rewards[j])
    if(j==100){
      regret100 = c(regret100,cumulative_regret_trial[j])
    }
    divide_vector[j] = divide_vector[j]+1
    cumulative_regret_average[j] = (cumulative_regret_average[j]*(divide_vector[j]-1) + cumulative_regret_trial[j])/divide_vector[j]
    theta1_average[j] = (theta1_average[j]*(divide_vector[j]-1) + trial$theta1[j])/divide_vector[j]
    theta2_average[j] = (theta2_average[j]*(divide_vector[j]-1) + trial$theta2[j])/divide_vector[j]
  }
}

average_theta1 = theta1_average[1:m]
average_theta2 = theta2_average[1:m]

# Construct a plot of the theta estimates:

df = data.frame(x = c(1:length(average_theta1)),y1 = average_theta1, y2=average_theta2)

plotlines2 = ggplot(df, aes(x)) + xlab("Time") + ylab("Theta Estimates") + ggtitle("Average Theta Estimates over 1000 Replications of Toy Algorithm Trial")+  geom_line(aes(y = y1), color = "black") + geom_line(aes(y = y2), color = "red")  
plotlines2

# Construct a plot of the average cumulative regret:

df_regret = data.frame(x = c(1:length(cumulative_regret_average)), y1 = cumulative_regret_average)

plotlines3 = ggplot(df_regret, aes(x)) + xlab("Time") + ylab("Cumulative Regret") + ggtitle("Average Cumulative Regret over 1000 Replications of Toy Algorithm Trial")+  geom_line(aes(y = y1), color = "black") 
plotlines3

regret100 # vector containing the cumulative regret for each trial by time 100
regret_by_100 = mean(regret100) # the average cumulative regret by time 100


# Construct a plot of the input data: 

set.seed(12345)
simData = c(rnorm(n,100,1))
anomalous_gaps=c()
for(i in 1:m){
  anomalous_gaps = c(anomalous_gaps,sample(c(l:u),1,replace=FALSE,prob=NULL))
}
anomalous_values = runif(m,0,85)
p=0
for(k in 1:m){
  p = p+anomalous_gaps[k]
  simData[p] = anomalous_values[k]
}

dfSimData = data.frame(x = c(1:1000),y1 = simData[1:1000])

plotlinesSimData = ggplot(dfSimData, aes(x)) + xlab("Data Index") + ylab("Data Value") + ggtitle("Simulated Data")+  geom_point(aes(y = y1), color = "black") 
plotlinesSimData

# Clustering Diagram

set.seed(12345)
simDataX = c(rnorm(25,70,1))
simDataY = c(rnorm(25,50,1))
ColourVector1 = c(rep('High Risk',25))
simDataX = c(simDataX,rnorm(50,5,1))
simDataY = c(simDataY,rnorm(50,25,1))
ColourVector1 = c(ColourVector1, rep('Low Risk',50))
simDataX = c(simDataX,rnorm(50,40,2))
simDataY = c(simDataY,rnorm(50,40,2))
ColourVector1 = c(ColourVector1, rep('Medium Risk',50))
simDataX = c(simDataX,rnorm(50,30,0.7))
simDataY = c(simDataY,rnorm(50,44,0.7))
ColourVector1 = c(ColourVector1, rep('High Risk',50))
simDataX = c(simDataX,rnorm(50,7,0.7))
simDataY = c(simDataY,rnorm(50,55,0.7))
ColourVector1 = c(ColourVector1, rep('High Risk',50))
simDataX = c(simDataX,rnorm(10,7,1))
simDataY = c(simDataY,rnorm(10,37,1))
ColourVector1 = c(ColourVector1, rep('Medium Risk',10))
simDataX = c(simDataX,rnorm(20,60,1))
simDataY = c(simDataY,rnorm(20,30,1))
ColourVector1 = c(ColourVector1, rep('Low Risk',20))
simDataX = c(simDataX,rnorm(10,50,0.5))
simDataY = c(simDataY,rnorm(10,50,0.5))
ColourVector1 = c(ColourVector1, rep('Medium Risk',10))


df_test = data.frame(Index = simDataX,Value = simDataY,Risk = ColourVector1)

plotlines4 = ggplot(data = df_test, aes(x = Index, y = Value, colour = Risk)) + 
  geom_point(aes(col = ColourVector1)) + xlab("Dimension 1") + ylab("Dimension 2") + ggtitle("A Plot of Context Vectors Forming Clusters in 2 Dimensions") 
plotlines4

# Clustering Diagram #2

set.seed(12345)
simDataX = c(rnorm(100,15,0.3))
simDataY = c(rnorm(100,15,0.3))
ColourVector1 = c(rep('Cluster 1',100))
simDataX = c(simDataX,rnorm(100,40,3))
simDataY = c(simDataY,rnorm(100,15,3))
ColourVector1 = c(ColourVector1, rep('Cluster 2',100))
simDataX = c(simDataX,runif(1,25,25))
simDataY = c(simDataY,runif(1,15,15))
ColourVector1 = c(ColourVector1, rep('New Context',1))

df_test = data.frame(Index = simDataX,Value = simDataY,Key = ColourVector1)

plotlines5 = ggplot(data = df_test, aes(x = Index, y = Value, colour = Key)) + 
  geom_point(aes(col = ColourVector1)) + xlab("Dimension 1") + ylab("Dimension 2") + ggtitle("A Plot of Two Clusters with Different Mean and Variance and a New Context") 
plotlines5


# Construct a plot showing a collective and point anomalies:

set.seed(0)
simData = c(rnorm(250,100,1))
ColourVector1 = c(rep('Baseline Data',250))
simData = c(simData,150,rnorm(250,100,1))
ColourVector1 = c(ColourVector1,rep('Point Anomaly',1))
ColourVector1 = c(ColourVector1,rep('Baseline Data',250))
simData = c(simData,rnorm(25,120,1),rnorm(250,100,1))
ColourVector1 = c(ColourVector1,rep('Collective Anomaly',25))
ColourVector1 = c(ColourVector1,rep('Baseline Data',250))

df_test = data.frame(Index = c(1:length(simData)),Value = simData,Key = ColourVector1)
ggplot(data = df_test, aes(x = Index, y = Value, colour=Key)) + 
  geom_point(aes(col = ColourVector1), show.legend = TRUE) + xlab("Index") + ylab("Data Value") + ggtitle("A Visualisation of a Point Anomaly and a Collective Anomaly")


