#SWC Intermediate R Exercises

#Using assignment operators 

x<-5
x
x=8
x

surveys<-read.csv(file="surveys.csv")

inflam2<-read.csv(file="inflammation-02.csv", header=FALSE)
avg_day_inflam<-apply(inflam2,MARGIN=2, mean)
plot(avg_day_inflam)

max_day_inflam<-apply(inflam2,MARGIN=2, max)
plot(max_day_inflam)

min_day_inflam<-apply(inflam2,MARGIN=2, min)
plot(min_day_inflam)

sd_day_inflam<-apply(inflam2,MARGIN=2, sd)
plot(sd_day_inflam)


fahr_to_kelvin<-function(temp){
  kelvin<-((temp-32)*(5/9))+273.15
  return(kelvin)
}
fahr_to_kelvin(7)

kelvin_to_celsius<-function(temp){
  celsius<-(temp-273.15)
  return(celsius)
}

fahr_to_celsius<-function(temp){
  temp_k<-fahr_to_kelvin(temp)
  result<-kelvin_to_celsius(temp_k)
  return(result)

}

best_practice<-c("Write","programs","for","people","not","computers")
asterisk<-"***"

fence<-function(original,wrapper){
  result<-c(wrapper,original,wrapper)
  return(result)
}
fence(best_practice,asterisk)
star<-"*"




#Creating analyze function to complete analysis of inflammation data with one command

analyze<-function(filename){
  
inflam2<-read.csv(file=filename, header=FALSE)
avg_day_inflam<-apply(inflam2,MARGIN=2, mean)
plot(avg_day_inflam)
max_day_inflam<-apply(inflam2,MARGIN=2, max)
plot(max_day_inflam)
min_day_inflam<-apply(inflam2,MARGIN=2, min)
plot(min_day_inflam)

}
analyze("inflammation-03.csv")


#Looping

length(best_practice)
len<-0
for(v in best_practice){
 len<-len+1
}

#Loop to sum a vector
values<-c(1,2,3)
mysum<-function(vec){
  total=0
  for(v in vec){
    total=total+v
  }
  return(total)
  }

mysum(values)


#Creating a loop to batch analyze inflammation data

list.files(pattern="csv")
list.files(pattern="^inflammation.+csv$")
filenames<-list.files(pattern="inflammation")

for(f in filenames){
  print(f)
  analyze(f)
}

analyze_all<-function(datapattern){
  filenames<-list.files(pattern=datapattern)
  for(f in filenames){
    print(f)
    analyze(f)
  }
    
  }
analyze_all("inflam")
