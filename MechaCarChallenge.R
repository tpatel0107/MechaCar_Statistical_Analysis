library(dplyr)
cars<-read.csv('MechaCar_mpg.csv')
regression<-lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,cars)
s<-summary(regression)
print(s)


suspension=read.csv('Suspension_Coil.csv')
summary=summarise(suspension,
                   Mean=mean(PSI),
                   Median=median(PSI),
                   Variance=sum((PSI-mean(PSI))^2)/(length(PSI)-1),
                   SD=sd(PSI), .groups='keep')
summary2=suspension%>%group_by(Manufacturing_Lot)%>%
  summarise(Mean=mean(PSI),
            Median=median(PSI),
            Variance=sum((PSI-mean(PSI))^2)/(length(PSI)-1),
            SD=sd(PSI), .groups='keep')

test=t.test((suspension$PSI),mu=mean(suspension$PSI))
print(test)

lot1=subset(suspension,Manufacturing_Lot='Lot1')
lot2=subset(suspension,Manufacturing_Lot='Lot2')
lot3=subset(suspension,Manufacturing_Lot='Lot3')  

test2=t.test((lot1$PSI),mu=mean(lot1$PSI))
test3=t.test((lot2$PSI),mu=mean(lot2$PSI))
test4=t.test((lot3$PSI),mu=mean(lot3$PSI))

