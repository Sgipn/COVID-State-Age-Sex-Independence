
#Question 2: Test for Independence:

#Subsetting for Age Groups:
yo18.29<- CovidB[CovidB[,4] == "18-29 years",]
yo30.39<- CovidB[CovidB[,4] == "30-39 years",]
yo40.49<- CovidB[CovidB[,4] == "40-49 years",]  
yo50.64<- CovidB[CovidB[,4] == "50-64 years",]
yo65.74<- CovidB[CovidB[,4] == "65-74 years",]
yo75.84<- CovidB[CovidB[,4] == "75-84 years",]
yo85.<- CovidB[CovidB[,4] == "85 years and over",]
yo.18.85<- rbind.data.frame(yo18.29,yo30.39,yo40.49,yo50.64,yo65.74,yo75.84,yo85.)
zee.table = xtabs(Death ~ Age_Group + Sex, data = yo.18.85)
zee.table

Age_Group = rep(c("18-29 years","30-39 years", "40-49 years", "50-64 years", "65-74 years", "75-84 years","85 years and older"),times = c(1684,5028,13482,70152,103439,133552,151341))
Sex = rep(c("Female","Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Male"),
          times = c(640,1044,1736,3292,4582,8900,24893,45259,40030,63409,58426,75126,87402,63939))
data18.85 = data.frame(Age_Group,Sex)

zee.table = table(data18.85)
the.test = chisq.test(zee.table,correct = FALSE)
eij = the.test$expected
chi.sq.obs = as.numeric(the.test$statistic)


set.seed(104)
R = 3000
r.perms = sapply(1:R,function(i){
  perm.data = data18.85
  perm.data$Age_Group = sample(perm.data$Age_Group,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})

perm.pval = mean(r.perms >= chi.sq.obs)
perm.pval

set.seed(104)
R = 3000
r.perms = sapply(1:R,function(i){
  perm.data = data18.85
  perm.data$Sex = sample(perm.data$Sex,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})

perm.pval = mean(r.perms >= chi.sq.obs)
perm.pval


#P(75-84 years | Female) - P(75-84 years | Male)

n = sum(zee.table)
n
ni. = rowSums(zee.table)
ni.
n.j = colSums(zee.table)
n.j

#Fixed Row

set.seed(104)
R=2000
r.perms.cutoff = sapply(1:R,function(i){
  perm.data = data18.85
  perm.data$Age_Group = sample(perm.data$Age_Group,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(table(perm.data))
  col.sum = colSums(table(perm.data))
  all.pji1 = table(perm.data)[1,]/row.sum[1]
  all.pji2= table(perm.data)[2,]/row.sum[2]
  all.pji3= table(perm.data)[3,]/row.sum[3]
  all.pji4= table(perm.data)[4,]/row.sum[4]
  all.pji5= table(perm.data)[5,]/row.sum[5]
  all.pji6= table(perm.data)[6,]/row.sum[6]
  all.pji7= table(perm.data)[7,]/row.sum[7]
  all.pbar = col.sum/sum(row.sum)
  all.Zij = c((all.pji1 - all.pji2)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[2])),
              (all.pji1 - all.pji3)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[3])),
              (all.pji1 - all.pji4)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[4])),
              (all.pji1 - all.pji5)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[5])),
              (all.pji1 - all.pji6)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[6])),
              (all.pji1 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[7])),
              (all.pji2 - all.pji3)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[2] + 1/row.sum[3])),
              (all.pji2 - all.pji4)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[2] + 1/row.sum[4])),
              (all.pji2 - all.pji5)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[2] + 1/row.sum[5])),
              (all.pji2 - all.pji6)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[2] + 1/row.sum[6])),
              (all.pji2 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[2] + 1/row.sum[7])),
              (all.pji3 - all.pji4)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[3] + 1/row.sum[4])),
              (all.pji3 - all.pji5)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[3] + 1/row.sum[5])),
              (all.pji3 - all.pji6)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[3] + 1/row.sum[6])),
              (all.pji3 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[3] + 1/row.sum[7])),
              (all.pji4 - all.pji5)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[4] + 1/row.sum[5])),
              (all.pji4 - all.pji6)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[4] + 1/row.sum[6])),
              (all.pji4 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[4] + 1/row.sum[7])),
              (all.pji5 - all.pji6)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[5] + 1/row.sum[6])),
              (all.pji5 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[5] + 1/row.sum[7])),
              (all.pji6 - all.pji7)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[6] + 1/row.sum[7])))
  Q.r = max(abs(all.Zij))
  return(Q.r)
})
hist(r.perms.cutoff)
alpha = 0.05
cutoff.q = as.numeric(quantile(r.perms.cutoff,(1-alpha)))

all.pjG1 = zee.table[1,]/ni.[1] #18-29
all.pjG1
all.pjG2= zee.table[2,]/ni.[2] #30-39
all.pjG2
all.pjG3= zee.table[3,]/ni.[3] #40-49
all.pjG3
all.pjG4= zee.table[4,]/ni.[4] #50-64
all.pjG4
all.pjG5= zee.table[5,]/ni.[5] #65-74
all.pjG5
all.pjG6= zee.table[6,]/ni.[6] #75-84
all.pjG6
all.pjG7= zee.table[7,]/ni.[7] #85+
all.pjG7
all.pbar = n.j/n #all probabilities regardless of group
all.pbar
all.Zij1 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #(18-29) - (30-39) 
all.Zij1
all.Zij2 = c(all.pjG1 - all.pjG3)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[3])) #(18-29) - (40-49)
all.Zij2
all.Zij3 = c(all.pjG1 - all.pjG4)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[4])) #(18-29) - (50-64)
all.Zij3
all.Zij4 = c(all.pjG1 - all.pjG5)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[5])) #(18-29) - (65-74)
all.Zij4
all.Zij5 = c(all.pjG1 - all.pjG6)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[6])) #(18-29) - (75-84)
all.Zij5
all.Zij6 = c(all.pjG1 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[7])) #(18-29) - (85+)
all.Zij6
all.Zij7 = c(all.pjG2 - all.pjG3)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[2] + 1/ni.[3])) #(30-39) - (40-49)
all.Zij7
all.Zij8 = c(all.pjG2 - all.pjG4)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[2] + 1/ni.[4])) #(30-39) - (50-64)
all.Zij8
all.Zij9 = c(all.pjG2 - all.pjG5)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[2] + 1/ni.[5])) #(30-39) - (65-74)
all.Zij9
all.Zij10 = c(all.pjG2 - all.pjG6)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[2] + 1/ni.[6])) #(30-39) - (75-84)
all.Zij10
all.Zij11= c(all.pjG2 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[2] + 1/ni.[7])) #(30-39) - (85+)
all.Zij11
all.Zij12= c(all.pjG3 - all.pjG4)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[3] + 1/ni.[4])) #(40-49) - (50-64)
all.Zij12
all.Zij13= c(all.pjG3 - all.pjG5)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[3] + 1/ni.[5])) #(40-49) - (65-74)
all.Zij13
all.Zij14= c(all.pjG3 - all.pjG6)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[3] + 1/ni.[6])) #(40-49) - (75-84)
all.Zij14
all.Zij15= c(all.pjG3 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[3] + 1/ni.[7])) #(40-49) - (85+)
all.Zij15
all.Zij16= c(all.pjG4 - all.pjG5)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[4] + 1/ni.[5])) #(50-64) - (65-74)
all.Zij16
all.Zij17= c(all.pjG4 - all.pjG6)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[4] + 1/ni.[6])) #(50-64) - (75-84)
all.Zij17
all.Zij18= c(all.pjG4 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[4] + 1/ni.[7])) #(50-64) - (85+)
all.Zij18
all.Zij19= c(all.pjG5 - all.pjG6)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[5] + 1/ni.[6])) #(65-74) - (75-84)
all.Zij19
all.Zij20= c(all.pjG5 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[5] + 1/ni.[7])) #(65-74) - (85+)
all.Zij20
all.Zij21= c(all.pjG6 - all.pjG7)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[6] + 1/ni.[7])) #(75-84) - (85+)
all.Zij21


all.Zij = c(all.Zij1,all.Zij2,all.Zij3,all.Zij4,all.Zij5,all.Zij6,all.Zij7,all.Zij8,all.Zij9,all.Zij10,all.Zij11,
            all.Zij12,all.Zij13,all.Zij14,all.Zij15,all.Zij16,all.Zij17,all.Zij18,all.Zij19,all.Zij20,all.Zij21) #The z-test-statistics
all.Zij = matrix(all.Zij,nrow=  42, ncol=1)

#Fixed Column

zee.table = table(data18.85)
n = sum(zee.table)
ni. = rowSums(zee.table)
n.j = colSums(zee.table)
all.pjG3 = zee.table[,1]/n.j[1] #all conditional probabilites for row 1
all.pjG4= zee.table[,2]/n.j[2] #all conditional probabilites for row 2
all.pbar2 = ni./n #all probabilities regardless of group
all.Zij22 = c((all.pjG3 - all.pjG4)/sqrt(all.pbar2*(1-all.pbar2)*(1/(n.j[1]) + 1/(n.j[2]))))
all.Zij22 #The z-test-statistics

set.seed(104)
R = 2000
r.perms.cutoff1 = sapply(1:R,function(i){
  perm.data = data18.85
  perm.data$Sex = sample(perm.data$Sex,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(table(perm.data))
  col.sum = colSums(table(perm.data))
  all.pji8 = table(perm.data)[,1]/col.sum[1]
  all.pji9= table(perm.data)[,2]/col.sum[2]
  all.pbar1 = row.sum/sum(col.sum)
  all.Zij3 = c((all.pji8 - all.pji9)/sqrt(all.pbar1*(1-all.pbar1)*(1/col.sum[1] + 1/col.sum[2])))
  Q.r = max(abs(all.Zij3))
  return(Q.r)
})
hist(r.perms.cutoff1)
alpha = 0.05
cutoff.q1 = as.numeric(quantile(r.perms.cutoff1,(1-alpha)))
cutoff.q1


all.Zij22 = c((all.pjG3 - all.pjG4)/sqrt(all.pbar2*(1-all.pbar2)*(1/(n.j[1]) + 1/(n.j[2]))))
all.Zij22 
all.Zij22 = matrix(all.Zij22,nrow=  1)
colnames(all.Zij2) = c("18-29 years","30-39 years", "40-49 years", "50-64 years", "65-74 years", "75-84 years","85 years and older")
rownames(all.Zij2) = c("Female vs. Male")
all.Zij22


all.Zij
cutoff.q
hist(r.perms.cutoff)
all.Zij22
cutoff.q1
hist(r.perms.cutoff1)


all.pjG1 = zee.table[1,]/ni.[1] #18-29
all.pjG1
all.pjG2= zee.table[2,]/ni.[2] #30-39
all.pjG2
all.pjG3= zee.table[3,]/ni.[3] #40-49
all.pjG3
all.pjG4= zee.table[4,]/ni.[4] #50-64
all.pjG4
all.pjG5= zee.table[5,]/ni.[5] #65-74
all.pjG5
all.pjG6= zee.table[6,]/ni.[6] #75-84
all.pjG6
all.pjG7= zee.table[7,]/ni.[7] #85+
all.pjG7


all.pjG3 = zee.table[,1]/n.j[1] #Female
all.pjG4= zee.table[,2]/n.j[2] #Male
all.pbar2 = ni./n #all probabilities regardless of group
all.Zij22 = c((all.pjG3 - all.pjG4)/sqrt(all.pbar2*(1-all.pbar2)*(1/(n.j[1]) + 1/(n.j[2]))))
all.Zij22 








############################################################################################################################## E D I T ##############################
yo1 <- CovidB[CovidB[,4] == "Under 1 year",]
yo1.4 <- CovidB[CovidB[,4] == "1-4 years",]
yo5.14 <- CovidB[CovidB[,4] == "5-14 years",]
yo15.24 <- CovidB[CovidB[,4] == "15-24 years",]
yo25.34 <- CovidB[CovidB[,4] == "25-34 years",]
yo35.44 <- CovidB[CovidB[,4] == "35-44 years",]
yo45.54 <- CovidB[CovidB[,4] == "45-54 years",]
yo55.64 <- CovidB[CovidB[,4] == "55-64 years",]
yo65.74 <- CovidB[CovidB[,4] == "65-74 years",]
yo75.84 <- CovidB[CovidB[,4] == "75-84 years",]
yo85. <- CovidB[CovidB[,4] == "85 years and over",]
yo75.85 = rbind.data.frame(yo75.84 , yo85.)


zee.table1 = xtabs(Death ~ Age_Group + Sex, data = yo75.85)
zee.table1
the.test1 = chisq.test(zee.table,correct = FALSE)
eij1 = the.test$expected

Age_Group1 = rep(c("75-84 years","85 years and older"),times = c(133552,151341))
Sex1 = rep(c("Female","Male","Female","Male"),times = c(58426,75126,87402,63939))################################################################################3
data75.85 = data.frame(Age_Group1,Sex1)
table(data75.85)
zee.table1

chi.sq.obs = as.numeric(the.test$statistic)
chi.sq.obs

set.seed(104)
R = 4000
r.perms = sapply(1:R,function(i){
  perm.data = yo75.85
  perm.data$Sex = sample(perm.data$Sex,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(xtabs(Death ~ Age_Group + Sex, data = yo75.85))$stat
  return(chi.sq.i)
})
perm.pval = mean(r.perms >= chi.sq.obs)
perm.pval

#dependency test: 
yo75.84 <- CovidB[CovidB[,4] == "75-84 years",]
yo85. <- CovidB[CovidB[,4] == "85 years and over",]
yo75.85 = rbind.data.frame(yo75.84 , yo85.)


n = sum(zee.table)
n
ni. = rowSums(zee.table)
ni.
n.j = colSums(zee.table)
n.j

#P(75-84 years | Female) - P(75-84 years | Male)

all.pjG1 = zee.table[1,]/ni.[1] #all conditional probabilites for row 1
all.pjG2= zee.table[2,]/ni.[2] #all conditional probabilites for row 2
all.pbar = n.j/n #all probabilities regardless of group
all.Zij = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
all.Zij

set.seed(104)
R = 3000
r.perms.cutoff = sapply(1:R,function(i){
  perm.data = yo75.85
  perm.data$Age_Group = sample(yo75.85$Age_Group,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(table(perm.data))
  col.sum = colSums(table(perm.data))
  all.pji = zee.table[1,]/row.sum[1]
  all.pji.= zee.table[2,]/row.sum[2]
  all.pbar = col.sum/sum(row.sum)
  all.Zij = c(all.pji - all.pji.)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[2]))
  Q.r = max(abs(all.Zij))
  return(Q.r)
})
hist(r.perms.cutoff)
alpha = 0.01
cutoff.q = as.numeric(quantile(r.perms.cutoff,(1-alpha)))
cutoff.q

all.Zij = matrix(all.Zij,nrow=  1)
colnames(all.Zij) = c("Females","Males")
rownames(all.Zij) = c("75-85 years vs. 85 years and older")
all.Zij

#P(85 years and over | Female) - P(85 years and over | Male)

all.pjG3 = zee.table[,1]/n.j[1] #all conditional probabilites for row 1
all.pjG3
all.pjG4= zee.table[,2]/n.j[2] #all conditional probabilites for row 2
all.pjG4
all.pbar2 = ni./n #all probabilities regardless of group
all.Zij2 = c(all.pjG3 - all.pjG4)/sqrt(all.pbar2*(1-all.pbar)*(1/n.j[1] + 1/n.j[2])) #The z-test-statistics
all.Zij2

set.seed(104)
R = 3000
r.perms.cutoff1 = sapply(1:R,function(i){
  perm.data = yo75.85
  perm.data$Sex = sample(perm.data$Sex,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(zee.table)
  col.sum = colSums(zee.table)
  all.pji = zee.table[,1]/col.sum[1]
  all.pji.= zee.table[,2]/col.sum[2]
  all.pbar = row.sum/sum(col.sum)
  all.Zij = c(all.pji - all.pji.)/sqrt(all.pbar*(1-all.pbar)*(1/col.sum[1] + 1/col.sum[2]))
  Q.r = max(abs(all.Zij))
  return(Q.r)
})
alpha = 0.01
cutoff.q = as.numeric(quantile(r.perms.cutoff1,(1-alpha)))
cutoff.q

all.Zij2 = matrix(all.Zij2,nrow=  1)
colnames(all.Zij2) = c("75-84 years","95 years and older")
rownames(all.Zij2) = c("Female vs. Male")
all.Zij2

############################################################################################################################### E D I T #############################
zee.table = xtabs(Death ~ Age_Group + Sex, data = yo.18.85)
zee.table
the.test = chisq.test(zee.table,correct = FALSE)
eij = the.test$expected


#Summary Statistics:
  #Expected Count on Average:
  eij
  #Contingency Table:
  zee.table
  ni. = rowSums(zee.table)
  n.j = colSums(zee.table)
  nt = sum(zee.table)
  
  #Plots:
  
  library(ggplot2)
  library("plyr")
  DeathSex <- ddply(yo.18.85,.(Age_Group,Sex),summarise, val = mean(Death))
  
  ggplot(yo.18.85, aes(x = factor(Age_Group), y = Death, colour = Sex)) + 
    geom_boxplot(aes(fill =Age_Group )) + 
    geom_point(data = DeathSex, aes(y = val)) +
    geom_line(data = DeathSex, aes(y = val, group = Sex)) +
    theme_bw() + 
    ggtitle("Interaction Plot of Death Counts from COVID-19 Between Age and Sex (n=478678)") +
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Age Groups")+
    ylab("Death Count")+
    labs(caption = "Figure 1: Variability of Death counts appears to be much larger for elder Age-groups.
         Apart from85 and older, Males appear to have a higher death count than women at any given Age-group. 
         Death counts appear to be skewed right for elder Age-groups (65-74 years,75-84 years,85 years and older). 
         Number of deaths per age group do not appear to be the same regardless of sex. 
         Number of deaths per sex do not appear to be the same regardless of age.")+
    theme(plot.caption = element_text(hjust = 0.5))
  
  library(ggplot2)
  ggplot(yo.18.85, aes(fill=Age_Group, y=Death, x=Sex),color=Sex) + 
    geom_bar(position="dodge", stat="identity",aes(fill =Age_Group ))+
    theme_bw()+
    ggtitle("Grouped Barchart of Death Counts from COVID-19 Between Age and Sex (n=478678)") +
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Sex")+
    ylab("Death Count")+
    labs(caption = "Figure 2: Variability of Death counts appears to be much larger for elder Age-groups.
         Apart from age groups of 85 and older and 18-29 years old, 
         Males appear to have a higher death count than women all other Age-group.
         Number of deaths per age group do not appear to be the same regardless of sex. 
         Number of deaths per sex do not appear to be the same regardless of age.")+
    theme(plot.caption = element_text(hjust = 0.5))
    
  
    
    
  
#Permutation Test for Independence
  the.test = chisq.test(zee.table,correct = FALSE)
  chi.sq.obs = as.numeric(the.test$statistic)
  chi.sq.obs
  
  set.seed(104)
  R = 4000
  r.perms = sapply(1:R,function(i){
    perm.data = yo.18.85
    perm.data$Sex = sample(perm.data$Sex,nrow(perm.data),replace = FALSE)
    chi.sq.i = chisq.test(zee.table)$stat
    return(chi.sq.i)
  })
  perm.pval = mean(r.perms >= chi.sq.obs)
  perm.pval

#Assuming Reject H_0:
  n = sum(zee.table)
  ni. = rowSums(zee.table)
  n.j = colSums(zee.table)
  all.pjG1 = zee.table[1,]/ni.[1] #all conditional probabilites for row 1
  all.pjG2= zee.table[2,]/ni.[2] #all conditional probabilites for row 2
  all.pjG2= zee.table[3,]/ni.[3] #all conditional probabilites for row 3
  all.pjG2= zee.table[4,]/ni.[4] #all conditional probabilites for row 4
  all.pjG2= zee.table[5,]/ni.[5] #all conditional probabilites for row 5
  all.pjG2= zee.table[6,]/ni.[6] #all conditional probabilites for row 6
  all.pjG2= zee.table[7,]/ni.[7] #all conditional probabilites for row 7
  all.pbar = n.j/n #all probabilities regardless of group
  Zij.row1.2 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.3 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.4 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.5 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.6 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.7 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  
  Zij.row1.2 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  Zij.row1.2 = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
  
  
  
  
