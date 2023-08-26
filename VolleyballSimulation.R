# Reading in SuperLega league table for 2020/21 season

Superlega_table <- read.table(file='/Users/OYETAYOADEDAMOLA/Documents/project/volleyballdata.txt')
scores<-c(Superlega_table[1,], Superlega_table[2,], Superlega_table[3,], Superlega_table[4,], Superlega_table[5,], Superlega_table[6,], Superlega_table[7,], Superlega_table[8,], Superlega_table[9,], Superlega_table[10,], Superlega_table[11,], Superlega_table[12,])
scores<-as.numeric(scores)

teammix<-function(x, y){
  #x the team
  #y the set of opponents
  names<-vector("integer", 22)
  for(i in 1:11){
    names[2*i]<-y[i]
    names[(2*i)-1]<-x
  }      
  names}

opponentmix<-function(x, y){
  #x the team
  #y the set of opponents
  names3<-vector("character", 22)
  for(i in 1:11){
    names3[(2*i)-1]<-y[i]
    names3[(2*i)]<-x
  }      
  names3}

teammix2<-function(a){
  teamnames<-c("Cisterna", "LubeCivitinova", "Milano", "Modena", "Monza", "Padova", "Perugia", "Piacenza", "Ravenna", "Trentino", "Verona", "ViboValentia")
  names2<-teammix(teamnames[a], teamnames[-a])
  names2
}

teammix3<-function(a){
  teamnames<-c("Cisterna", "LubeCivitinova", "Milano", "Modena", "Monza", "Padova", "Perugia", "Piacenza", "Ravenna", "Trentino", "Verona", "ViboValentia")
  names4<-opponentmix(teamnames[a], teamnames[-a])
  names4
}  

team<-c(teammix2(1), teammix2(2), teammix2(3), teammix2(4), teammix2(5), teammix2(6), teammix2(7), teammix2(8), teammix2(9), teammix2(10), teammix2(11), teammix2(12))
opponent<-c(teammix3(1), teammix3(2), teammix3(3), teammix3(4), teammix3(5), teammix3(6), teammix3(7), teammix3(8), teammix3(9), teammix3(10), teammix3(11), teammix3(12))

home<-rep(c(1, 0), 132)

matrixres<-cbind(team, opponent, home, scores)

resultsrightformat<-matrix(0, 132, 4)
for(i in 1:132){
  resultsrightformat[i,]<-c(matrixres[2*i-1, 1], matrixres[2*i-1, 2], matrixres[2*i-1, 3], matrixres[2*i, 4])
}
yres<-cbind(as.numeric(resultsrightformat[,3]), as.numeric(resultsrightformat[,4]))
hometeam<-resultsrightformat[,1]
awayteam<-resultsrightformat[,2]
model1.glm<-glm(yres~hometeam+awayteam, family=binomial)

homeLubeCivitinova=1*(hometeam=="LubeCivitinova")
homeMilano=1*(hometeam=="Milano")
homeModena=1*(hometeam=="Modena")
homeMonza=1*(hometeam=="Monza")
homePadova=1*(hometeam=="Padova")
homePerugia=1*(hometeam=="Perugia")
homePiacenza=1*(hometeam=="Piacenza")
homeRavenna=1*(hometeam=="Ravenna")
homeTrentino=1*(hometeam=="Trentino")
homeVerona=1*(hometeam=="Verona")
homeViboValentia=1*(hometeam=="ViboValentia")
awayLubeCivitinova=1*(awayteam=="LubeCivitinova")
awayMilano=1*(awayteam=="Milano")
awayModena=1*(awayteam=="Modena")
awayMonza=1*(awayteam=="Monza")
awayPadova=1*(awayteam=="Padova")
awayPerugia=1*(awayteam=="Perugia")
awayPiacenza=1*(awayteam=="Piacenza")
awayRavenna=1*(awayteam=="Ravenna")
awayTrentino=1*(awayteam=="Trentino")
awayVerona=1*(awayteam=="Verona")
awayViboValentia=1*(awayteam=="ViboValentia")

null.glm<-glm(yres~1, family=binomial)

full.glm<-glm(yres~homeLubeCivitinova+homeMilano+homeModena+homeMonza+homePadova+homePerugia+homePiacenza+homeRavenna+homeTrentino+homeVerona+homeViboValentia+awayLubeCivitinova+awayMilano+awayModena+awayMonza+awayPadova+awayPerugia+awayPiacenza+awayRavenna+awayTrentino+awayVerona+awayViboValentia, family=binomial)

step(null.glm, direction="both", scope=formula(full.glm))
step(null.glm, direction="forward", scope=formula(full.glm))
step(full.glm, direction="backward")

step.glm<-glm(formula = yres ~ homeLubeCivitinova + homeMilano + homeModena + homePadova + homeVerona
                +     homeMonza + homePerugia + homePiacenza + homeTrentino + homeViboValentia + homeRavenna
                +     awayLubeCivitinova + awayMilano + awayModena + awayMonza + 
                +     awayPadova + awayPerugia + awayPiacenza + awayRavenna + awayTrentino + 
                +     awayVerona + awayViboValentia, family = binomial)
summary(step.glm)

# Reading in PlusLega league table for 2020/21 season

Pluslega_table <- read.table(file='/Users/OYETAYOADEDAMOLA/Documents/project/volleyballdata2.txt')
scores<-c(Pluslega_table[1,], Pluslega_table[2,], Pluslega_table[3,], Pluslega_table[4,], Pluslega_table[5,], Pluslega_table[6,], Pluslega_table[7,], Pluslega_table[8,], Pluslega_table[9,], Pluslega_table[10,], Pluslega_table[11,], Pluslega_table[12,],Pluslega_table[13,],Pluslega_table[14,])
scores<-as.numeric(scores)


teammix<-function(x, y){
  #x the team
  #y the set of opponents
  names<-vector("character", 26)
  for(i in 1:13){
    names[2*i]<-y[i]
    names[(2*i)-1]<-x
  }      
  names}

opponentmix<-function(x, y){
  #x the team
  #y the set of opponents
  names3<-vector("character", 26)
  for(i in 1:13){
    names3[(2*i)-1]<-y[i]
    names3[(2*i)]<-x
  }      
  names3}

teammix2<-function(a){
  teamnames<-c("Bedzin","Belchatow","Cuprum","CzarniRado","Gdansk","GKSKatowise","Jastrzebski","KedzierzynKozle","Olsztyn","ProjektWar","Rzeszow","SlepskSuwalki","StalNysa","Zawiercie") 
names2<-teammix(teamnames[a], teamnames[-a])
names2
}

teammix3<-function(a){
  teamnames<-c("Bedzin","Belchatow","Cuprum","CzarniRado","Gdansk","GKSKatowise","Jastrzebski","KedzierzynKozle","Olsztyn","ProjektWar","Rzeszow","SlepskSuwalki","StalNysa","Zawiercie")  
names4<-opponentmix(teamnames[a], teamnames[-a])
names4
}

team<-c(teammix2(1), teammix2(2), teammix2(3), teammix2(4), teammix2(5), teammix2(6), teammix2(7), teammix2(8), teammix2(9), teammix2(10), teammix2(11), teammix2(12), teammix2(13), teammix2(14))
opponent<-c(teammix3(1), teammix3(2), teammix3(3), teammix3(4), teammix3(5), teammix3(6), teammix3(7), teammix3(8), teammix3(9), teammix3(10), teammix3(11), teammix3(12), teammix3(13), teammix3(14))

home<-rep(c(1, 0), 182)

matrixres<-cbind(team, opponent, home, scores)
resultsrightformat<-matrix(0, 182, 4)
for(i in 1:182){
  resultsrightformat[i,]<-c(matrixres[2*i-1, 1], matrixres[2*i-1, 2], matrixres[2*i-1, 4], matrixres[2*i, 4])
}

yres<-cbind(as.numeric(resultsrightformat[,3]), as.numeric(resultsrightformat[,4]))
hometeam<-resultsrightformat[,1]
awayteam<-resultsrightformat[,2]
model1.glm<-glm(yres~hometeam+awayteam, family=binomial)
homeBelchatow =1*(hometeam=="Belchatow")
homeCuprum =1*(hometeam=="Cuprum")
homeCzarniRado =1*(hometeam=="CzarniRado")
homeGdansk =1*(hometeam=="Gdansk")
homeGKSKatowise =1*(hometeam=="GKSKatowise")
homeJastrzebski =1*(hometeam=="Jastrzebski")
homeKedzierzynKozle =1*(hometeam=="KedzierzynKozle")
homeOlsztyn =1*(hometeam=="Olsztyn")
homeProjektWar =1*(hometeam=="ProjektWar")
homeRzeszow =1*(hometeam=="Rzeszow")
homeSlepskSuwalki =1*(hometeam=="SlepskSuwalki")
homeStalNysa =1*(hometeam=="StalNysa")
homeZawiercie =1*(hometeam=="Zawiercie")
awayBelchatow =1*(awayteam=="Belchatow")
awayCuprum =1*(awayteam=="Cuprum")
awayCzarniRado =1*(awayteam=="CzarniRado")
awayGdansk =1*(awayteam=="Gdansk")
awayGKSKatowise =1*(awayteam=="GKSKatowise")
awayJastrzebski =1*(awayteam=="Jastrzebski")
awayKedzierzynKozle =1*(awayteam=="KedzierzynKozle")
awayOlsztyn =1*(awayteam=="Olsztyn")
awayProjektWar =1*(awayteam=="ProjektWar")
awayRzeszow =1*(awayteam=="Rzeszow")
awaySlepskSuwalki =1*(awayteam=="SlepskSuwalki")
awayStalNysa =1*(awayteam=="StalNysa")
awayZawiercie =1*(awayteam=="Zawiercie")

null.glm<-glm(yres~1, family=binomial)
full.glm<-glm(yres~ homeBelchatow+homeCuprum+homeCzarniRado+homeGdansk+homeGKSKatowise+homeJastrzebski+homeKedzierzynKozle+homeOlsztyn+homeProjektWar+homeRzeszow+homeSlepskSuwalki+homeStalNysa+homeZawiercie+awayBelchatow+awayCuprum+awayCzarniRado+awayGdansk+awayGKSKatowise+awayJastrzebski+awayKedzierzynKozle+awayOlsztyn+awayProjektWar+awayRzeszow+awaySlepskSuwalki+awayStalNysa+awayZawiercie, family=binomial)

step(null.glm, direction="both", scope=formula(full.glm))
step(null.glm, direction="forward", scope=formula(full.glm))
step(full.glm, direction="backward")

step.glm<-glm(formula = yres ~ homeBelchatow+homeCuprum+homeCzarniRado+homeGdansk+homeGKSKatowise+homeJastrzebski+homeKedzierzynKozle+homeOlsztyn+homeProjektWar+homeRzeszow+homeSlepskSuwalki+homeStalNysa+homeZawiercie+awayBelchatow+awayCuprum+awayCzarniRado+awayGdansk+awayGKSKatowise+awayJastrzebski+awayKedzierzynKozle+awayOlsztyn+awayProjektWar+awayRzeszow+awaySlepskSuwalki+awayStalNysa+awayZawiercie, family = binomial)
summary(step.glm)

glm(formula = yres~awayZawiercie+homeGdansk,family=binomial)


# Creating a mini-league simulation for tournament ranking

#Fix the four teams as Milano, Monza, Perugia, Verona
#Function to simulate a single volleyball as a function of the probability the home team wins a set
volleyballsim<-function(p){
  #Is a function of the probability the first home team wins a set
  #list the possible scores that can occur
  score<-c(3, 0, 0, 3, 3, 1, 1, 3, 3, 2, 2, 3)
  #then group them into a matrix
  score<-matrix(score, byrow=T, ncol=2)
  #Need a list of 6 numbers corresponding to the rows of the above matrix to simulate from
  row<-c(1, 2, 3, 4, 5, 6)
  #Calculate the probabilities calculated in the previous maths part above
  probs<-c(p^3, (1-p)^3, 3*p^3*(1-p), 3*p*(1-p)^3, 6*p^3*(1-p)^2, 6*p^2*(1-p)^3)
  #Choose a row at random by sampling from the numbers 1 to 6 according to the given probabilities
  number<-sample(row, 1, prob=probs)
  #The simulated match score is then the row of score matrix that corresponds to this sampled value
  score[number,]}
#Make sure it is obvious that the following numbers come from the regression model fitted
home<-c(0.3433, 0.4300, 1.0137, 0)
away<-c(-1.1573, -1.4398, -1.6004, -1.1368)
const<-0.2557
volleyregextract<-function(x, y){
  #extract the probability values from the logistic regression model
  reg<-const+home[x]+away[y]
  prob<-exp(reg)/(1+exp(reg))
  prob
}
results<-matrix(0, 4, 8)
for(i in 1:4){
  results[i,]<-c(volleyballsim(volleyregextract(i, i)), volleyballsim(volleyregextract(i, i)), volleyballsim(volleyregextract(i, i)), volleyballsim(volleyregextract(i, i)))
}


homepoints<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(1*(x[2*tags-1]==2))+2*sum((x[2*tags-1]==3+x[2*tags]))
}
#function to calculate the points scored by the away team
awaypoints<-function(x, y){
  n<-length(x)
  tags<-seq(1:n)
  sum(3*(y[tags]>1+x[tags]))+2*sum(y[tags]==x[tags]+1)+1*sum(y[tags]==2)
}
homegoaldiff<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])-sum(x[2*tags])
  
}

homegoals<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])
}

milanopoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
monzapoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
perugiapoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
veronapoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])

milanogd<-homegoaldiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
monzagd<-homegoaldiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
perugiagd<-homegoaldiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
veronagd<-homegoaldiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])

milanogoals<-homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
monzagoals<-homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
perugiagoals<-homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
veronagoals<-homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])

points<-rank(c(milanopoints, monzapoints, perugiapoints, veronapoints))
gd<-rank(c(milanogd, monzagd, perugiagd, veronagd))
goals<-rank(c(milanogoals, monzagoals, perugiagoals, veronagoals))
#alphabetical order
alphabet<-c(4, 3, 2, 1)
#overall ranking
orank<-1000*points+100*gd+10*goals+alphabet
#translate back to lower values meaning higher position
5-rank(orank)

#Can in principle simulate the league say 10000 times
john<-matrix(0, 10, 4)
#copy the code in from above after setting up a loop

john <-matrix(0, 10, 4)
#copy the code in from above after setting up a loop
for(j in 1:10){
  results<-matrix(0, 4, 8)
  for(i in 1:4){
    results<-matrix(0, 4, 8)
    for(i in 1:4){
    results[i,]<-c(volleyballsim(volleyregextract(i, 1)), volleyballsim(volleyregextract(i, 2)), volleyballsim(volleyregextract(i, 3)), volleyballsim(volleyregextract(i, 4)))
                 }
homepoints<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(x[2*tags-1]==2)+2*sum((x[2*tags-1]==1+x[2*tags]))
}
#function to calculate the points scored by the away team
awaypoints<-function(x, y){
  n<-length(x)
  tags<-seq(1:n)
  sum(3*(y[tags]>1+x[tags]))+2*sum(y[tags]==x[tags]+1)+sum(y[tags]==2)
}
homegoaldiff<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])-sum(x[2*tags])
  
}

homegoals<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])
}

milanopoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
monzapoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
perugiapoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
veronapoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])

milanogd<-homegoaldiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
monzagd<-homegoaldiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
perugiagd<-homegoaldiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
veronagd<-homegoaldiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])

milanogoals<-homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
monzagoals<-homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
perugiagoals<-homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
veronagoals<-homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])


points<-rank(c(milanopoints, monzapoints, perugiapoints, veronapoints))
setd<-rank(c(milanogd, monzagd, perugiagd, veronagd))
sets<-rank(c(milanogoals, monzagoals, perugiagoals, veronagoals))

#alphabetical order
alphabet<-c(4, 3, 2, 1)
#overall ranking
orank<-1000*points+100*setd+10*sets+alphabet
#translate back to lower values meaning higher position
john[j,]<-5-rank(orank)}}




# Creating a full-league simulation for tournament ranking for Superlega

#The full superlega volleyball leauge for 2021
#Function to simulate a single volleyball as a function of the probability the home team wins a set
volleyballsim<-function(p){
  #Is a function of the probability the first home team wins a set
  #list the possible scores that can occur
  score<-c(3, 0, 0, 3, 3, 1, 1, 3, 3, 2, 2, 3)
  #then group them into a matrix
  score<-matrix(score, byrow=T, ncol=2)
  #Need a list of 6 numbers corresponding to the rows of the above matrix to simulate from
  row<-c(1, 2, 3, 4, 5, 6)
  #Calculate the probabilities calculated in the previous maths part above
  probs<-c(p^3, (1-p)^3, 3*p^3*(1-p), 3*p*(1-p)^3, 6*p^3*(1-p)^2, 6*p^2*(1-p)^3)
  #Choose a row at random by sampling from the numbers 1 to 6 according to the given probabilities
  number<-sample(row, 1, prob=probs)
  #The simulated match score is then the row of score matrix that corresponds to this sampled value
  score[number,]}
#Make sure it is obvious that the following numbers come from the regression model fitted
home<-c(0.84435, 0.38722, 0.44469, 1.05849, 0.40368, 0.80719, 0.56774, 0.47376, 0.02143, 0.03788, 0.56774, 0.11924)
away<-c(-1.55885, -1.15744, -1.29055, -1.60012, -1.23376, -1.45024, -1.36975, -1.44209, -0.53265, -1.13811, -1.35975, -0.94163)
const<-0.2557
volleyregextract<-function(x, y){
  #extract the probability values from the logistic regression model
  reg<-const+home[x]+away[y]
  prob<-exp(reg)/(1+exp(reg))
  prob
}
results<-matrix(0, 12, 24)
for(i in 1:12){
  results[i,]<-c(volleyballsim(volleyregextract(i, 1)), volleyballsim(volleyregextract(i, 2)), volleyballsim(volleyregextract(i, 3)), volleyballsim(volleyregextract(i, 4)),
                 volleyballsim(volleyregextract(i, 5)), volleyballsim(volleyregextract(i, 6)), volleyballsim(volleyregextract(i, 7)), volleyballsim(volleyregextract(i, 8)),
                 volleyballsim(volleyregextract(i, 9)), volleyballsim(volleyregextract(i, 10)), volleyballsim(volleyregextract(i, 11)), volleyballsim(volleyregextract(i, 12)))
}


homepoints<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(1*(x[2*tags-1]==2))+sum(2*(x[2*tags-1]==1+x[2*tags]))
}
#function to calculate the points scored by the away team
awaypoints<-function(x, y){
  n<-length(x)
  tags<-seq(1:n)
  sum(3*(y[tags]>1+x[tags]))+sum(2*y[tags]==x[tags]+1)+sum(1 *(y[tags]==2))
}
homesetdiff<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])-sum(x[2*tags])
  
}

homesets<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])
}

LubeCivitinovapoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
Milanopoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
Modenapoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
Perugiapoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])
Piacenzapoints<-homepoints(results[5, -c(9, 10)])+awaypoints(results[-5, 9], results[-5, 10])
Trentinopoints<-homepoints(results[6, -c(11, 12)])+awaypoints(results[-6, 11], results[-6, 12])
ViboValentiapoints<-homepoints(results[7, -c(13, 14)])+awaypoints(results[-7, 13], results[-7, 14])
Monzapoints<-homepoints(results[8, -c(15, 16)])+awaypoints(results[-8, 15], results[-8, 16])
Padovapoints<-homepoints(results[9, -c(17, 18)])+awaypoints(results[-9, 17], results[-9, 18])
Veronapoints<-homepoints(results[10, -c(19, 20)])+awaypoints(results[-10, 19], results[-10, 20])
ViboValentiapoints<-homepoints(results[11, -c(21, 22)])+awaypoints(results[-11, 21], results[-11, 22])
Ravennapoints<-homepoints(results[12, -c(23, 24)])+awaypoints(results[-12, 23], results[-12, 24])


LubeCivitinovasetd<-homesetdiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
Milanosetd<-homesetdiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
Modenasetd<-homesetdiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
Perugiasetd<-homesetdiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])
Piacenzasetd<-homesetdiff(results[5, -c(9, 10)])+sum(results[-5, 10])-sum(results[-5, 9])
Trentinosetd<-homesetdiff(results[6, -c(11, 12)])+sum(results[-6, 12])-sum(results[-6, 11])
ViboValentiasetd<-homesetdiff(results[7, -c(13, 14)])+sum(results[-7, 14])-sum(results[-7, 13])
Monzasetd<-homesetdiff(results[8, -c(15, 16)])+sum(results[-8, 16])-sum(results[-8, 15])
Padovasetd<-homesetdiff(results[9, -c(17, 18)])+sum(results[-9, 18])-sum(results[-9, 17])
Veronasetd<-homesetdiff(results[10, -c(19, 20)])+sum(results[-10, 20])-sum(results[-10, 19])
ViboValentiasetd<-homesetdiff(results[11, -c(20, 21)])+sum(results[-11, 22])-sum(results[-11, 21])
Ravennasetd<-homesetdiff(results[12, -c(22, 24)])+sum(results[-12, 24])-sum(results[-12, 23])


LubeCivitinovasets<- homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
Milanosets<- homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
Modenasets<- homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
Perugiasets<- homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])
Piacenzasets<- homegoals(results[5, -c(9, 10)])+sum(results[-5, 10])
Trentinosets<- homegoals(results[6, -c(11, 12)])+sum(results[-6, 12])
ViboValentiasets<- homegoals(results[7, -c(13, 14)])+sum(results[-7, 14])
Monzasets<- homegoals(results[8, -c(15, 16)])+sum(results[-8, 16])
Padovasets<- homegoals(results[9, -c(17, 18)])+sum(results[-9, 18])
Veronasets<- homegoals(results[10, -c(19, 20)])+sum(results[-10, 20])
ViboValentiasets<- homegoals(results[11, -c(21, 22)])+sum(results[-11, 22])
Ravennasets<- homegoals(results[12, -c(23, 24)])+sum(results[-12, 24])


points<-rank(c(LubeCivitinovapoints,Milanopoints,Modenapoints,Perugiapoints,Piacenzapoints,Trentinopoints,ViboValentiapoints,Monzapoints, Padovapoints,
               Veronapoints, ViboValentiapoints,Ravennapoints))
setd<-rank(c(LubeCivitinovasetd,Milanosetd,Modenasetd,Perugiasetd,Piacenzasetd,Trentinosetd,ViboValentiasetd,Monzasetd, Padovasetd,
             Veronasetd, ViboValentiasetd,Ravennasetd))
sets<-rank(c(LubeCivitinovasets,Milanosets,Modenasets,Perugiasets,Piacenzasets,Trentinosets,ViboValentiasets,Monzasets, Padovasets,
             Veronasets, ViboValentiasets,Ravennasets))

#alphabetical order
alphabet<-c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#overall ranking
orank<-1000*points+100*setd+10*sets+alphabet
#translate back to lower values meaning higher position
#table <-13-rank(orank)

    
    #Can in principle simulate the league say 10000 times
    
    table_superlega <-matrix(0, 15, 12)
    #copy the code in from above after setting up a loop
    results<-matrix(0, 12, 24)
    for(j in 1:15){
      results<-matrix(0, 12, 24)
      for(i in 1:12){
        results<-matrix(0, 12, 24)
        for(i in 1:12){
          results[i,]<-c(volleyballsim(volleyregextract(i, 1)), volleyballsim(volleyregextract(i, 2)), volleyballsim(volleyregextract(i, 3)), volleyballsim(volleyregextract(i, 4)),
                         volleyballsim(volleyregextract(i, 5)), volleyballsim(volleyregextract(i, 6)), volleyballsim(volleyregextract(i, 7)), volleyballsim(volleyregextract(i, 8)),
                         volleyballsim(volleyregextract(i, 9)), volleyballsim(volleyregextract(i, 10)), volleyballsim(volleyregextract(i, 11)), volleyballsim(volleyregextract(i, 12)))
        }
        homepoints<-function(x){
          n<-length(x)
          tags<-seq(1:(n/2))
          sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(1*(x[2*tags-1]==2))+sum(2*(x[2*tags-1]==1+x[2*tags]))
        }
        #function to calculate the points scored by the away team
        awaypoints<-function(x, y){
          n<-length(x)
          tags<-seq(1:n)
          sum(3*(y[tags]>1+x[tags]))+sum(2*(y[tags]==x[tags]+1))+sum(y[tags]==2)
        }
        homesetdiff<-function(x){
          n<-length(x)
          tags<-seq(1:(n/2))
          sum(x[2*tags-1])-sum(x[2*tags])
          
        }
        
        homesets<-function(x){
          n<-length(x)
          tags<-seq(1:(n/2))
          sum(x[2*tags-1])
        }
        
        LubeCivitinovapoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
        Milanopoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
        Modenapoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
        Perugiapoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])
        Piacenzapoints<-homepoints(results[5, -c(9, 10)])+awaypoints(results[-5, 9], results[-5, 10])
        Trentinopoints<-homepoints(results[6, -c(11, 12)])+awaypoints(results[-6, 11], results[-6, 12])
        ViboValentiapoints<-homepoints(results[7, -c(13, 14)])+awaypoints(results[-7, 13], results[-7, 14])
        Monzapoints<-homepoints(results[8, -c(15, 16)])+awaypoints(results[-8, 15], results[-8, 16])
        Padovapoints<-homepoints(results[9, -c(17, 18)])+awaypoints(results[-9, 17], results[-9, 18])
        Veronapoints<-homepoints(results[10, -c(19, 20)])+awaypoints(results[-10, 19], results[-10, 20])
        ViboValentiapoints<-homepoints(results[11, -c(21, 22)])+awaypoints(results[-11, 21], results[-11, 22])
        Ravennapoints<-homepoints(results[12, -c(23, 24)])+awaypoints(results[-12, 23], results[-12, 24])
        
        
        LubeCivitinovasetd<-homesetdiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
        Milanosetd<-homesetdiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
        Modenasetd<-homesetdiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
        Perugiasetd<-homesetdiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])
        Piacenzasetd<-homesetdiff(results[5, -c(9, 10)])+sum(results[-5, 10])-sum(results[-5, 9])
        Trentinosetd<-homesetdiff(results[6, -c(11, 12)])+sum(results[-6, 12])-sum(results[-6, 11])
        ViboValentiasetd<-homesetdiff(results[7, -c(13, 14)])+sum(results[-7, 14])-sum(results[-7, 13])
        Monzasetd<-homesetdiff(results[8, -c(15, 16)])+sum(results[-8, 16])-sum(results[-8, 15])
        Padovasetd<-homesetdiff(results[9, -c(17, 18)])+sum(results[-9, 18])-sum(results[-9, 17])
        Veronasetd<-homesetdiff(results[10, -c(19, 20)])+sum(results[-10, 20])-sum(results[-10, 19])
        ViboValentiasetd<-homesetdiff(results[11, -c(20, 21)])+sum(results[-11, 22])-sum(results[-11, 21])
        Ravennasetd<-homesetdiff(results[12, -c(22, 24)])+sum(results[-12, 24])-sum(results[-12, 23])
        
        
        LubeCivitinovasets<- homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
        Milanosets<- homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
        Modenasets<- homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
        Perugiasets<- homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])
        Piacenzasets<- homegoals(results[5, -c(9, 10)])+sum(results[-5, 10])
        Trentinosets<- homegoals(results[6, -c(11, 12)])+sum(results[-6, 12])
        ViboValentiasets<- homegoals(results[7, -c(13, 14)])+sum(results[-7, 14])
        Monzasets<- homegoals(results[8, -c(15, 16)])+sum(results[-8, 16])
        Padovasets<- homegoals(results[9, -c(17, 18)])+sum(results[-9, 18])
        Veronasets<- homegoals(results[10, -c(19, 20)])+sum(results[-10, 20])
        ViboValentiasets<- homegoals(results[11, -c(21, 22)])+sum(results[-11, 22])
        Ravennasets<- homegoals(results[12, -c(23, 24)])+sum(results[-12, 24])
        
        
        
        points<-rank(c(LubeCivitinovapoints,Milanopoints,Modenapoints,Perugiapoints,Piacenzapoints,Trentinopoints,ViboValentiapoints,Monzapoints, Padovapoints,
                       Veronapoints, ViboValentiapoints,Ravennapoints))
        setd<-rank(c(LubeCivitinovasetd,Milanosetd,Modenasetd,Perugiasetd,Piacenzasetd,Trentinosetd,ViboValentiasetd,Monzasetd, Padovasetd,
                     Veronasetd, ViboValentiasetd,Ravennasetd))
       
        
        #alphabetical order
        alphabet<-c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
        #overall ranking
        orank<-1000*points+100*setd+10*sets+alphabet
        #translate back to lower values meaning higher position
        table_superlega[j,]<-13-rank(orank)
      }}


    
# Loop to calculate Standard deviation for simulated table
for (cols in 1:ncol(table_superlega)) {
    team_sd <- sd(table_superlega[,cols])
    print(paste( "Team", cols, "standard deviation:", team_sd))
    
}
mean(sd(table_superlega[,1]),sd(table_superlega[,2]),sd(table_superlega[,3]),sd(table_superlega[,4]),sd(table_superlega[,5]),sd(table_superlega[,6]),sd(table_superlega[,7]),
    sd(table_superlega[,8]),sd(table_superlega[,9]),sd(table_superlega[,10]),sd(table_superlega[,11],sd(table_superlega[,12])))


# Creating a full-league simulation for tournament ranking for Pluslega

#The full pluslega volleyball leauge for 2021
#Function to simulate a single volleyball as a function of the probability the home team wins a set
volleyballsim<-function(p){
  #Is a function of the probability the first home team wins a set
  #list the possible scores that can occur
  score<-c(3, 0, 0, 3, 3, 1, 1, 3, 3, 2, 2, 3)
  #then group them into a matrix
  score<-matrix(score, byrow=T, ncol=2)
  #Need a list of 6 numbers corresponding to the rows of the above matrix to simulate from
  row<-c(1, 2, 3, 4, 5, 6)
  #Calculate the probabilities calculated in the previous maths part above
  probs<-c(p^3, (1-p)^3, 3*p^3*(1-p), 3*p*(1-p)^3, 6*p^3*(1-p)^2, 6*p^2*(1-p)^3)
  #Choose a row at random by sampling from the numbers 1 to 6 according to the given probabilities
  number<-sample(row, 1, prob=probs)
  #The simulated match score is then the row of score matrix that corresponds to this sampled value
  score[number,]}
#Make sure it is obvious that the following numbers come from the regression model fitted
home<-c(1.498812, 1.033559, 0.637942, 1.774420, 1.097550, 2.068995, 2.077612, 1.371850, 1.266587, 1.791166, 0.952415, 0.829076, 1.045071)
away<-c(-1.738855, -0.694098, -0.5043767, -1.298150, -0.829960, -2.064124, -2.587916, -0.676496, -1.741596, -1.375229, -0.997624, -0.3772294, -1.704793)
const<-0.2557
volleyregextract<-function(x, y){
  #extract the probability values from the logistic regression model
  reg<-const+home[x]+away[y]
  prob<-exp(reg)/(1+exp(reg))
  prob
}
results<-matrix(0, 13, 26)
for(i in 1:13){
results[i,]<-c(volleyballsim(volleyregextract(i, 1)), volleyballsim(volleyregextract(i, 2)), volleyballsim(volleyregextract(i, 3)), volleyballsim(volleyregextract(i, 4)),
                             volleyballsim(volleyregextract(i, 5)), volleyballsim(volleyregextract(i, 6)), volleyballsim(volleyregextract(i, 7)), volleyballsim(volleyregextract(i, 8)),
                             volleyballsim(volleyregextract(i, 9)), volleyballsim(volleyregextract(i, 10)), volleyballsim(volleyregextract(i, 11)), volleyballsim(volleyregextract(i, 12)),
                             volleyballsim(volleyregextract(i, 13)))
}


homepoints<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(1*(x[2*tags-1]==2))+sum(2*(x[2*tags-1]==1+x[2*tags]))
}
#function to calculate the points scored by the away team
awaypoints<-function(x, y){
  n<-length(x)
  tags<-seq(1:n)
  sum(3*(y[tags]>1+x[tags]))+sum(2*y[tags]==x[tags]+1)+sum(1 *(y[tags]==2))
}
homesetdiff<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])-sum(x[2*tags])
  
}

homesets<-function(x){
  n<-length(x)
  tags<-seq(1:(n/2))
  sum(x[2*tags-1])
}

Belchatowpoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
Cuprumpoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
CzarniRadopoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
Gdanskpoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])
Gksktatowisepoints<-homepoints(results[5, -c(9, 10)])+awaypoints(results[-5, 9], results[-5, 10])
Jastrzebskipoints<-homepoints(results[6, -c(11, 12)])+awaypoints(results[-6, 11], results[-6, 12])
Kedzierzynkozlepoints<-homepoints(results[7, -c(13, 14)])+awaypoints(results[-7, 13], results[-7, 14])
Olsztynpoints<-homepoints(results[8, -c(15, 16)])+awaypoints(results[-8, 15], results[-8, 16])
Projektwarpoints<-homepoints(results[9, -c(17, 18)])+awaypoints(results[-9, 17], results[-9, 18])
Rzeszowpoints<-homepoints(results[10, -c(19, 20)])+awaypoints(results[-10, 19], results[-10, 20])
SlepskSuwalkipoints<-homepoints(results[11, -c(21, 22)])+awaypoints(results[-11, 21], results[-11, 22])
StalNysapoints<-homepoints(results[12, -c(23, 24)])+awaypoints(results[-12, 23], results[-12, 24])
Zawierciepoints<-homepoints(results[13, -c(25, 26)])+awaypoints(results[-13, 25], results[-13, 26])

Belchatowsetd<-homesetdiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
Cuprumsetd<-homesetdiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
CzarniRadosetd<-homesetdiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
Gdansksetd<-homesetdiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])
Gksktatowisesetd<-homesetdiff(results[5, -c(9, 10)])+sum(results[-5, 10])-sum(results[-5, 9])
Jastrzebskisetd<-homesetdiff(results[6, -c(11, 12)])+sum(results[-6, 12])-sum(results[-6, 11])
Kedzierzynkozlesetd<-homesetdiff(results[7, -c(13, 14)])+sum(results[-7, 14])-sum(results[-7, 13])
Olsztynsetd<-homesetdiff(results[8, -c(15, 16)])+sum(results[-8, 16])-sum(results[-8, 15])
Projektwarsetd<-homesetdiff(results[9, -c(17, 18)])+sum(results[-9, 18])-sum(results[-9, 17])
Rzeszowsetd<-homesetdiff(results[10, -c(19, 20)])+sum(results[-10, 20])-sum(results[-10, 19])
SlepskSuwalkisetd<-homesetdiff(results[11, -c(20, 21)])+sum(results[-11, 22])-sum(results[-11, 21])
StalNysasetd<-homesetdiff(results[12, -c(22, 24)])+sum(results[-12, 24])-sum(results[-12, 23])
Zawierciesetd<-homesetdiff(results[13, -c(25, 26)])+sum(results[-13, 26])-sum(results[-13, 25])

Belchatowsets<- homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
Cuprumsets<- homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
CzarniRadosets<- homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
Gdansksets<- homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])
Gksktatowisesets<- homegoals(results[5, -c(9, 10)])+sum(results[-5, 10])
Jastrzebskisets<- homegoals(results[6, -c(11, 12)])+sum(results[-6, 12])
Kedzierzynkozlesets<- homegoals(results[7, -c(13, 14)])+sum(results[-7, 14])
Olsztynsets<- homegoals(results[8, -c(15, 16)])+sum(results[-8, 16])
Projektwarsets<- homegoals(results[9, -c(17, 18)])+sum(results[-9, 18])
Rzeszowsets<- homegoals(results[10, -c(19, 20)])+sum(results[-10, 20])
SlepskSuwalkisets<- homegoals(results[11, -c(21, 22)])+sum(results[-11, 22])
StalNysasets<- homegoals(results[12, -c(23, 24)])+sum(results[-12, 24])
Zawierciesets<- homegoals(results[13, -c(25, 26)])+sum(results[-13, 26])

points<-rank(c(Belchatowpoints,Cuprumpoints,CzarniRadopoints,Gdanskpoints,Gksktatowisepoints,Jastrzebskipoints,Kedzierzynkozlepoints,Olsztynpoints, Projektwarpoints,
               Rzeszowpoints, SlepskSuwalkipoints,StalNysapoints,Zawierciepoints))
setd<-rank(c(Belchatowsetd,Cuprumsetd,CzarniRadosetd,Gdansksetd,Gksktatowisesetd,Jastrzebskisetd,Kedzierzynkozlesetd,Olsztynsetd, Projektwarsetd,
             Rzeszowsetd, SlepskSuwalkisetd,StalNysasetd,Zawierciesetd))
sets<-rank(c(Belchatowsets,Cuprumsets,CzarniRadosets,Gdansksets,Gksktatowisesets,Jastrzebskisets,Kedzierzynkozlesets,Olsztynsets, Projektwarsets,
             Rzeszowsets, SlepskSuwalkisets,StalNysasets,Zawierciesets))

#alphabetical order
alphabet<-c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#overall ranking
orank<-1000*points+100*setd+10*sets+alphabet
#translate back to lower values meaning higher position


#Can in principle simulate the league say 10000 times

table_Plus_Lega <-matrix(0, 15, 13)
#copy the code in from above after setting up a loop
for(j in 1:15){
results<-matrix(0, 13, 26)
for(i in 1:13){
  results<-matrix(0, 13, 26)
  for(i in 1:13){
  results[i,]<-c(volleyballsim(volleyregextract(i, 1)), volleyballsim(volleyregextract(i, 2)), volleyballsim(volleyregextract(i, 3)), volleyballsim(volleyregextract(i, 4)),
                 volleyballsim(volleyregextract(i, 5)), volleyballsim(volleyregextract(i, 6)), volleyballsim(volleyregextract(i, 7)), volleyballsim(volleyregextract(i, 8)),
                 volleyballsim(volleyregextract(i, 9)), volleyballsim(volleyregextract(i, 10)), volleyballsim(volleyregextract(i, 11)), volleyballsim(volleyregextract(i, 12)),
                 volleyballsim(volleyregextract(i, 13)))
  }
  homepoints<-function(x){
    n<-length(x)
    tags<-seq(1:(n/2))
    sum(3*(x[2*tags-1]>1+x[2*tags]))+sum(1*(x[2*tags-1]==2))+sum(2*(x[2*tags-1]==1+x[2*tags]))
  }
  #function to calculate the points scored by the away team
  awaypoints<-function(x, y){
    n<-length(x)
    tags<-seq(1:n)
    sum(3*(y[tags]>1+x[tags]))+sum(2*(y[tags]==x[tags]+1))+sum(y[tags]==2)
  }
  homesetdiff<-function(x){
    n<-length(x)
    tags<-seq(1:(n/2))
    sum(x[2*tags-1])-sum(x[2*tags])
    
  }
  
  homesets<-function(x){
    n<-length(x)
    tags<-seq(1:(n/2))
    sum(x[2*tags-1])
  }
  
  Belchatowpoints<-homepoints(results[1, -c(1, 2)])+awaypoints(results[-1, 1], results[-1, 2])
  Cuprumpoints<-homepoints(results[2, -c(3, 4)])+awaypoints(results[-2, 3], results[-2, 4])
  CzarniRadopoints<-homepoints(results[3, -c(5, 6)])+awaypoints(results[-3, 5], results[-3, 6])
  Gdanskpoints<-homepoints(results[4, -c(7, 8)])+awaypoints(results[-4, 7], results[-4, 8])
  Gksktatowisepoints<-homepoints(results[5, -c(9, 10)])+awaypoints(results[-5, 9], results[-5, 10])
  Jastrzebskipoints<-homepoints(results[6, -c(11, 12)])+awaypoints(results[-6, 11], results[-6, 12])
  Kedzierzynkozlepoints<-homepoints(results[7, -c(13, 14)])+awaypoints(results[-7, 13], results[-7, 14])
  Olsztynpoints<-homepoints(results[8, -c(15, 16)])+awaypoints(results[-8, 15], results[-8, 16])
  Projektwarpoints<-homepoints(results[9, -c(17, 18)])+awaypoints(results[-9, 17], results[-9, 18])
  Rzeszowpoints<-homepoints(results[10, -c(19, 20)])+awaypoints(results[-10, 19], results[-10, 20])
  SlepskSuwalkipoints<-homepoints(results[11, -c(21, 22)])+awaypoints(results[-11, 21], results[-11, 22])
  StalNysapoints<-homepoints(results[12, -c(23, 24)])+awaypoints(results[-12, 23], results[-12, 24])
  Zawierciepoints<-homepoints(results[13, -c(25, 26)])+awaypoints(results[-13, 25], results[-13, 26])
  
  Belchatowsetd<-homesetdiff(results[1, -c(1, 2)])+sum(results[-1, 2])-sum(results[-1, 1])
  Cuprumsetd<-homesetdiff(results[2, -c(3, 4)])+sum(results[-2, 4])-sum(results[-2, 3])
  CzarniRadosetd<-homesetdiff(results[3, -c(5, 6)])+sum(results[-3, 6])-sum(results[-3, 5])
  Gdansksetd<-homesetdiff(results[4, -c(7, 8)])+sum(results[-4, 8])-sum(results[-4, 7])
  Gksktatowisesetd<-homesetdiff(results[5, -c(9, 10)])+sum(results[-5, 10])-sum(results[-5, 9])
  Jastrzebskisetd<-homesetdiff(results[6, -c(11, 12)])+sum(results[-6, 12])-sum(results[-6, 11])
  Kedzierzynkozlesetd<-homesetdiff(results[7, -c(13, 14)])+sum(results[-7, 14])-sum(results[-7, 13])
  Olsztynsetd<-homesetdiff(results[8, -c(15, 16)])+sum(results[-8, 16])-sum(results[-8, 15])
  Projektwarsetd<-homesetdiff(results[9, -c(17, 18)])+sum(results[-9, 18])-sum(results[-9, 17])
  Rzeszowsetd<-homesetdiff(results[10, -c(19, 20)])+sum(results[-10, 20])-sum(results[-10, 19])
  SlepskSuwalkisetd<-homesetdiff(results[11, -c(20, 21)])+sum(results[-11, 22])-sum(results[-11, 21])
  StalNysasetd<-homesetdiff(results[12, -c(22, 24)])+sum(results[-12, 24])-sum(results[-12, 23])
  Zawierciesetd<-homesetdiff(results[13, -c(25, 26)])+sum(results[-13, 26])-sum(results[-13, 25])
  
  Belchatowsets<- homegoals(results[1, -c(1, 2)])+sum(results[-1, 2])
  Cuprumsets<- homegoals(results[2, -c(3, 4)])+sum(results[-2, 4])
  CzarniRadosets<- homegoals(results[3, -c(5, 6)])+sum(results[-3, 6])
  Gdansksets<- homegoals(results[4, -c(7, 8)])+sum(results[-4, 8])
  Gksktatowisesets<- homegoals(results[5, -c(9, 10)])+sum(results[-5, 10])
  Jastrzebskisets<- homegoals(results[6, -c(11, 12)])+sum(results[-6, 12])
  Kedzierzynkozlesets<- homegoals(results[7, -c(13, 14)])+sum(results[-7, 14])
  Olsztynsets<- homegoals(results[8, -c(15, 16)])+sum(results[-8, 16])
  Projektwarsets<- homegoals(results[9, -c(17, 18)])+sum(results[-9, 18])
  Rzeszowsets<- homegoals(results[10, -c(19, 20)])+sum(results[-10, 20])
  SlepskSuwalkisets<- homegoals(results[11, -c(21, 22)])+sum(results[-11, 22])
  StalNysasets<- homegoals(results[12, -c(23, 24)])+sum(results[-12, 24])
  Zawierciesets<- homegoals(results[13, -c(25, 26)])+sum(results[-13, 26])
  
  
  points<-rank(c(Belchatowpoints,Cuprumpoints,CzarniRadopoints,Gdanskpoints,Gksktatowisepoints,Jastrzebskipoints,Kedzierzynkozlepoints,Olsztynpoints, Projektwarpoints,
                 Rzeszowpoints, SlepskSuwalkipoints,StalNysapoints,Zawierciepoints))
  setd<-rank(c(Belchatowsetd,Cuprumsetd,CzarniRadosetd,Gdansksetd,Gksktatowisesetd,Jastrzebskisetd,Kedzierzynkozlesetd,Olsztynsetd, Projektwarsetd,
              Rzeszowsetd, SlepskSuwalkisetd,StalNysasetd,Zawierciesetd))
  sets<-rank(c(Belchatowsets,Cuprumsets,CzarniRadosets,Gdansksets,Gksktatowisesets,Jastrzebskisets,Kedzierzynkozlesets,Olsztynsets, Projektwarsets,
               Rzeszowsets, SlepskSuwalkisets,StalNysasets,Zawierciesets))
  
  #alphabetical order
  alphabet<-c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  #overall ranking
  orank<-1000*points+100*setd+10*sets+alphabet
  #translate back to lower values meaning higher position
  table_Plus_Lega[j,]<-14-rank(orank)
}}


# Loop to calculate Standard deviation for simulated table
for (cols in 1:ncol(table_Plus_Lega)) {
  team_sd <- sd(table_Plus_Lega[,cols])
  print(paste( "Team", cols, "standard deviation:", team_sd))
  
}
mean(sd(table_Plus_Lega[,1]),sd(table_Plus_Lega[,2]),sd(table_Plus_Lega[,3]),sd(table_Plus_Lega[,4]),sd(table_Plus_Lega[,5]),sd(table_Plus_Lega[,6]),sd(table_Plus_Lega[,7]),
     sd(table_Plus_Lega[,8]),sd(table_Plus_Lega[,9]),sd(table_Plus_Lega[,10]),sd(table_Plus_Lega[,11],sd(table_Plus_Lega[,12]),sd(table_Plus_Lega[,13])))


# Book makers odd for 10 selected matches in Superlega league
Superlega_bookmaker<-function(x, y){
  sum = 0.21080+x+y
  p <- exp(sum)/(1+exp(sum))
  pxwin<-6*p^5-15*p^4+10*p^3
  oddx<- (1-pxwin)/pxwin
  marginx <- oddx*1.1
  pywin<-1-pxwin
  oddy <- (1-pywin)/pywin
  marginy <- oddy * 1.1
  return(c(oddx,marginx,oddy,marginy))
}

booker_odd_table <- matrix(0,10,4)
teamscompared <-c("homeModena_awayPerugia", "homePadova_awayPiacennza", "homeRavenna_awayTretino", "homeVerona_awayVibolentia", "homeLubeCivitinova_awayMilano",
                  "homePadova_awayVerona","homePerugia_awayModena","homeViboValentina_awayRavenna","homePiacenza_awayVerona","homeLubeCivitinova_awayPiacenza")
home_ <- c(0.44469, 0.02143, 0.11924, 0.03788,0.84435,0.02143,1.05849,0.56774,0.40368,0.84435 )
away_ <- c(-1.60012, -1.23376, -1.45024, -1.35975, -1.15744,-1.13811,-1.29055,-0.94163,-1.13811,-1.23376)

booker_odd_table<-matrix(0,10,4)
for (i in 1:10){
  booker_odd_table[i,] =Superlega_bookmaker(home_[i],away_[i])
}

bookers_odd_table <- cbind(teamscompared ,booker_odd_table)


# Book makers odd for 10 selected matches in Pluslega league
Pluslega_bookmaker<-function(x, y){
  sum = -0.07848+x+y
  p <- exp(sum)/(1+exp(sum))
  pxwin<-6*p^5-15*p^4+10*p^3
  oddx<- (1-pxwin)/pxwin
  marginx <- oddx*1.1
  pywin<-1-pxwin
  oddy <- (1-pywin)/pywin
  marginy <- oddy * 1.1
  return(c(oddx,marginx,oddy,marginy))
}

booker_odd_table <- matrix(0,10,4)
teamscompared <-c("homeGdansk_awayOlsztyn", "homeStalNysa_awayRzeszow", "homeCuprum_awayZawiercie", "homeJastrzebski_awayCuprum", "homeProjektwar_awayStalNysa",
                  "homeBelchatow_awayCuprum","homeOlsztyn_awaySlepskSuwalki","homeJastrzebski_awayGKSKatowise","homeGdansk_awayCzarniRado","homeGKSKatowise_awayProjektWar" )
homee <- c(0.44469, 0.829076, 1.033559, 2.068995, 1.266587,1.498812,1.371850,2.068995,1.774420,1.097550)
awayy <- c(-1.60012, -1.375229, -1.704793, -0.694098, -0.372294,-0.694098,-0.997624,-0.829960,-0.504377,-1.741596)

booker_odd_table<-matrix(0,10,4)
for (i in 1:10){
  booker_odd_table[i,] = Pluslega_bookmaker(homee[i],awayy[i])
}

bookers_odd_table <- cbind(teamscompared ,booker_odd_table)




