library(tidyverse)
library(HMDHFDplus)
options(scipen = 999)
# Input your HMD username and password
HMD.id = ""
HMD.pw = ""
# Input country code of interest (list below)
# getHMDcountries() %>% View()
Names = "AUS"


yeard = 10 # Default: a2-a1, but you can change it to any number
latest = 2020
y1 = latest-yeard
top = 100


# calcuation
Deco = c()
for (A in c(25,50,75,100)) {
  aged = A

  RES =c()
    for (g in c("Female")) {
      mx = readHMDweb(Names,"Mx_1x1",HMD.id,HMD.pw)
      # mx = read.table(paste0(z,"/Mx_1x1.txt"), header = TRUE, fill = TRUE, skip = 1)
      mx = pivot_longer(mx,3:5,names_to = "Sex",values_to = "mx")
      mx = mx %>% filter(Sex==g)
      mx$mx = as.numeric(as.character(mx$mx))
      mx$mx = ifelse(is.na(mx$mx),0,mx$mx)
      P = readHMDweb(Names,"Population",HMD.id,HMD.pw) %>% select(1,2,4,5,6)
      colnames(P) = c("Year","Age","Female","Male","Total")# P = read.table(paste0(z,"/Population.txt"), header = TRUE, fill = TRUE, skip = 1)
      P$Year = as.numeric(gsub("[+]", "", P$Year))
      P = pivot_longer(P,3:5,names_to = "Sex",values_to = "PopS")
      P = P %>% filter(Sex==g)
      P$PopS = ifelse(P$PopS==0,1,P$PopS)
      tem = P %>% mutate(State = z)
      # Pop = rbind(Pop,tem)
      
      lowest.year=max(range(mx$Year)[1],range(P$Year)[1])
      
      if((y1-aged)<lowest.year){break()}
      
      pop1 <- P %>% filter(Year == y1+yeard) %>% summarise(sum(PopS)) %>% as.numeric()
      pop2 <- P %>% filter(Year == y1) %>% summarise(sum(PopS)) %>% as.numeric()
      R = log(pop1/pop2)/yeard
      
      # select a country, insert starting year (y1), ending year (y2) and starting age (a1), ending age (a2), the difference of year(yeard)
      Ra1 <-  c()
      Ra2 <-  c()
      DS <- c()
      MIG <- c()
      for (AGE in c(0:(top-aged))) {
        #  # Find the contries name above
        # see y2 below
        a1 = AGE
        a2 = AGE+aged
        
        # calculate growth rate for a2
        ra2<-c()
        i<-y1
        x<-0
        t = which(P$Year==i & P$Age==a2)
        N <- P$PopS[t]
        t = which(P$Year==i+yeard & P$Age==a2)
        Nf <- P$PopS[t]
        x=log(Nf/N)/yeard
        ra2 = append(ra2,x)
        
        
        # calculate growth rate for a1
        ra1<-c()
        i<-y1
        x<-0
        t = which(P$Year==i-aged+yeard & P$Age==a1)
        N <- P$PopS[t]
        t = which(P$Year==i-aged & P$Age==a1)
        Np <- P$PopS[t]
        x=log(N/Np)/yeard
        ra1 = append(ra1,x)
        
        remove(N,Nf,Np,x)
        
        # calculate growth rate of mortality
        dS <- c()
        qxe<-c()
        j = i-aged
        for (a in a1:(a2-1)) {
          t = which(mx$Year==j & mx$Age==a)
          q <- mx$mx[t]/(1+0.5*mx$mx[t])
          qxe = append(qxe,q)
          j=j+1
        }
        lxe<-cumprod((1-qxe))
        
        qxl<-c()
        j = i-aged+yeard
        for (a in a1:a2) {
          t = which(mx$Year==j & mx$Age==a)
          q <- mx$mx[t]/(1+0.5*mx$mx[t])
          qxl = append(qxl,q)
          j=j+1
        }
        lxl<- cumprod((1-qxl))
        
        s = log(lxl[(aged)]/lxe[(aged)])/(yeard)
        dS = append(dS,s)
        dS = ifelse(is.na(dS),0,dS)
        dS = ifelse(is.infinite(dS),0,dS)
        
        # remove(a,dxe,dxl,i,j,lxe,lxl,m,mxe,mxl,qxe,qxl,s,Se,Sl,y,aged,yeard)
        Mig=ra2-ra1-dS
        # calculate the error
        Ra1 <- append(Ra1,ra1)
        Ra2 <- append(Ra2,ra2)
        DS <- append(DS,dS)
        MIG <- append(MIG,Mig)
      }
      
      # create a final dataframe
      result<-data.frame(State = z, Sex = g,Age = aged:top,Ra2,Ra1,DS,MIG)
      
      
      Ra1 <-  c()
      Ra2 <-  c()
      DS <- c()
      MIG <- c()
      for (AGE in c(0:(aged-1))) {
        
        # see y2 below
        a1 = 0
        a2 = AGE
        
        # calculate growth rate for a2
        ra2<-c()
        i<-y1
        x<-0
        t = which(P$Year==i & P$Age==a2)
        N <- P$PopS[t]
        t = which(P$Year==i+yeard & P$Age==a2)
        Nf <- P$PopS[t]
        x=log(Nf/N)/yeard
        ra2 = append(ra2,x)
        
        
        # calculate growth rate for a1
        ra1<-c()
        i<-y1+aged-AGE
        x<-0
        t = which(P$Year==i-aged+yeard & P$Age==a1)
        N <- P$PopS[t]
        t = which(P$Year==i-aged & P$Age==a1)
        Np <- P$PopS[t]
        x=log(N/Np)/yeard
        ra1 = append(ra1,x)
        
        remove(N,Nf,Np,x)
        
        dS <- c()
        qxe<-c()
        j = i-aged
        for (a in a1:a2) {
          t = which(mx$Year==j & mx$Age==a)
          q <- mx$mx[t]/(1+0.5*mx$mx[t])
          qxe = append(qxe,q)
          j=j+1
        }
        lxe<-cumprod((1-qxe))
        
        qxl<-c()
        j = i-aged+yeard
        for (a in a1:a2) {
          t = which(mx$Year==j & mx$Age==a)
          q <- mx$mx[t]/(1+0.5*mx$mx[t])
          qxl = append(qxl,q)
          j=j+1
        }
        lxl<- cumprod((1-qxl))
        
        s = log(lxl[length(lxl)]/lxe[(length(lxe))])/(yeard)
        dS = append(dS,s)
        
        Mig=ra2-ra1-dS
        # calculate the error
        Ra1 <- append(Ra1,ra1)
        Ra2 <- append(Ra2,ra2)
        DS <- append(DS,dS)
        MIG <- append(MIG,Mig)
        
      }
      
      Result<-rbind(data.frame(State = z, Sex = g,Age = 0:(aged-1),Ra2,Ra1,DS,MIG),result)
      
      
      #### Cx ####
      pop <- P %>% filter(Year == y1) %>% filter(Age %in% c(0:top)) %>% pull(PopS)
      pop2 <- pop * exp(Result$Ra2*0.5*yeard)
      Cx = pop2/sum(pop2)
      
      Result = cbind(Result,Cx)
      
      R - sum(Result$Cx * Result$Ra2)
      
      RES = rbind(RES,Result)
    }
  if((y1-aged)<lowest.year){break()}
    RES$aged = A
    Deco = rbind(Deco,RES)

}

# result
Deco %>%filter(Sex=="Female")%>% mutate(x= Ra2*Cx*100) %>% group_by(aged) %>% summarise(sum(x))

Deco %>%filter(Sex=="Female")%>% mutate(x= Ra1*Cx*100) %>% group_by(aged) %>% summarise(sum(x))
Deco %>%filter(Sex=="Female")%>% mutate(x= DS*Cx*100) %>% group_by(aged) %>% summarise(sum(x))
Deco %>%filter(Sex=="Female")%>% mutate(x= MIG*Cx*100) %>% group_by(aged) %>% summarise(sum(x))

Deco = Deco %>% mutate(`r past` = Ra1*Cx,Survival = DS*Cx,Migration = MIG*Cx)

ggplot(Deco %>% pivot_longer(10:12) %>% mutate(name = factor(name,level= c("r past","Survival","Migration")))) +
  geom_hline(yintercept = 0,linetype=2) + 
  facet_wrap(~name) +
  geom_line(aes(x=Age,y=value,group = as.factor(aged),color = as.factor(aged)))+
  scale_color_manual("Truncation age",values = c("red","orange","darkgreen","purple"))+
  labs(y="Growth Rate with age component")+
  theme_bw() +
  theme(legend.position = "bottom") 
# ggsave("rates with age component (by truncation).png",width = 9,height = 4)
