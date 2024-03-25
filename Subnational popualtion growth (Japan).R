library(tidyverse)
options(scipen = 999)

aged = 25
yeard = 10 # Default: a2-a1, but you can change it to any number
latest = 2020
y1 = latest-yeard
Pop = c()
RES =c()
for (z in sprintf("%02d", 0:47)) {
  for (g in c("Female","Male")) {
    
    Names = colnames(read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/",z,"/STATS/Population.txt"), header = TRUE, fill = TRUE))[1]
    Names = strsplit(Names,split = "[.]")[[1]][2]
    Names = ifelse(is.na(Names),"Japan",Names)
    
    mx = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/",z,"/STATS/Mx_1x1.txt"), header = TRUE, fill = TRUE, skip = 1)
    mx = pivot_longer(mx,3:5,names_to = "Sex",values_to = "mx")
    mx = mx %>% filter(Sex==g)
    mx$mx = as.numeric(as.character(mx$mx))
    mx$mx = ifelse(is.na(mx$mx),0,mx$mx)
    P = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/",z,"/STATS/Population.txt"), header = TRUE, fill = TRUE, skip = 1)
    P = pivot_longer(P,3:5,names_to = "Sex",values_to = "PopS")
    P = P %>% filter(Sex==g)
    P$PopS = ifelse(P$PopS==0,1,P$PopS)
    tem = P %>% mutate(State = Names)
    Pop = rbind(Pop,tem)
    
    pop1 <- P %>% filter(Year == y1+yeard) %>% summarise(sum(PopS)) %>% as.numeric()
    pop2 <- P %>% filter(Year == y1) %>% summarise(sum(PopS)) %>% as.numeric()
    R = log(pop1/pop2)/yeard
    
    # select a country, insert starting year (y1), ending year (y2) and starting age (a1), ending age (a2), the difference of year(yeard)
    Ra1 <-  c()
    Ra2 <-  c()
    DS <- c()
    MIG <- c()
    for (AGE in c(0:(100-aged))) {
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
    result<-data.frame(State = Names, Sex = g,Age = aged:100,Ra2,Ra1,DS,MIG)
    
    
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
    
    Result<-rbind(data.frame(State = Names, Sex = g,Age = 0:(aged-1),Ra2,Ra1,DS,MIG),result)
    
    
    #### Cx ####
    pop <- P %>% filter(Year == y1) %>% filter(Age %in% c(0:100)) %>% pull(PopS)
    pop2 <- pop * exp(Result$Ra2*0.5*yeard)
    Cx = pop2/sum(pop2)
    
    Result = cbind(Result,Cx)
    
    R - sum(Result$Cx * Result$Ra2)

    RES = rbind(RES,Result)
  }
}

# male
pop1 <- Pop %>% filter(Year == y1,Sex == "Male",State=="Japan") %>% summarise(pop= sum(PopS)) %>% as.numeric()
pop2 <- Pop %>% filter(Year == y1+yeard,Sex == "Male",State=="Japan") %>% summarise(pop= sum(PopS))%>% as.numeric()
R = log(pop2/pop1)/yeard
popJPN <- pop1 * exp(R*0.5*yeard)
pop1 <- Pop %>% filter(Year == y1,Sex == "Male",State!="Japan") %>% group_by(State) %>% summarise(pop= sum(PopS)) 
pop2 <- Pop %>% filter(Year == y1+yeard,Sex == "Male",State!="Japan") %>% group_by(State) %>% summarise(pop= sum(PopS)) 
R = log(pop2$pop/pop1$pop)/yeard
popstate <- pop1$pop * exp(R*0.5*yeard)
Cx.JPN = popstate/popJPN
Cx.JPN = data.frame(Sex = "Male",State = pop1$State,Cx.JPN)
# female
pop1 <- Pop %>% filter(Year == y1,Sex == "Female",State=="Japan") %>% summarise(pop= sum(PopS)) %>% as.numeric()
pop2 <- Pop %>% filter(Year == y1+yeard,Sex == "Female",State=="Japan") %>% summarise(pop= sum(PopS))%>% as.numeric()
R = log(pop2/pop1)/yeard
popJPN <- pop1 * exp(R*0.5*yeard)
pop1 <- Pop %>% filter(Year == y1,Sex == "Female",State!="Japan") %>% group_by(State) %>% summarise(pop= sum(PopS)) 
pop2 <- Pop %>% filter(Year == y1+yeard,Sex == "Female",State!="Japan") %>% group_by(State) %>% summarise(pop= sum(PopS)) 
R = log(pop2$pop/pop1$pop)/yeard
popstate <- pop1$pop * exp(R*0.5*yeard)
Cx.JPN.f = popstate/popJPN
Cx.JPN.f = data.frame(Sex = "Female",State = pop1$State,Cx.JPN=Cx.JPN.f)
Cx.JPN = rbind(Cx.JPN,Cx.JPN.f)
#
RES = left_join(RES,Cx.JPN)
RES$Cx.JPN = ifelse(is.na(RES$Cx.JPN),1,RES$Cx.JPN)

tem = RES %>% group_by(State,Sex) %>% summarise(r_bar= sum(Ra2*Cx), r.past_bar = sum(Ra1*Cx), survival_bar= sum(DS*Cx),mig_bar = sum(MIG*Cx)) %>% View()
RES %>% group_by(State,Sex) %>% summarise(r_bar= sum(Ra2*Cx*Cx.JPN)*1000, r.past_bar = sum(Ra1*Cx*Cx.JPN)*1000, survival_bar= sum(DS*Cx*Cx.JPN)*1000,mig_bar = sum(MIG*Cx*Cx.JPN)*1000) %>% View()

ggplot(RES %>% filter(Sex=="Female"))  + facet_wrap(~State,scales = "free_y") +
  geom_hline(yintercept = 0,linetype=2)+
  geom_line(aes(Age,Ra2,color = "Ra2")) + 
  geom_line(aes(Age,Ra1,color = "Ra1"))+
  geom_line(aes(Age,DS,color = "DS"))+
  geom_line(aes(Age,MIG,color = "MIG"))+
  labs(y="Growth Rate")+
  theme_bw()+
  scale_colour_manual("",values = c("black","red","blue","orange"),breaks = c("Ra2","Ra1","DS","MIG"),labels = c("r","r-past","Survival","Migration"))
ggsave("rates.png",width = 12,height = 6)

ggplot(RES %>% filter(Sex=="Female")) + facet_wrap(~State) +
  geom_hline(yintercept = 0,linetype=2)+
  geom_line(aes(Age,Ra2*Cx,color = "Ra2")) + 
  geom_line(aes(Age,Ra1*Cx,color = "Ra1"))+
  geom_line(aes(Age,DS*Cx,color = "DS"))+
  geom_line(aes(Age,MIG*Cx,color = "MIG"))+
  labs(y="Growth Rate with age component")+
  theme_bw()+
  scale_colour_manual("",values = c("black","red","blue","orange"),breaks = c("Ra2","Ra1","DS","MIG"),labels = c("r","r-past","Survival","Migration"))
ggsave("rates with age component.png",width = 10,height = 6)


ggplot(RES %>% filter(Sex=="Female"))  + facet_wrap(~State) +
  geom_hline(yintercept = 0,linetype=2)+
  geom_line(aes(Age,Ra2*Cx*Cx.JPN,color = "Ra2")) + 
  geom_line(aes(Age,Ra1*Cx*Cx.JPN,color = "Ra1"))+
  geom_line(aes(Age,DS*Cx*Cx.JPN,color = "DS"))+
  geom_line(aes(Age,MIG*Cx*Cx.JPN,color = "MIG"))+
  labs(y="Growth Rate with age and population component")+
  theme_bw()+
  scale_colour_manual("",values = c("black","red","blue","orange"),breaks = c("Ra2","Ra1","DS","MIG"),labels = c("r","r-past","Survival","Migration"))
ggsave("rates with age component and population component.png",width = 10,height = 6)


RES %>%filter(Sex=="Female")%>% mutate(x= Ra2*Cx*Cx.JPN*100) %>% group_by(State) %>% summarise(sum(x))

RES %>%filter(Sex=="Female")%>% mutate(x= Ra1*Cx*100) %>% group_by(State) %>% summarise(sum(x))
RES %>%filter(Sex=="Female")%>% mutate(x= Ra1*Cx*Cx.JPN*100) %>% group_by(State) %>% summarise(sum(x))
RES %>%filter(Sex=="Female")%>% mutate(x= DS*Cx*100) %>% group_by(State) %>% summarise(sum(x))
RES %>%filter(Sex=="Female")%>% mutate(x= DS*Cx*Cx.JPN*100) %>% group_by(State) %>% summarise(sum(x))
RES %>%filter(Sex=="Female")%>% mutate(x= MIG*Cx*100) %>% group_by(State) %>% summarise(sum(x))
RES %>%filter(Sex=="Female")%>% mutate(x= MIG*Cx*Cx.JPN*100) %>% group_by(State) %>% summarise(sum(x))

write_csv(RES,"RES.csv")
