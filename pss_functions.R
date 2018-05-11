#power calculation using a range of parameters: get power given N
powerChiP <- function(Nmin,Nmax,RRmin,RRmax,alpha,nratio,p1){
  N <- seq(Nmin,Nmax,50) # sample sizes
  RR <- seq(RRmin,RRmax,.1) # effect sizes
  
  out.pow <- data.frame(N=rep(N,each=length(RR)),RR=rep(RR,times=length(N))) %>% 
    mutate(alpha=alpha,
           p1=p1,
           p2=p1*RR,
           nratio=nratio,
           n1=floor(N/(nratio+1)),
           n2=ceiling(N-n1),
           power=round(as.numeric(bpower(p1=p1,p2=p2,n1=n1,n2=n2,alpha=alpha)),2))
  out.pow
}

#power calculation using a range of parameters: get N given power
powerChiN <- function(power,RR,alpha,nratiomin,nratiomax,p1min,p1max){
  nratio <- seq(nratiomin,nratiomax,.1) # ratio of exposed/unexposed
  p1 <- seq(p1min,p1max,.1) # prop. with outcome in unexposed
  
  out.N <- data.frame(nratio=rep(nratio,each=length(p1)),p1=rep(p1,times=length(nratio))) %>% 
    mutate(alpha=alpha,
           RR=RR,
           p2=p1*RR,
           power=power,
           fraction=1/(nratio+1))
  
  out.N$no <- 1:nrow(out.N)
  n1.n2 <- ddply(out.N,.(no), function(x) bsamsize(p1=x$p1,p2=x$p2,power=x$power,fraction=x$fraction,alpha=x$alpha))
  out.N <- out.N %>% mutate(n1=floor(n1.n2$n1), n2=ceiling(n1.n2$n2), N=n1+n2) #%>% 
  out.N[,c("nratio","p1","p2","alpha","RR","power","n1","n2","N")]
}