
##########################################

#' Compute Morris (2008) "dppc2"
#' @description Computes "dppc2", that is Cohen's d estimate for pretest-posttest designs, using pooled pretest SD and correction for small samples, as suggested by Morris (2008)
#' @param cgN N of control group
#' @param cgMpre mean value for control group at pretest
#' @param cgMpost mean value for control group at posttest
#' @param cgSDpre standard deviation for control group at pretest
#' @param cgSDpost standard deviation for control group at posttest
#' @param tgN N of treated group
#' @param tgMpre mean value for treated group at pretest
#' @param tgMpost mean value for treated group at posttest
#' @param tgSDpre standard deviation for treated group at pretest
#' @param tgSDpost standard deviation for treated group at posttest
#' @param rho Assumed correlation for pretest-posttest scores, needed to compute variance
#' @return A list containing multiple outputs.
#' \describe{
#'   \item{eff}{The effect size estimate.}
#'   \item{vi}{The variance of the effect size estimate.}
#'   \item{vi.0}{The variance of the effect size fixing the effect to zero: needed for examination of publication bias via meta-regression, to make effect size and its variance independent.}
#' }
#' @export
dppc2 = function(cgN,cgMpre,cgMpost,cgSDpre,cgSDpost, tgN,tgMpre,tgMpost,tgSDpre,tgSDpost, rho=.5){
  diff = ((tgMpost-tgMpre)-(cgMpost-cgMpre))
  SDpre = sqrt(((tgN-1)*tgSDpre^2+(cgN-1)*cgSDpre^2)/(tgN+cgN-2))
  delta = diff/SDpre
  Cp = 1-3/(4*(tgN+cgN-2)-1)
  eff = Cp*delta
  vi = 2*Cp^2*(1-r)*((tgN+cgN)/(tgN*cgN))*((tgN+cgN-2)/(tgN+cgN-4))*(1+delta^2/(2*(1-rho)*((tgN+cgN)/(tgN*cgN))))-delta^2
  vi.0 = 2*Cp^2*(1-r)*((tgN+cgN)/(tgN*cgN))*((tgN+cgN-2)/(tgN+cgN-4))
  return(list(eff=eff,vi=vi,vi.0=vi.0))
}

##########################################

#' Cohen's d with Hedge's correction for small samples
#' @description Computes Cohen's d with Hedge's correction for small samples
#' @param cgN N of control group
#' @param cgM mean value for control group
#' @param cgSD standard deviation for control group
#' @param tgN N of treated group
#' @param tgM mean value for treated group
#' @param tgSD standard deviation for treated group
#' @return Cohen's d with Hedge's correction for small samples
#' @export
hg = function(cgN,cgM,cgSD, tgN,tgM,tgSD){
   Swith = sqrt(((cgN-1)*cgSDpre^2+(tgN-1)*tgSDpre^2)/(cgN+tgN-2))
   J = 1-3/(4*(tgN+cgN-2)-1)
   return(J*(cgMpre-tgMpre)/Swith)
}

##########################################

#' Combine across effect sizes / multiple outcomes
#' @description Combine across effect sizes / multiple outcomes using the strategy proposed by Borenstein et al. (2009, pp.226-229): unweighted average is used for point estimate, which is adequate assuming that very similar variances apply to all effect sizes within a cluster/study. This does a similar job as the aggregate.escalc function in "metafor" using argument weighted=FALSE, but this one may be more practical because its inputs are vectors of effects sizes and variances instead of an "escalc" object. 
#' @param eff vector of effect sizes to be combined
#' @param vi vector of variances of the effect sizes
#' @param rho assumed correlation across effect sizes (applies to all covariances)
#' @return list with combined effect size (mu) and its variance (v)
#' @export

combine = function(eff=c(),vi=c(),rho=.7){
  mu = mean(eff)
  if(length(vi)==1) v = vi
  if(length(vi)>1){
    Svi = vi
    for(i in 1:length(vi)){
      srvv = 0
      for(j in 1:length(vi)) if(i!=j) srvv = srvv + rho*sqrt(vi[i])*sqrt(vi[j])
      Svi[i] = Svi[i] + srvv
    }
    v = sum(Svi) * ((1/length(Svi))^2)
  }
  return(list(mu=mu,v=v))
}

##########################################

#' Enhanced funnel plot
#' @description Depicts funnel plot with colors and shapes for studies in multilevel meta-analysis, and possibly plot pet-peese meta-regression if required
#' @param fit a rma or rma.mv fitted object
#' @param petpeese either "petpeese" / "pet-peese" for full method, or "pet" or "peese" only, or empty/anything else for not using meta-regression
#' @param showStudies whether it should show the studies with different colors and shapes, default is TRUE (but only works if there are actually random effects)
#' @param xlab label to be used on the x axis of the funnel plot, if not specified label is "Effect size"
#' @param sizetext font size to be used in plot
#' @return funnel plot drawn with ggplot2
#' @export
funnelT = function(fit=NA,petpeese=NA,showStudies=T,xlab=NA,sizetext=20){
  
  # fix arguments issues
  if(is.na(petpeese)) petpeese = ""
  if(petpeese=="pet-peese") petpeese = "petpeese"
  if(is.na(sizetext)) sizetext = 20
  if(is.na(xlab)) xlab = "Effect size"
  if(is.na(showStudies)) showStudies = FALSE
  
  # Preliminarily calculate funnel
  b0 <- fit$beta
  se <- sqrt(fit$vi)
  se.seq <- seq(0.001, max(se), 0.01)
  eff.ll95 <- as.vector(b0) + qnorm(.025) * se.seq
  eff.ul95 <- as.vector(b0) + qnorm(.975) * se.seq
  dfp <- data.frame(se=rep(se.seq,2),eff=c(eff.ll95,eff.ul95))
  
  # preliminarily calculate other stuff
  yi = fit$yi
  if(!is.null(fit$random)){
    Study = strsplit(as.character(fit$random),"| ",fixed=T)[[1]][2]
    if(grepl("/",Study)) Study = substr(Study,1,gregexpr("/",Study)[[1]][1]-1)
  }else{Study=""}
  d = data.frame(yi=yi,vi=vi,se=se)
  if(Study!="" & showStudies==TRUE) d$study = as.factor(df[,Study])
  
  # Perform pet-peese meta-regression
  if(petpeese=="pet"|petpeese=="petpeese"){
    method = "pet"
    (pet = update(fit,mods=~se))
    regEff<-data.frame(se=seq(0,max(se),length=100),eff=NA)
    regEff$eff<-pet$beta[1]+pet$beta[2]*regEff$se
    regEst<-data.frame(b=pet$beta[1],lb=pet$ci.lb[1],ub=pet$ci.ub[1])
    pvalest<-pet$pval[1]; pvalreg<-pet$pval[2]
  }
  if(petpeese=="petpeese"){
    if(pet$pval[1]<.05&pet$beta[1]>0){
      method = "peese"
      peese = update(fit,mods=~vi)
      regEff<-data.frame(se=seq(0,max(se),length=100),eff=NA)
      regEff$eff<-peese$beta[1]+peese$beta[2]*regEff$se^2
      regEst<-data.frame(b=peese$beta[1],lb=peese$ci.lb[1],ub=peese$ci.ub[1])
      pvalest<-peese$pval[1]; pvalreg<-peese$pval[2]
    }
  }
  if(petpeese=="peese"){
    method = "peese"
    peese = update(fit,mods=~vi)
    regEff<-data.frame(se=seq(0,max(se),length=100),eff=NA)
    regEff$eff<-peese$beta[1]+peese$beta[2]*regEff$se^2
    regEst<-data.frame(b=peese$beta[1],lb=peese$ci.lb[1],ub=peese$ci.ub[1])
    pvalest<-peese$pval[1]; pvalreg<-peese$pval[2]
  }
  if(petpeese=="petpeese"){
    print(paste("Based on the pet-peese procedure, the method used was:",method),quote=F)
    print(paste("The p-value for the",method,"regression was p =",format(round(pvalreg,4),nsmall=4)),quote=F)
    print(paste("The bias-adjusted effect size was d = ",format(round(regEst$b,2),nsmall=2)," [95% CI: ",format(round(regEst$lb,2),nsmall=2),", ",format(round(regEst$ub,2),nsmall=2),"], p = ",format(round(pvalest,4),nsmall=4),sep=""),quote=F)
  }
  
  # Draw Funnel with Regression line using ggplot2
  if(is.null(d$study)){
    ggFunnel = ggplot()+theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank(),
                              panel.grid.minor.y=element_blank(),panel.grid = element_line(size=1.2))+
      geom_point(data=d,aes(x=se,y=yi),size=2.6,stroke=3)+
      geom_point(data=dfp,aes(x=se,y=eff),size=1.3,color="#888888")+
      geom_segment(aes(x=0, xend = max(se), y = b0, yend = b0),size=1)+
      theme(axis.text=element_text(size=sizetext),axis.title.x=element_text(size=sizetext),axis.title.y=element_text(size=sizetext*1.2))+
      xlab("Standard error")+ylab(xlab)+
      coord_flip()+scale_x_reverse(breaks=seq(0,2,.1))
    if(petpeese %in% c("petpeese","pet","peese")){
      ggFunnel = ggFunnel + geom_line(data=regEff,aes(x=se,y=eff),size=1)+
        geom_point(data=regEst,aes(x=0,y=b),size=3)+
        geom_errorbar(data=regEst,aes(x=0,ymin=lb,ymax=ub),width=.02,size=.8)+
        scale_y_continuous(breaks=seq(-10,10,.50),sec.axis=sec_axis(~.*1,name=paste("FUNNEL PLOT (with ",method,")",sep=""),breaks=seq(-10,10,.50)))
    }else{
      ggFunnel = ggFunnel + scale_y_continuous(breaks=seq(-10,10,.50),sec.axis=sec_axis(~.*1,name="FUNNEL PLOT",breaks=seq(-10,10,.50)))
    }
  }else{
    ggFunnel = ggplot()+theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank(),
                              panel.grid.minor.y=element_blank(),panel.grid = element_line(size=1.2))+
      geom_point(data=d,aes(x=se,y=eff,color=study,shape=study),size=2.6,stroke=3)+
      scale_shape_manual(values=rep(1:24,10)[1:length(levels(as.factor(d$study)))])+
      geom_point(data=dfp,aes(x=se,y=eff),size=1.3,color="#888888")+
      geom_segment(aes(x=0, xend = max(d$se), y = b0, yend = b0),size=1)+
      theme(axis.text=element_text(size=sizetext),axis.title.x=element_text(size=sizetext),axis.title.y=element_text(size=sizetext*1.2))+
      guides(shape=guide_legend(title="Study"),color=guide_legend(title="Study"))+
      theme(legend.text=element_text(size=sizetext/1.8),legend.title=element_text(size=sizetext/1.5))+
      xlab("Standard error")+ylab(xlab)+
      coord_flip()+scale_x_reverse(breaks=seq(0,2,.1))
    if(petpeese %in% c("petpeese","pet","peese")){
      ggFunnel = ggFunnel + geom_line(data=regEff,aes(x=se,y=eff),size=1)+
        geom_point(data=regEst,aes(x=0,y=b),size=3)+
        geom_errorbar(data=regEst,aes(x=0,ymin=lb,ymax=ub),width=.02,size=.8)+
        scale_y_continuous(breaks=seq(-10,10,.50),sec.axis=sec_axis(~.*1,name=paste("FUNNEL PLOT (with ",method,")",sep=""),breaks=seq(-10,10,.50)))
    }else{
      ggFunnel = ggFunnel + scale_y_continuous(breaks=seq(-10,10,.50),sec.axis=sec_axis(~.*1,name="FUNNEL PLOT",breaks=seq(-10,10,.50)))
    }
  }
  return(ggFunnel)
}

##########################################



