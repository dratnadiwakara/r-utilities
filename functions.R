
.repath <- function() {
  cat('Copy windows file path and hit RETURN')
  x <- scan(what = "")
  x <- readClipboard()
  xa <- gsub('\\\\', '/', x)
  writeClipboard(paste(xa))
  cat(xa)
}



printtable <- function(reg,column.labels="",note="",lines="") {
  if(column.labels=="") {
    stargazer(reg,type="text",no.space = TRUE,omit.stat = c("f","rsq","ser"),notes= note,dep.var.labels = "",dep.var.labels.include = FALSE,add.lines = lines)
  } else {
    stargazer(reg,type="text",no.space = TRUE,omit.stat = c("f","rsq","ser"),notes= note,column.labels = column.labels, dep.var.labels = "",dep.var.labels.include = FALSE,add.lines = lines)
  }
}

coef_plot_1reg_10ci <- function(reg,coef,omitted,labels=c("","","")) {
  coefs.df <- as.data.frame(reg$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg$coefficients[coef_range])
  se <- c(0,reg$cse[coef_range])
  
  se_lb <- coefs - se*1.68
  se_ub <- coefs + se*1.68
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  if(setequal(labels,c("","",""))){
    labs = xlabs
  } else {
    labs = labels
  }
  df <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df$coefs <- as.numeric(df$coefs)
  df$xlabs <- as.integer(df$xlabs)
  df$se_lb <- as.numeric(df$se_lb)
  df$se_ub <- as.numeric(df$se_ub)
  
  df <- df[order(df$xlabs),]
  
  gr<-  ggplot(df, aes(x=xlabs, y=coefs))+
    geom_errorbar(aes(ymin=se_lb, ymax=se_ub), width=0.2,color="dodgerblue4",alpha=0.5)+
    scale_x_continuous(breaks=xlabs,labels = labs)+geom_point(size=2,color="dodgerblue4")+geom_line(size=1,color="dodgerblue4")+
    geom_hline(yintercept = 0)+
    theme_minimal()+
    ylab(expression(beta[y]))+
    xlab("")+
    theme(legend.position="bottom",legend.title=element_blank(),panel.border = element_blank(), axis.line.x = element_line(color = 'gray80'),axis.text.x = element_text(angle = 90,hjust = 1))
  
  return(gr)
}


coef_plot_1reg <- function(reg,coef,omitted,labels=c("","","")) {
  coefs.df <- as.data.frame(reg$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg$coefficients[coef_range])
  se <- c(0,reg$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  if(setequal(labels,c("","",""))){
    labs = xlabs
  } else {
    labs = labels
  }
  df <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df$coefs <- as.numeric(df$coefs)
  df$xlabs <- as.integer(df$xlabs)
  df$se_lb <- as.numeric(df$se_lb)
  df$se_ub <- as.numeric(df$se_ub)
  
  df <- df[order(df$xlabs),]
  
  gr<-  ggplot(df, aes(x=xlabs, y=coefs))+
    geom_errorbar(aes(ymin=se_lb, ymax=se_ub), width=0.2,color="dodgerblue4",alpha=0.5)+
    scale_x_continuous(breaks=xlabs,labels = labs)+geom_point(size=2,color="dodgerblue4")+geom_line(size=1,color="dodgerblue4")+
    geom_hline(yintercept = 0)+
    theme_minimal()+
    ylab(expression(beta[y]))+
    xlab("")+
    theme(legend.position="bottom",legend.title=element_blank(),panel.border = element_blank(), axis.line.x = element_line(color = 'gray80'),axis.text.x = element_text(angle = 90,hjust = 1))
  
  return(gr)
}

coef_plot_2reg <- function(reg1,label1,reg2,label2,coef,omitted) {
  
  coefs.df <- as.data.frame(reg1$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg1$coefficients[coef_range])
  se <- c(0,reg1$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  df <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df$coefs <- as.numeric(df$coefs)
  df$xlabs <- as.integer(df$xlabs)
  df$se_lb <- as.numeric(df$se_lb)
  df$se_ub <- as.numeric(df$se_ub)
  
  df <- df[order(df$xlabs),]
  df['label'] <- label1
  
  
  
  coefs.df <- as.data.frame(reg2$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg2$coefficients[coef_range])
  se <- c(0,reg2$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  df2 <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df2$coefs <- as.numeric(df2$coefs)
  df2$xlabs <- as.integer(df2$xlabs)
  df2$se_lb <- as.numeric(df2$se_lb)
  df2$se_ub <- as.numeric(df2$se_ub)
  
  df2 <- df2[order(df2$xlabs),]
  df2['label'] <- label2
  
  df <- rbind(df,df2)
  
  
  
  gr <-  ggplot(df, aes(x=xlabs, y=coefs,group=label,color=factor(label)))+
    geom_errorbar(aes(ymin=se_lb, ymax=se_ub), alpha=0.5)+
    scale_x_continuous(breaks=xlabs)+geom_point(aes(shape=factor(label)),size=3)+geom_line(size=1)+
    geom_hline(yintercept = 0)+
    theme_minimal()+
    ylab(expression(beta[y]))+
    xlab("")+
    theme(panel.grid.minor.x = element_blank(),legend.position="bottom",legend.title=element_blank(),panel.border = element_blank(), axis.line.x = element_line(color = 'gray80'),axis.text.x = element_text(angle = 90,hjust = 1))+
    scale_fill_manual(values = c("dodgerblue1","dodgerblue4"),na.value="white")+scale_color_manual(values = c("dodgerblue1","dodgerblue4"),na.value="white")
  
  return(gr)
}



coef_plot_3reg <- function(reg1,label1,reg2,label2,reg3,label3,coef,omitted) {
  
  coefs.df <- as.data.frame(reg1$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg1$coefficients[coef_range])
  se <- c(0,reg1$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  df <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df$coefs <- as.numeric(df$coefs)
  df$xlabs <- as.integer(df$xlabs)
  df$se_lb <- as.numeric(df$se_lb)
  df$se_ub <- as.numeric(df$se_ub)
  
  df <- df[order(df$xlabs),]
  df['label'] <- label1
  
  
  
  coefs.df <- as.data.frame(reg2$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg2$coefficients[coef_range])
  se <- c(0,reg2$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  df2 <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df2$coefs <- as.numeric(df2$coefs)
  df2$xlabs <- as.integer(df2$xlabs)
  df2$se_lb <- as.numeric(df2$se_lb)
  df2$se_ub <- as.numeric(df2$se_ub)
  
  df2 <- df2[order(df2$xlabs),]
  df2['label'] <- label2
  
  df <- rbind(df,df2)
  
  coefs.df <- as.data.frame(reg3$coefficients)
  coef_range <- (min(which(substr(row.names(coefs.df),1,nchar(coef))==coef))):(max(which(substr(row.names(coefs.df),1,nchar(coef))==coef)))
  coefs <- c(0,reg3$coefficients[coef_range])
  se <- c(0,reg3$cse[coef_range])
  
  se_lb <- coefs - se*1.96
  se_ub <- coefs + se*1.96
  
  xlabs <- c(omitted,as.numeric(substr(row.names(coefs.df)[coef_range],nchar(coef)+1,nchar(coef)+100)))
  df2 <- as.data.frame(cbind(xlabs,coefs,se_lb,se_ub),stringsAsFactors = FALSE)
  df2$coefs <- as.numeric(df2$coefs)
  df2$xlabs <- as.integer(df2$xlabs)
  df2$se_lb <- as.numeric(df2$se_lb)
  df2$se_ub <- as.numeric(df2$se_ub)
  
  df2 <- df2[order(df2$xlabs),]
  df2['label'] <- label3
  
  df <- rbind(df,df2)
  
  
  
  gr <-  ggplot(df, aes(x=xlabs, y=coefs,group=label,color=factor(label)))+
    geom_errorbar(aes(ymin=se_lb, ymax=se_ub), alpha=0.5)+
    scale_x_continuous(breaks=xlabs)+geom_point(aes(shape=factor(label)),size=3)+geom_line(size=1)+
    geom_hline(yintercept = 0)+
    theme_minimal()+
    ylab(expression(beta[y]))+
    xlab("")+
    theme(panel.grid.minor.x = element_blank(),legend.position="bottom",legend.title=element_blank(),panel.border = element_blank(), axis.line.x = element_line(color = 'gray80'),axis.text.x = element_text(angle = 90,hjust = 1))+
    scale_fill_manual(values = c("dodgerblue1","dodgerblue3","dodgerblue4"),na.value="white")+scale_color_manual(values = c("dodgerblue1","dodgerblue3","dodgerblue4"),na.value="white")
  
  return(gr)
}


irr1 <- function(x, period = 1, starting.value = .1){
  
  ### This should detect the number of sign changes.  Should correctly not warn if there are many negative cash flows (so long as there is only 1 change in sign).
  
  irr.func <- function(r){ ( sum(x / (1 + r)^{0:(length(x)-1)}) )^2 }
  result <- optim(par = starting.value, fn = irr.func, method = "Brent", lower = -1000000, upper = 1000000)
  
  ## detecting number of sign changes
  x.ge.0 <- 1 * (x >= 0)
  changes <- diff(x.ge.0)
  changes <- changes * changes
  num.changes <- sum(changes)
  
  if( num.changes > 1) {
    
    statement <- "Your cash flows change more than once -- so you may have multiple IRRs. This function will only return the first IRR it finds. To find the others, you can try different starting values.  However, note the IRR does not make sense if the signs change more than once (try Modified IRR or NPV)."
    value <- period * result$par
    # return(list(beware = statement, IRR = value))
    return(value)
    
  } else {
    
    return(period * result$par)
    
  }
}

NPV <- function(C, r) {
  sum(C / (1 + r) ^ (seq(along = C) - 1))
}

irr2 <- function(C) {
  uniroot(NPV, c(0, 1), C = C)$root
}