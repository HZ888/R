library(shiny)
library(lavaan)
library(blavaan)
library(fmsb)
library(rmarkdown)
library(knitr)
library(xtable)
library(shinyWidgets)
library(rsconnect)
library(runjags)
library(rjags)
library(psych)

server <- function(input, output, session) 
{
  myData <- reactive(
  {infile <- input$file1
  if (is.null(infile))
    return(NULL)
  
  dat <- read.csv(infile$datapath, header = TRUE)
  })

noProc <- reactive(
  {
    noProc<-3
  })

#selectedCol<-match(c(input$independents),names(myData()))

x<-reactive({dim(myData())[1]-1})

output$dimensionval<-renderPrint(
  {
    if (is.null(myData())) return()
    paste("Sample size:",{x()})
  }
  )

output$dimension<-renderText(
  {
    if (is.null(myData())) return ()
    if(x()>=50 & x()<= 150)
    {
      "We are happy to help!"
    }
    else
    {
      "Please email money to tibor.schuster@mcgill.ca"
    }
  }
)

output$moneyface<-renderText(
  {
    if (is.null(myData())) return ()
    if (x()<50 | x()> 150) return('The sample size is out of the range for free service.')
  }
)
  
output$CA<-renderPrint({
  if (is.null(myData())) return ()
  if (is.null(input$independents)) return ()
  indi<-match(c(input$independents),names(myData()))
  alpha(as.data.frame(myData())[,indi])[[1]][1]
}) 

#output$CAA<-renderPrint({
#  indi<-match(c(input$independents),names(myData()))
#  library(ltm)
#  cronbach.alpha(data[,indi], CI = TRUE, B = 500)$alpha
#}) 

url<-a(" documentation",href="https://www.github.com/HZ888/Bayesian-Questionnaire-Validation/")
output$tab<-renderUI({
  tagList("URL link:", url)
})

output$independents <- renderUI(
  {
    if (is.null(myData()))
      return (NULL)
    if (x()>=50 & x()<=150)
    {
    dat<-myData()
    items<-names(dat)
    selectInput('independents',"Step 2: Select variables to include in the Factor Analyses (note the number of items needs to be less than five in the free version):", items, multiple = TRUE)
    }
    else return ()
  })


n <- reactive({
  length(c(input$independents))
})

output$numexpert<-renderText({
  if (is.null(myData())) return (NULL)
  if (is.null(input$independents)) return()
  input$nexpert
  })

output$finetuningpriora<-renderUI(
  {
    if (is.null(myData())) return ()
    if (is.null(input$independents)) return()
    numprior<-n()
    lapply(1:numprior,function(i)
    {
      sliderInput(paste0("ShapeOneforVar",i), paste0("ParameterforVar",i),
                  min = 1, max = input$nexpert+2,
                  value = 1)
    })
  })

heightprior<-reactive(
  {
    if (is.null(myData())) return ()
    if (is.null(input$independents)) return()
    if (is.null(input$nexpert)) return ()
    return (320*input$nexpert)
})

output$finetuningpriora2<-renderUI(
    {
      if (is.null(myData())) return ()
      if (is.null(input$independents)) return()
      numprior <- n()
      numexp <- input$nexpert
      lapply(1:numexp,function(j)
        {
        lapply(1:numprior,function(i)
        {
          sliderInput(paste0("Var",j,i), paste0("Expert ",j," Var",i),
                      min = 1, max = input$nexpert+2,
                      value = 1)
        })
      })
    })

output$priorplot <- renderPlot(
  {
    if (is.null(myData()))
      return (NULL)
    if (is.null(input$independents))
      return (NULL)
    for (k in 1:n())
    {
      if (is.null(input[[paste0("ShapeOneforVar",k)]]))
        return (NULL)
    }
    
    par(mfrow=c(1,as.numeric(n())))
    
    for (j in 1:n())
    {
      valShapeOne<- paste0("ShapeOneforVar", j)
      a <- as.numeric(input[[valShapeOne]])
      #valShapeTwo<- paste0("ShapeTwoforVar", j)
      #b <- as.numeric(input[[valShapeTwo]])
      priordata<-matrix(NA,nrow=length(seq(0,1,0.01)),ncol=2)
      priordata[,1]<-seq(0,1,0.01)
      priordata[,2]<-dbeta(seq(0,1,0.01),a,input$nexpert+2-a)
      priordata<-data.frame(priordata)
      names(priordata)[1]<-"Prior"
      names(priordata)[2]<-"FL"
      ylabdensity<-c("density(likelihood)",rep("",j))
      plot(priordata$Prior,priordata$FL,
           xlab=c(input$independents)[j],ylab=ylabdensity[j],
           lty=1,lwd=2,
           cex.axis=2,cex.lab=1.8,
           type = "n")
      lines(priordata[,1],priordata[,2])
    }
  })

#rowsplot<-function(){return (400*input$nexpert)}

output$priorplot2 <- renderPlot(
  {
    if (is.null(myData()))      return (NULL)
    if (is.null(input$independents))      return (NULL)
    for (j in 1:input$nexpert){
    for (i in 1:n())
    {
      if (is.null(input[[paste0("Var",j,i)]]))        return (NULL)
    }
    }
    
    par(mfrow=c(input$nexpert,as.numeric(n())))
    
    for (j in 1:input$nexpert){
    for (i in 1:n())
    {
      valShapeOne<- paste0("Var",j,i)
      a <- as.numeric(input[[valShapeOne]])
      b <- input$nexpert+2-a
      priordataindiv<-matrix(NA,nrow=length(seq(0,1,0.01)),ncol=2)
      priordataindiv[,1]<-seq(0,1,0.01)
      priordataindiv[,2]<-dbeta(seq(0,1,0.01),a,input$nexpert+2-a)
      priordataindiv<-data.frame(priordataindiv)
      names(priordataindiv)[1]<-"Prior"
      names(priordataindiv)[2]<-"FL"
      ylabdensity<-c(paste("Expert",j,"density(likelihood)"),rep("",j*i))
      plot(priordataindiv$Prior,priordataindiv$FL,
           xlab=c(input$independents)[i],ylab=ylabdensity[i],
           lty=1,lwd=2,
           cex.axis=2,cex.lab=1.8,
           type = "n")
      lines(priordataindiv[,1],priordataindiv[,2])
    }
    }
  },height = function() heightprior())

output$ui.action <- renderUI(
  {
    if (is.null(myData())) return()
    if (is.null(input$independents)) return()
    actionButton("action", "Run CFA")
  })

betabb <- reactive(
  {
    if (is.null(myData()))
      return (NULL)
    if (is.null(input$independents))
      return (NULL)
    for (k in 1:n())
    {
      if (is.null(input[[paste0("ShapeOneforVar",k)]]))
        return (NULL)
    }
    
    priordata<-matrix(NA,nrow=n(),ncol=2)
    
    for (j in 1:n())
    {
      valShapeOne<- paste0("ShapeOneforVar", j)
      a <- as.numeric(input[[valShapeOne]])
      b <-input$nexpert+2-a 
      priordata[j,1]<-a
      priordata[j,2]<-b
    }
    priordata<-data.frame(priordata)
  })

############################################################################
#Frequentist Confirmatory Factor Analyses model        
CFAFreqModel <- reactive (
  {
    if (is.null(input$action)) return ()
    if (input$action==0) return ()
    isolate (
      {
        if (is.null(myData())) return (NULL)
        variables <- paste(input$independents, collapse = '+')
        m<-'model=~'
        CFA <- cfa(paste(m,variables,sep=''),data=myData(),
                   std.lv=TRUE,missing = "fiml")
        CFA
      })
  })

#Frequentist approach, shown in the report
CFAModelsum <- reactive(
  {
    CFA<-CFAFreqModel()
    return(summary(CFA,standardized=TRUE,fit.measures=TRUE))
  })

#Bayesian CFA model. This is what is shown in the report.
bCFAFreqModel <- reactive (
  {
    if (is.null(input$action)) return ()
    if (input$action==0) return ()
    isolate (
      {
        if (is.null(myData()))
          return (NULL)
        
        m<-'model=~'
        num <- n()
        table<-data.frame(lapply(1:num, function(i) 
        {
          input[[paste0("Variable", i)]]
        }))
        
        numprior <- n()
        priors<-list()
        for (j in 1:numprior)
        {
          valShapeOne<- paste0("ShapeOneforVar", j)
          a <- as.numeric(input[[valShapeOne]])
          b <-input$nexpert+2-a 
          priors[[j]]<-paste("prior(\"dbeta(",a,",", b, ")\")")
        }
        
        vnameprior<-NULL
        variablename <- c(input$independents)
        for (i in 1:n())
        {
          vnamepriornew <- paste(priors[[i]],"*",variablename[i])
          vnameprior <- c(vnameprior,vnamepriornew)
        }
        variables<-paste(vnameprior,collapse = "+")
        bCFA <- bcfa(paste(m,variables,sep=''),data=myData(),
                     bcontrol =list(method="rjparallel"),burnin = 100,sample=50)
        bCFA
      })
  })

#This is for generating the report.
bCFAModelsum <- reactive(
  {
    bCFA<-bCFAFreqModel()
    return(summary(bCFA,standardized=TRUE,fit.measures=TRUE))
  })

#we generate confidence intervals for both the frequentist and bayesian approach estimation. This is what is shown on the shiny screen.   
Tableloadings <- reactive ({
  if (is.null(myData()))
    return (NULL)
  if (is.null(input$independents))
    return (NULL)
  rd <-2 
  dataa<-myData()
  m<-'model=~'
  variables <- paste(input$independents, collapse = '+')
  CFA <- cfa(paste(m,variables,sep=''),data=dataa,
             std.lv=TRUE,missing = "fiml")
  coutput<-standardizedSolution(CFA)[1:n(),]
  CFAestimates<-coutput[,c(4,8,9)]
  
  #new method
  idc<-inspect(CFA,what="std")$lambda
  data<-dataa
  set.seed(123)
  idc.boot<-matrix(NA,length(idc),10)
  i<-1
  while(i <= 10)
  {
    boot.dat<-data[sample(1:nrow(data),nrow(data),T),]
    fit.boot <- suppressWarnings(cfa(paste(m,variables,sep=''), data=boot.dat))
    boot.lambda<-suppressWarnings(inspect(fit.boot,what="std")$lambda)
    # making sure that no implausible estimes occur:
    if(length(boot.lambda[which(boot.lambda!=0)])==n()&sum(boot.lambda[which(boot.lambda!=0)]>=1)==0)
    {idc.boot[,i]<-boot.lambda[which(boot.lambda!=0)]
    i<-i+1  
    }
  }
  z.mat<-log((1+idc.boot)/(1-idc.boot))*0.5
  means.z<-apply(z.mat,1,mean)
  SEs.z<-apply(z.mat,1,sd)
  r.values<-seq(0,0.99,by=0.01)
  z.values<-log((1+r.values)/(1-r.values))*0.5
  store.val3<-matrix(NA,ncol = 3,nrow=n())
  for(i in 1:n())
  {
    valShapeOne<- paste0("ShapeOneforVar", i)
    a <- as.numeric(input[[valShapeOne]])
    b <- input$nexpert+2-a
    
    posterior<-dbeta(r.values,a,b)*dnorm(z.values,means.z[i],SEs.z[i])
    posterior<-posterior/sum(posterior)
    lo<-r.values[which(cumsum(posterior)>0.025)][1]
    mm<-r.values[which(cumsum(posterior)>0.5)][1]
    up<-r.values[which(cumsum(posterior)>0.975)][1]
    store.val3[i,]<-cbind(mm,lo,up)
  }

  ##################################################### prior loadings  
  ##################################################### bayesian estimates
  m<-'model=~'
  num <- n()
  table<-data.frame(lapply(1:num, function(i) 
  {
    input[[paste0("Variable", i)]]
  }))
  
  priors<-list()
  for (j in 1:n())
  {
    vala<- paste0("ShapeOneforVar", j)
    a <- as.numeric(input[[vala]])
    b <- input$nexpert+2-a
    priors[[j]]<-paste("prior(\"dbeta(",a,",", b, ")\")")
  }
  
  vnameprior<-NULL
  variablename <- c(input$independents)
  for (i in 1:n())
  {
    vnamepriornew <- paste(priors[[i]],"*",variablename[i])
    vnameprior <- c(vnameprior,vnamepriornew)
  }
  variables<-paste(vnameprior,collapse = "+")
  fit3 <- bcfa(paste(m,variables,sep=''),data=myData(),
               burnin = 100,sample=50)
  pt <- parTable(fit3)
  b3output<-standardizedSolution(fit3, se=pt$se[pt$free > 0])[1:n(),]
  BCFAestimates<-b3output[,c(4,6,7)]
  loadings <- round(cbind(CFAestimates,store.val3,BCFAestimates),rd)
  colnames(loadings)[1] <- c("CFA")
  colnames(loadings)[2] <- c("2.5%")
  colnames(loadings)[3] <- c("97.5%")
  colnames(loadings)[4] <- c("bootstrapBCFA")
  colnames(loadings)[5] <- c("2.5%")
  colnames(loadings)[6] <- c("97.5%")
  colnames(loadings)[7] <- c("BCFA")
  colnames(loadings)[8] <- c("2.5%")
  colnames(loadings)[9] <- c("97.5%")
  rownames(loadings) <- c(input$independents)
  loadings
})

# modify the format of the FLs table 
formatted.Tableloadings <- reactive({
  table <- Tableloadings()
  nrow <- n()
  newtable <- matrix (NA, nrow = nrow, ncol = 3)
  for (k in 1:nrow){
    newtable[k,] <- c(table[k,1],table[k,4],table[k,7])
  }
  #rownames(newtable)<- c(input$independents)
  newtable2 <- matrix (NA, nrow = nrow, ncol = 3)
  for (j in 1:nrow){
    newtable2[j,1] <- paste("(", table[j,2], ",", table[j,3], ")")
    newtable2[j,2] <- paste("(", table[j,5], ",", table[j,6], ")")
    newtable2[j,3] <- paste("(", table[j,8], ",", table[j,9], ")")
  }
  finaltable<-rbind(newtable[1,],newtable2[1,])
  
  for (rows in 2:nrow){
    finaltable<-rbind(finaltable,newtable[rows,],newtable2[rows,])
  }
  colnames(finaltable)<- c("FrequntistCFA","BootstrapBayesianCFA","BayesianCFA")
  name<-c(input$independents)
  rownames(finaltable)<- rep(name,each=2)
  finaltable
})

output$plot <- renderPlot({
  if (is.null(input$action)) return ()
  if (input$action==0) return ()
  isolate({
    if (is.null(myData()))
      return (NULL)
    Table <- Tableloadings()
    noItems <- n()
    plot(1,1,xlim=c(0.75,noItems+0.25),ylim=c(0,1),type="n",xaxt="n",
         xlab="Items Selected", ylab = "Item-domain correlation",cex.lab=1.5)
    abline(h=c(0,0.2,0.4,0.6,0.8,1),lty="dotted")
    axis(1,at=1:noItems,labels=T)
    noProc <- 3
    for(i in 1:noProc)
    {
      arrows((1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(2+(i-1)*3)],(1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(3+(i-1)*3)],length = 0.05,angle=90,code=3,col=i,lwd=5)
      points((1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(1+(i-1)*3)],pch=i+15,col=i,cex=1.5)
    }
    legend("bottomleft", legend=c("FrequentistCFA","BootstrapBCFA","BayesianCFA"), col=1:3,lwd=1, lty=c(1,1,1,1), pch=c(1,2,3,4)+15)
    title(sub="")
  })
})

output$FactorLoadings <- renderPrint({
  if (is.null(input$action)) return ()
  if (input$action==0) return ()
  isolate({
    if (is.null(myData()))
      return (NULL)
    formatted.Tableloadings()
  })
})
###########################################################################condition individuals
#we generate confidence intervals for both the frequentist and bayesian approach estimation. This is what is shown on the shiny screen.   
Tableloadings2 <- reactive ({
  if (is.null(myData()))
    return (NULL)
  if (is.null(input$independents))
    return (NULL)
  rd <-2 
  dataa<-myData()
  m<-'model=~'
  variables <- paste(input$independents, collapse = '+')
  CFA <- cfa(paste(m,variables,sep=''),data=dataa,
             std.lv=TRUE,missing = "fiml")
  coutput<-standardizedSolution(CFA)[1:n(),]
  CFAestimates<-coutput[,c(4,8,9)]
  
  #new method
  idc<-inspect(CFA,what="std")$lambda
  data<-dataa
  set.seed(123)
  idc.boot<-matrix(NA,length(idc),10)
  i<-1
  while(i <= 10)
  {
    boot.dat<-data[sample(1:nrow(data),nrow(data),T),]
    fit.boot <- suppressWarnings(cfa(paste(m,variables,sep=''), data=boot.dat))
    boot.lambda<-suppressWarnings(inspect(fit.boot,what="std")$lambda)
    # making sure that no implausible estimes occur:
    if(length(boot.lambda[which(boot.lambda!=0)])==n()&sum(boot.lambda[which(boot.lambda!=0)]>=1)==0)
    {idc.boot[,i]<-boot.lambda[which(boot.lambda!=0)]
    i<-i+1  
    }
  }
  z.mat<-log((1+idc.boot)/(1-idc.boot))*0.5
  means.z<-apply(z.mat,1,mean)
  SEs.z<-apply(z.mat,1,sd)
  r.values<-seq(0,0.99,by=0.01)
  z.values<-log((1+r.values)/(1-r.values))*0.5
  store.val3<-matrix(NA,ncol = 3,nrow=n())
  
  astore<-matrix(NA,nrow=1,ncol = input$nexpert)
  for(i in 1:n())
  {
    for(j in 1:input$nexpert){
      valShapeOne<- paste0("Var",j, i)
      astore[1,j] <- as.numeric(input[[valShapeOne]])
    }
    a <- mean(astore[1,])
    b <- input$nexpert+2-a
    
    posterior<-dbeta(r.values,a,b)*dnorm(z.values,means.z[i],SEs.z[i])
    posterior<-posterior/sum(posterior)
    lo<-r.values[which(cumsum(posterior)>0.025)][1]
    mm<-r.values[which(cumsum(posterior)>0.5)][1]
    up<-r.values[which(cumsum(posterior)>0.975)][1]
    store.val3[i,]<-cbind(mm,lo,up)
  }
  
  ##################################################### prior loadings  
  ##################################################### bayesian estimates
  m<-'model=~'
  num <- n()
  table<-data.frame(lapply(1:num, function(i) 
  {
    input[[paste0("Variable", i)]]
  }))
  
  priors<-list()
  for (kk in 1:n())
  {
    for (j in 1:input$nexpert)
    {
      valshapeone<-paste0("Var",j,kk)
      astore[1,j]<-as.numeric(input[[valshapeone]])
    }
    a<-mean(astore[1,])
    
    b <- input$nexpert+2-a
    priors[[kk]]<-paste("prior(\"dbeta(",a,",", b, ")\")")
  }
  
  vnameprior<-NULL
  variablename <- c(input$independents)
  for (i in 1:n())
  {
    vnamepriornew <- paste(priors[[i]],"*",variablename[i])
    vnameprior <- c(vnameprior,vnamepriornew)
  }
  variables<-paste(vnameprior,collapse = "+")
  fit3 <- bcfa(paste(m,variables,sep=''),data=myData(),
               burnin = 100,sample=50)
  pt <- parTable(fit3)
  b3output<-standardizedSolution(fit3, se=pt$se[pt$free > 0])[1:n(),]
  BCFAestimates<-b3output[,c(4,6,7)]
  loadings <- round(cbind(CFAestimates,store.val3,BCFAestimates),rd)
  colnames(loadings)[1] <- c("CFA")
  colnames(loadings)[2] <- c("2.5%")
  colnames(loadings)[3] <- c("97.5%")
  colnames(loadings)[4] <- c("bootstrapBCFA")
  colnames(loadings)[5] <- c("2.5%")
  colnames(loadings)[6] <- c("97.5%")
  colnames(loadings)[7] <- c("BCFA")
  colnames(loadings)[8] <- c("2.5%")
  colnames(loadings)[9] <- c("97.5%")
  rownames(loadings) <- c(input$independents)
  loadings
})

# modify the format of the FLs table 
formatted.Tableloadings2 <- reactive({
  table <- Tableloadings2()
  nrow <- n()
  newtable <- matrix (NA, nrow = nrow, ncol = 3)
  for (k in 1:nrow){
    newtable[k,] <- c(table[k,1],table[k,4],table[k,7])
  }
  #rownames(newtable)<- c(input$independents)
  newtable2 <- matrix (NA, nrow = nrow, ncol = 3)
  for (j in 1:nrow){
    newtable2[j,1] <- paste("(", table[j,2], ",", table[j,3], ")")
    newtable2[j,2] <- paste("(", table[j,5], ",", table[j,6], ")")
    newtable2[j,3] <- paste("(", table[j,8], ",", table[j,9], ")")
  }
  finaltable<-rbind(newtable[1,],newtable2[1,])
  
  for (rows in 2:nrow){
    finaltable<-rbind(finaltable,newtable[rows,],newtable2[rows,])
  }
  colnames(finaltable)<- c("FrequntistCFA","BootstrapBayesianCFA","BayesianCFA")
  name<-c(input$independents)
  rownames(finaltable)<- rep(name,each=2)
  finaltable
})

output$plot2 <- renderPlot({
  if (is.null(input$action)) return ()
  if (input$action==0) return ()
  isolate({
    if (is.null(myData()))
      return (NULL)
    Table <- Tableloadings2()
    noItems <- n()
    plot(1,1,xlim=c(0.75,noItems+0.25),ylim=c(0,1),type="n",xaxt="n",
         xlab="Items Selected", ylab = "Item-domain correlation",cex.lab=1.5)
    abline(h=c(0,0.2,0.4,0.6,0.8,1),lty="dotted")
    axis(1,at=1:noItems,labels=T)
    noProc <- 3
    for(i in 1:noProc)
    {
      arrows((1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(2+(i-1)*3)],(1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(3+(i-1)*3)],length = 0.05,angle=90,code=3,col=i,lwd=5)
      points((1:noItems)-0.25+0.5/(noProc+1)*i,Table[,(1+(i-1)*3)],pch=i+15,col=i,cex=1.5)
    }
    legend("bottomleft", legend=c("FrequentistCFA","BootstrapBCFA","BayesianCFA"), col=1:3,lwd=1, lty=c(1,1,1,1), pch=c(1,2,3,4)+15)
    title(sub="")
  })
})

output$FactorLoadings2 <- renderPrint({
  if (is.null(input$action)) return ()
  if (input$action==0) return ()
  isolate({
    if (is.null(myData()))
      return (NULL)
    formatted.Tableloadings2()
  })
})



############################################################################

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('my-report', '.docx', sep="")
  },
  content = function(file) {
    src <- normalizePath('report.Rmd')
    owd <-setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd',overwrite = TRUE)
    out <- render(input = 'report.Rmd')
    file.rename(out, file)  
  }
)
}