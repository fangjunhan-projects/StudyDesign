# This is a Shiny web application for futility analysis.
# date: 06/01/2023
# Jay Zhang and Fanni Zhang
# update: 
# 11/27/2023: incorporate different dropouts for control and treatment groups
# 02/06/2024: update alpha (one-side) tooltip - indicating "for FA" 
#             add target total event number option
#             update input summary accordingly
#             replace "prob" by full "probability"
# 03/04/2024: correct the ppcv calculation (Jay's email on 03/04/2024)
#             update related zcut code accordingly
# 03/14/2024: add include dropout checkbox
#             use same dropout input parameters as those in futility nph
#             extend pp lower boundary to 0.01
# 02/10/2025: update IFIA bounds from [0.1, 0.8] to [0, 1) 
# 03/06/2026: update HR lower boundary

library(shiny)
library(dplyr)
library(shinyhelper)
library(mvtnorm)
library(msm)

# source functions from Jay
rct.r = function(t,rpow,rct.T) (t<=rct.T)*(t/rct.T)^rpow+(t>rct.T)
rct.d = function(t,rpow,rct.T) (t<=rct.T)*rpow*(t/rct.T)^(rpow-1)/rct.T+0
cen.s = function(t,hzc) 1-pexp(t,hzc)
survv = function(t,hz) 1-pexp(t,hz)
atrsk = function(t,hz,hzc) survv(t,hz)*cen.s(t,hzc)
Atrsk = function(tt,hz,hzc,rpow,rct.T) {tt1=min(tt,rct.T);
integrate(function(t){rct.d(t,rpow,rct.T)*atrsk(tt-t,hz,hzc)},0,tt1)$value
}; Atrsk = Vectorize(Atrsk,"tt")
dmm = function(tt,hz,hzc,rpow,rct.T) hz*Atrsk(tt,hz,hzc,rpow,rct.T)

# Jay's functions that apply the same dropouts for both control and treatment groups
#dmmt= function(tt,hz0,hz1,hzc,rpow,rct.T,rw) {
#  rw[1]*dmm(tt,hz0,hzc,rpow,rct.T)+rw[2]*dmm(tt,hz1,hzc,rpow,rct.T)}
#cmmt= function(dcot,hz0,hz1,hzc,rpow,rct.T,rw) {
#  integrate(function(tt) dmmt(tt,hz0,hz1,hzc,rpow,rct.T,rw),0,dcot)$value}
#cMMt = Vectorize(cmmt,"dcot")
#Tdco=function(Mat.r,hz0,hz1,hzc,rpow,rct.T,rw) {uniroot(function(t) cMMt(t,hz0,hz1,hzc,rpow,rct.T,rw)-Mat.r,c(0,1e2))$root}

# Fanni updated the functions to incorporate different dropouts for control and treatment groups
dmmt= function(tt,hz0,hz1,hzc0,hzc1,rpow,rct.T,rw) {
  rw[1]*dmm(tt,hz0,hzc0,rpow,rct.T)+rw[2]*dmm(tt,hz1,hzc1,rpow,rct.T)}
cmmt= function(dcot,hz0,hz1,hzc0,hzc1,rpow,rct.T,rw) {
  integrate(function(tt) dmmt(tt,hz0,hz1,hzc0,hzc1,rpow,rct.T,rw),0,dcot)$value}
cMMt = Vectorize(cmmt,"dcot")
Tdco=function(Mat.r,hz0,hz1,hzc0,hzc1,rpow,rct.T,rw) {uniroot(function(t) cMMt(t,hz0,hz1,hzc0,hzc1,rpow,rct.T,rw)-Mat.r,c(0,1e2))$root}
Tdco.hz1=Vectorize(Tdco,"hz1")
pfstp=function(mm,hr,zcut,ifia,za,finf) { pmvnorm(c(zcut,za),Inf,-sqrt(mm*finf*c(ifia,1))*log(hr),
                                                  sigma=diag(1-sqrt(ifia),2)+sqrt(ifia))[1] } 
pfstp.hr  =Vectorize(pfstp,"hr")
pfnstp=function(mm,hr,zcut,ifia,za,finf)  pnorm(-sqrt(mm*finf)*log(hr)-za)
pfnstp.hr =Vectorize(pfnstp,"hr")
m.pow =function(mm,hr,zcut,ifia,za,finf) {pww=pfnstp(mm,hr,zcut,ifia,za,finf);
uniroot(function(x) pfstp(x,hr,zcut,ifia,za,finf)-pww,mm*c(1,9))$root  }
m.pow.hr =Vectorize(m.pow,"hr")  
pjfun=function(mm,hr,zcut,ifia,za,finf) {   
  sigm=diag(1-sqrt(ifia),2)+sqrt(ifia); mzs = -sqrt(mm*finf*c(ifia,1))*log(hr);
  c(pmvnorm(  -Inf,    c(zcut,za), mzs,sigma=sigm)[1],
    pmvnorm(c(-Inf,za),c(zcut,Inf),mzs,sigma=sigm)[1] ) } 
pjfun.hr=Vectorize(pjfun,"hr")

DRC.Template = function(mos, HR, cen.r0, cen.r1, alp, pow=NULL, ranr, N_DM, N_DM_v, 
                        rct.T, rpow, IA.delay, ifia, fMtc, stp.cut, format=0, nevent=NULL) {
  # Derived Variables
  ran.r<-c(1,ranr)
  os.m <-c(1,1/HR)*mos
  hzi  <-log(2)/os.m
  lhr  <-log(HR)
  hzc0  <- -log(1-cen.r0)/12
  hzc1  <- -log(1-cen.r1)/12
  rw<-ran.r/sum(ran.r)
  finf <-prod(rw)
  z.ab <-qnorm(c(1-alp,pow))
  za   <-z.ab[1]; zb <-z.ab[2]; zab <-sum(z.ab)
  lhri <-lhr*c(0,za,zab)/zab
  HRi  <-exp(lhri)
  if(!is.null(pow)){M<-(zab/lhr)^2/finf}
  if(!is.null(nevent)){M<-nevent}
  if(N_DM=="Sample Size") {N <-N_DM_v; mat.r <-M/N} else{mat.r <-N_DM_v; N <-M/mat.r}
  ifj <-c(ifia,1); ifi<-diff(c(0,ifj))
  
  if(fMtc==1){
    #zcut <- qnorm(stp.cut,za,sqrt(1/ifia))*sqrt(ifia)
    zcut <- qnorm(stp.cut,za,sqrt((1-ifia)/ifia))*sqrt(ifia) # correction Jay's email 03/04/2024
  }else if(fMtc==2){
    zcut <- qnorm(stp.cut,za,sqrt(1-ifia))*sqrt(ifia)
  }else if(fMtc==3){
    zcut<- -log(stp.cut)*sqrt(ifia*M*finf)
  }else if(fMtc==4){
    zcut<-stp.cut
  }
  #ppcv <- 1-pnorm(za,zcut/sqrt(ifia),sqrt(1/ifia))
  ppcv <- pnorm(zcut/sqrt(ifia),za,sqrt((1-ifia)/ifia)) # correction Jay's email 03/04/2024
  hr1cv<- exp(-zcut/sqrt(ifia*M*finf))
  #zcut.back<-qnorm(ppcv,za,sqrt(1/ifia))*sqrt(ifia) 
  zcut.back<-qnorm(ppcv,za,sqrt((1-ifia)/ifia))*sqrt(ifia) # updated 03/04/2024
  cpcv<-pnorm(zcut.back/sqrt(ifia),za,sqrt(1-ifia))
  
  # original code from Jay, but ppcvi and zcuti are never used
  #ppcvi<- ppcv+c(-.05,0,.05)
  #zcuti<- qnorm(ppcvi,za,sqrt(1/ifia))*sqrt(ifia)  # End of derived variables
  #zcuti<- qnorm(ppcvi,za,sqrt((1-ifia)/ifia))*sqrt(ifia)  # updated 03/04/2024
  
  Template=list()
  Template$rules=matrix(c(ifia,ppcv,cpcv,hr1cv,zcut.back),5,1)
  #Template$rules<-data.frame(Template$rules)
  if(format==1){
    Template$rules[2,1]<-paste0(sprintf('%.1f',as.numeric(Template$rules[2,1])*100),"%")
    Template$rules[3,1]<-paste0(sprintf('%.1f',as.numeric(Template$rules[3,1])*100),"%")
    Template$rules[c(1,4,5),1]<-round(c(ifia,hr1cv,zcut.back),3)
  }
  rownames(Template$rules)=c("Information Fraction:",
                             "Predictive Power Boundary:",
                             "Corresponding Conditional Power:",
                             "Corresponding HR Observation:",
                             "Corresponding Z-statistic:")
  colnames(Template$rules)="Values"
  
  Template$err.ind<-c(0,0) ### check issues for T.IA and T.FA
  Template$err.str<-c("No error","No error")
  
  Tdco.IA<-NULL
  T.IA<-NULL
  N.IA<-NULL
  try(Tdco.IA <- Tdco.hz1(mat.r*ifia,hzi[1],hzi[1]*HRi,hzc0,hzc1,rpow,rct.T,rw))
  if(is.null(Tdco.IA)){
    err.str.IA<-paste0("Given the specified input parameters, data will never reach the desired maturity at IA. ",
                       "Possible reasons including but not limited to: ",
                       "too long median OS, too high censoring rate, small sample size if 'Sample Size' selected. ",
                       "Please consider reasonable input values.")
    err.ind.IA<-1
    Template$err.ind[1]<-err.ind.IA
    Template$err.str[1]<-err.str.IA
  }else{  
    T.IA =Tdco.hz1(mat.r*ifia,hzi[1],hzi[1]*HRi,hzc0,hzc1,rpow,rct.T,rw)+IA.delay
    N.IA =rct.r(T.IA,rpow,rct.T)*N
  }
  
  T.FA<-NULL
  N.FA<-NULL
  try(T.FA <- Tdco.hz1(mat.r,hzi[1],hzi[1]*HRi,hzc0,hzc1,rpow,rct.T,rw))
  if(is.null(T.FA)){
    err.str.FA<-paste0("Given the specified input parameters, data will never reach the desired maturity at FA. ",
                       "Possible reasons including but not limited to: ",
                       "too long median OS/PFS, too high censoring rate, small sample size if 'Sample Size' selected. ",
                       "Please consider reasonable input values.")
    #stop(err.str.FA) ## 07062022
    err.ind.FA<-1
    Template$err.ind[2]<-err.ind.FA
    Template$err.str[2]<-err.str.FA
  }else{  
    T.FA =Tdco.hz1(mat.r,hzi[1],hzi[1]*HRi,hzc0,hzc1,rpow,rct.T,rw)
    N.FA =rct.r(T.FA,rpow,rct.T)*N
  }
  
  if(sum(Template$err.ind)==0){
    stppr=pjfun.hr(M,hr=HRi,zcut,ifia,za,finf); 
    Stppr=colSums(stppr)
    pw.Fut=pfstp.hr(M,HRi,zcut,ifia,za,finf)
    pw.nFut=pfnstp(M,HRi,zcut,ifia,za,finf)
    Minc =m.pow.hr(M,HRi[2:3],zcut,ifia,za,finf)
    Template$OCs =round(t(matrix(c(T.IA,N.IA,T.FA,N.FA, 
                                   Stppr*T.IA+(1-Stppr)*T.FA,  Stppr*N.IA+(1-Stppr)*N.FA, 
                                   pw.nFut, pw.Fut, c(NA,Minc-M), Stppr, stppr[1,], stppr[2,]),3)),4)
    rnames<-c("Time to Futility IA (months)","N Enrolled by IA","Time to Final Analysis (months)",
              "Total N Enrolled","Average Trial Duration (months)","Average Sample Size","Power without Futility",
              "Power with Futility","Additional Events to Recoup Power","Probability of Futility Stop",
              "Probability of Correct Futility Stop","Probability of Incorrect Futility Stop"); 
    #Template$OCs<-data.frame(Template$OCs)
    if(format==1){
      rows.d1<-which(rnames %in% c("Time to Futility IA (months)","Time to Final Analysis (months)",
                                   "Average Trial Duration (months)"))
      rows.d0<-which(rnames %in% c("N Enrolled by IA","Total N Enrolled","Average Sample Size","Additional Events to Recoup Power"))
      rows.pc<-which(rnames %in% c("Power without Futility","Power with Futility","Probability of Futility Stop",
                                   "Probability of Correct Futility Stop","Probability of Incorrect Futility Stop"))
      Template$OCs<-as.data.frame(apply(Template$OCs, 2, as.numeric))
      fmt.d1<-apply(Template$OCs[rows.d1,], 2, sprintf, fmt='%.1f')
      fmt.d0<-apply(Template$OCs[rows.d0,], 2, ceiling)
      fmt.pc<-apply(Template$OCs[rows.pc,]*100, 2, sprintf, fmt='%.1f')
      Template$OCs[rows.d1,]<-fmt.d1
      Template$OCs[rows.d0,]<-fmt.d0
      Template$OCs[rows.pc,]<-paste0(fmt.pc,"%")
    }
    rownames(Template$OCs)<-rnames
    #colnames(Template$OCs)=c("Null Hypothesis","Critical Value","Alternative Hypothesis")
    colnames(Template$OCs)<-c("Null: HR=1",paste0("Critical Value: HR=",round(HRi[2],3)),paste0("Alternative: HR=",HRi[3]))
  }
  
  return(Template)
}

# Define UI for application
ui <- fluidPage(
  #Application title
  titlePanel(title=div(img(height = 50,src="az_header.jpg"), 
                       h2("Futility IA in Trial Design")),"Futility"),
  navbarPage("",
             tabPanel("Futility IA",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h3("Fixed Sample Design:"),
                          
                          p( h4(em("Hypotheses and Assumptions")),
                             numericInput("mos", "Control Arm Median (months):", 12, min=0, max=120, step=3) %>%
                               helper(type = "inline",content = "Please set a reasonable median OS/PFS. Given other specified input parameters, too long median OS/PFS might result in data maturity issue."), 
                             numericInput("HR", "Alternative HR:", 0.65, min=0.2, max=0.999, step=0.05)  %>%
                               helper(type = "inline",content = "Please set a hazard ratio within [0.2, 1)."),
                             
                             checkboxGroupInput("dropind", "Include Subject Dropout", 
                                                choices = c("Yes"=1), 
                                                selected=""), 
                             conditionalPanel(
                               condition = ("input.dropind == 1"),
                               numericInput("droptime", "Dropout in the First (months):", 12, min=0, max=90, step=1),
                               
                               fluidRow(
                                 column( 6, numericInput("drop0", "% Control Dropout:", 5, min=0, max=50, step=5) %>%
                                           helper(type = "inline",content = "The non-informative dropout percentage for control arm. Given other specified input parameters, too high censoring rate might result in data maturity issue.")),
                                 column( 6, numericInput("drop1", "% Experimental Dropout:", 5, min=0, max=50, step=5) %>%
                                           helper(type = "inline",content = "The non-informative dropout percentage for experimental arm. Given other specified input parameters, too high censoring rate might result in data maturity issue."))
                               )
                             ),
                             
                          ),
                          
                          p( h4(em("Statistical Design Parameters")),
                             fluidRow(
                               column( 6, numericInput("ran.r", "Randomization Ratio:", 1, min =0.5, max =3, step =0.05)  %>%
                                 helper(type = "inline",content = "Please set a randomization ratio within [0.5, 3]"),),
                               column( 6, numericInput("alp", "Alpha (One-sided):", 0.025, min=0.005, max=0.10, step=0.001)  %>%
                                         helper(type = "inline",content = "Please set an alpha value within (0, 0.1] for FA."), )
                               
                             ),

                             
                             radioButtons("dcotype", "DCO Determination", 
                                                choices = c("Power"="pow","Total Number of Events"="dcoevent"), 
                                                selected="pow"), 
                             conditionalPanel(
                               condition = ("input.dcotype == 'pow'"),
                               numericInput("pow", "Power:", 0.90, min=0.3, max=0.999, step=0.001)  %>%
                                 helper(type = "inline",content = "Please set a power value within [0.3, 0.999].")
                             ),
                             conditionalPanel(
                               condition = ("input.dcotype == 'dcoevent'"),
                               numericInput("dcoevent", "Total Number of Events:", 200, min =1, max =1000, step =1)  %>%
                                helper(type = "inline",content = "Please set a reasonable target total number of events to achieve the desired power given FSD input parameters.")
                             ),


                             
                             fluidRow(
                               column(width = 6, radioButtons("N_DM", "Select One:", c("Sample Size","Data Maturity"), ) ),
                               column(width = 6, offset = 0, 
                                      conditionalPanel( condition = "input.N_DM == 'Sample Size'",
                                                        numericInput("N_DM_v_ss", "Value:",  500, min=0, max=1000, step=100, )  %>%
                                                          helper(type = "inline",content = "Please set a reasonable sample size. Given other specified input parameters, small sample size might result in data maturity issue.")),
                                      conditionalPanel( condition = "input.N_DM == 'Data Maturity'",
                                                        numericInput("N_DM_v_dm", "Value:", 0.65, min=0, max=1, step=0.05, ) %>%
                                                          helper(type = "inline",content = "Please set a reasonable maturity value depending on the specified input parameters."))
                                      
                               )   
                             )
                          ),
                          
                          p( h4(em("Operational Variables")),
                             fluidRow(
                               column( 6, numericInput("rct.T", "Accrual Period (months):", 18, min=6, max=120, step=3) %>%
                                         helper(type = "inline",content = "Please set an accrual time within [6, 120].")),
                               column( 6, numericInput("rpow",  "Accrual Power Parameter:", 1.5, min=1, max=5, step=0.5)  %>%
                                         helper(type = "inline",content = "Please set an accrual power within [1, 5].")), 
                               br()
                             ),
                             numericInput("IA.delay", "Time from DCO to futility decision (months):", 2, min=0, max=6, step=1) %>%
                               helper(type = "inline",content = "Please set an IA delay time within [0, 6].")
                          ),
                          
                          tags$hr(),
                          h3("IA Futility Rules"),
                          p( numericInput("ifia", "Information Fraction:", 0.35, min=0.1, max=0.999, step=0.05)  %>%
                               helper(type = "inline",content = "Please set an information fraction within [0.1, 1)."),
                             h4(em("Stopping Metric and Boundary")),
                             fluidRow( 
                               column(6,radioButtons("fMtc", "Choose a Metric:",
                                                     choices = list("Predictive Probability"=1, "Conditional Probability"=2, "Hazard Ratio"=3, "IA Z-statistic"=4),
                                                     selected = 1 ) ),
                               column(6,
                                      conditionalPanel( condition = "input.fMtc == 1",
                                                        numericInput("stp.cut_pp", "Stopping Boundary:", 0.3, min=0.01, max=.90, step=0.05)  %>%
                                                          helper(type = "inline",content = "Please set a predictive power stopping boundary within [0.01, 0.9].")),
                                      conditionalPanel( condition = "input.fMtc == 2",
                                                        numericInput("stp.cut_cp", "Stopping Boundary:", 0.2, min=0.01, max=.90, step=0.05)  %>%
                                                          helper(type = "inline",content = "Please set a conditional power stopping boundary within [0.01, 0.9].")),
                                      conditionalPanel( condition = "input.fMtc == 3",
                                                        numericInput("stp.cut_hr", "Stopping Boundary:", 1, min=0.5, max=1.5, step=0.05)  %>%
                                                          helper(type = "inline",content = "Please set a hazard ratio stopping boundary within [0.5, 1.5].")),
                                      conditionalPanel( condition = "input.fMtc == 4",
                                                        numericInput("stp.cut_z", "Stopping Boundary:", 0.7, min=0, max=1.65, step=0.05) %>%
                                                          helper(type = "inline",content = "Please set a z-statistic stopping boundary within [0, 1.65]."))
                               )
                             )   
                          )
                        ),
                        
                        mainPanel(
                          #tabsetPanel(
                          #tabPanel("Table",
                          # br(),
                         
                          
                          p(strong("Input Summary")),
                          wellPanel(
                            uiOutput("text")
                          ),
                          p(strong("Rules")),
                          wellPanel(
                            tableOutput("rules")
                          ),
                          p(strong("DRC Template")),
                          wellPanel(
                            div(style = 'overflow-x: scroll', tableOutput("table")),
                            downloadButton("download.table", "Download table")
                          )
                          #)
                          #)
                        )
                      )
                      
                      
                      
             ),
             tabPanel("User Guide",
                      uiOutput("help")  
             )      
  )
)

server <- function(input, output,session) {
  
  observe_helpers(withMathJax = TRUE)
  
  input.update<-reactive({
    ## validation for input parameters
    validate(
      need(input$HR>=0.2 && input$HR<1, "Warning: Please set a hazard ratio within [0.2, 1).")
    )
    validate(
      need(input$drop0>=0 && input$drop0<=50, "Warning: Please set a dropout percentage within [0, 50] for control arm.")
    )
    validate(
      need(input$drop1>=0 && input$drop1<=50, "Warning: Please set a dropout percentage within [0, 50] for treatment arm.")
    )
    validate(
      need(input$alp>0 && input$alp<=0.1, "Warning: Alpha should be set within (0, 0.1] for FA.")
    )
    validate(
      need(input$pow>=0.3 && input$pow<=0.999, "Warning: Power should be set within [0.3, 0.999].")
    )
    validate(
      need(input$rct.T>=6 && input$rct.T<=120, "Warning: Please set an accrual time within [6, 120].")
    )
    validate(
      need(input$rpow>=1 && input$rpow<=5, "Warning: Please set an accrual power parameter within [1, 5].")
    )
    validate(
      need(input$IA.delay>=0 && input$IA.delay<=6, "Warning: Please set an IA delay time within [0, 6].")
    )
    validate(
      need(input$ifia>=0.1 && input$ifia<1, "Warning: Please set an information fraction within [0.1, 1).")
    )
    validate(
      need(input$stp.cut_pp>=0.01 && input$stp.cut_pp<=0.9, "Warning: Please set a predictive power within [0.01, 0.90].")
    )
    validate(
      need(input$stp.cut_cp>=0.01 && input$stp.cut_cp<=0.9, "Warning: Please set a conditional power within [0.01, 0.90].")
    )
    validate(
      need(input$stp.cut_hr>=0.5 && input$stp.cut_hr<=1.5, "Warning: Please set a hazard ratio stopping boundary within [0.5, 1.5].")
    )
    validate(
      need(input$stp.cut_z>0 && input$stp.cut_z<=1.65, "Warning: Please set a HR stopping boundary within (0, 1.65].")
    )
    
    input.par<-list()
    input.par$mos<-input$mos
    input.par$HR<-input$HR
    
    
    input.par$dropind<-0
    if (is.null(input$dropind)){
      input.par$droptime<-1000
      input.par$drop0<-0.0001
      input.par$drop1<-0.0001
    }else{
      validate(
        need(input$drop0>=0 && input$drop0<=50, "Warning: Please set a dropout percentage within [0, 50].")
      )
      validate(
        need(input$drop1>=0 && input$drop1<=50, "Warning: Please set a dropout percentage within [0, 50].")
      )
      input.par$dropind<-1
      input.par$droptime<-input$droptime
      input.par$drop0<-input$drop0/100
      input.par$drop1<-input$drop1/100
    }
    
    # input.par$cen.r0<-input$cen.r0/100
    # input.par$cen.r1<-input$cen.r1/100
    # if(input$cen.r0==0){
    #   input.par$cen.r0<-0.00001
    # }
    # if(input$cen.r1==0){
    #   input.par$cen.r1<-0.00001
    # }
    input.par$alp<-input$alp
    input.par$pow<-input$pow
    input.par$ran.r<-input$ran.r
    input.par$N_DM<-input$N_DM
    input.par$N_DM_v<-input$N_DM_v_dm
    
    finf<-prod(c(1,input.par$ran.r)/sum(c(1,input.par$ran.r)))
    
    if(input$dcotype=="pow"){
      input.par$pow<-input$pow
      M<-(sum(qnorm(c(1-input.par$alp,input.par$pow)))/log(input.par$HR))^2/finf
    }
    if(input$dcotype=="dcoevent"){
      M<-input$dcoevent
      input.par$pow<-pnorm(-sqrt(M*finf)*log(input.par$HR)-qnorm(1-input.par$alp))
    }
    
    z.ab <-qnorm(c(1-input$alp,input$pow))
    za   <-z.ab[1]; zb <-z.ab[2]; zab <-sum(z.ab)
    input.par$HRi  <-exp(log(input$HR)*c(0,za,zab)/zab)
    
    if(input.par$N_DM=="Sample Size"){
      input.par$N_DM_v<-input$N_DM_v_ss
      input.par$ss<-input$N_DM_v_ss
    }else if(input.par$N_DM=="Data Maturity"){
      input.par$ss<-M/input$N_DM_v_dm
    }
    input.par$rct.T<-input$rct.T
    input.par$rpow<-input$rpow
    input.par$IA.delay<-input$IA.delay
    input.par$ifia<-input$ifia
    input.par$fMtc<-input$fMtc

    if(input.par$fMtc==1){
      input.par$stp.cut<-input$stp.cut_pp
      #zcut <- qnorm(input.par$stp.cut,za,sqrt(1/input.par$ifia))*sqrt(input.par$ifia)
      zcut <- qnorm(input.par$stp.cut,za,sqrt((1-input.par$ifia)/input.par$ifia))*sqrt(input.par$ifia) # correction Jay's email 03/04/2024
    }else if(input.par$fMtc==2){
      input.par$stp.cut<-input$stp.cut_cp
      zcut <- qnorm(input.par$stp.cut,za,sqrt(1-input.par$ifia))*sqrt(input.par$ifia)
    }else if(input.par$fMtc==3){
      input.par$stp.cut<-input$stp.cut_hr
      zcut<- -log(input.par$stp.cut)*sqrt(input.par$ifia*M*finf)
    }else if(input.par$fMtc==4){
      input.par$stp.cut<-input$stp.cut_z
      zcut<-input.par$stp.cut
    }
    #ppcv <- 1-pnorm(za,zcut/sqrt(input.par$ifia),sqrt(1/input.par$ifia)) 
    ppcv <- pnorm(zcut/sqrt(input.par$ifia),za,sqrt((1-input.par$ifia)/input.par$ifia)) # correction Jay's email 03/04/2024
    
    input.par$M<-M
    input.par$ppcv<-ppcv
    input.par$hr1cv<- exp(-zcut/sqrt(input.par$ifia*M*finf))
    #input.par$zcut.back<-qnorm(ppcv,za,sqrt(1/input.par$ifia))*sqrt(input.par$ifia)
    input.par$zcut.back<-qnorm(ppcv,za,sqrt((1-input.par$ifia)/input.par$ifia))*sqrt(input.par$ifia) # updated 03/04/2024
    input.par$cpcv<-pnorm(input.par$zcut.back/sqrt(input.par$ifia),za,sqrt(1-input.par$ifia))
    
    return(input.par)
  })
  
  tableDRC<-reactive({
    
    input.par<-input.update()
    out.DRC<-DRC.Template(mos=input.par$mos, 
                          HR=input.par$HR, 
                          cen.r0=input.par$drop0, 
                          cen.r1=input.par$drop1,
                          alp=input.par$alp, 
                          pow=input.par$pow,
                          ranr=input.par$ran.r,
                          N_DM=input.par$N_DM, 
                          N_DM_v=input.par$N_DM_v, 
                          rct.T=input.par$rct.T, 
                          rpow=input.par$rpow, 
                          IA.delay=input.par$IA.delay, 
                          ifia=input.par$ifia, 
                          fMtc=input.par$fMtc, 
                          stp.cut=input.par$stp.cut,
                          format=1)
    return(out.DRC)
    
  })
  #Text Template: 800 patients recruited, ratio nE/nC=1, 20 months accrual (uniform accrual, k=1/non-uniform accrual, k=1.5). 
  #Control median=3 months (lambda=0.23). Experimental median=4 months (lambda=0.17). Exponential survival function. 
  #Control/Experimental dropout: 5% of subjects (in absence of events) would drop out per year in both arms 
  #using exponential dropout rate. 
  #HR(Experimental:Control)=0.75, critical HR value=0.81, alpha(1-sided)=2.5%, power=80%. 
  #2 months from DCO to IA, futility IA at 35% information fraction using a stopping boundary of HR=1.
  
  output$text <- renderText({
    input.par<-input.update()
    ppcv <- input.par$ppcv
    str1<-paste0("The study is designed to test the hypothesized HR=",round(input.par$HRi[3],3),
                 " with a significance level of alpha(1-side)=",input.par$alp," and a power of ",round(input.par$pow*100,1),"%. <br/>")
    hazrate.c<-round(log(2)/input.par$mos,4)
    hazrate.e<-round(log(2)/input.par$mos*input.par$HR,4)
    mos.e<-round(log(2)/(log(2)/input.par$mos*input.par$HR),1)
    accr.str<-paste0(input.par$rct.T, " months, ")
    if(input.par$rct.T==1){accr.str<-"one month, "}
    if(input.par$dropind==1){
      drop.str<-paste0("The non-informative exponential dropout is assumed to occur with ",input.par$drop0*100,
                       "% in control group and ",input.par$drop1*100, "% in experimental group by ",input.par$droptime," months. ")
    }else{
      drop.str<-""
    }
    str2<-HTML(paste0("<br/> Over a period of ",accr.str, ceiling(input.par$ss)," patients will be recruited in a ",
                      input.par$ran.r,":1 ratio of experimental to control groups. ","Cumulative accruals will follow a power function ",
                      "F(t)=(t/AccrualPeriod)^k where k=",input.par$rpow,". The expected control median =",input.par$mos,
                      " months, corresponding to a hazard rate ","&lambda;","=",hazrate.c,". The median of experimental group =",mos.e,
                      " months with ","&lambda;","=",hazrate.e," based on the hypothesized HR=",round(input.par$HRi[3],3),
                      ". ", drop.str, "Based on the above design, the estimated HR critical value =",round(input.par$HRi[2],3), 
                      " and the estimated number of events =", ceiling(input.par$M),". <br/>"
               ))
    str3<-paste0("<br/> The futility analysis will be executed at ",input.par$ifia*100,
                 "% information time and to stop the study if a predictive probability <",round(ppcv,3)*100,
                 "%.  Otherwise, the study will continue up to the final analysis. <br/>")
    str<-paste0(str1,str2,str3)
    HTML(str)
  })
  output$rules <- renderTable({
    out<-tableDRC()$rules
    return(out)
    
  },rownames = TRUE,colnames=F)
  
  output$table <- renderTable({
    table<-tableDRC()
    err.check<-tableDRC()$err.ind
    if(sum(err.check)==0){
      out<-table$OCs
    }else{
      err.print<-table$err.str[which(err.check==1)]
      out<-err.print
      print(out)
    }
    return(out)
    
  },rownames = TRUE)
  
  output$download.table <- downloadHandler(
    filename = function() {
      paste('DRC_output_input', '.csv', sep='')
    },
    content = function(file) {
      input.par<-input.update()
      out<-tableDRC()$OCs
      colnames(out)<-gsub(" ","",colnames(out))
      colnames(out)<-gsub(":|=","_",colnames(out))
      
      input.info<-c("Control Arm Median","Alternative HR","Censoring Rate per Year (control)","Censoring Rate per Year (treatment)",
                    "Alpha (One-sided)", "Power","Randomization Ratio","Sample Size","Data Maturity",
                    "Accrual Period (months)", "Accrual Power Parameter", "Time from DCO to IA (months)",
                    "Information Fraction","Predictive Probability","Conditional Probability","Hazard Ratio","IA Z-statistics")
      #input.cnames<-c("Input","Input Parameter","Value")
      input.cnames<-c("Input","Value")
      input.table<-matrix(NA,nrow=length(input.info),ncol=length(input.cnames))
      colnames(input.table)<-input.cnames
      input.table[,1]<-input.info
      input.table[,2]<-c(input.par$mos,input.par$HR,input.par$drop0,input.par$drop1,
                         input.par$alp,input.par$pow,input.par$ran.r,input.par$ss,round(input.par$M/input.par$ss,3),
                         input.par$rct.T,input.par$rpow,input.par$IA.delay,input.par$ifia,
                         paste0(sprintf('%.1f',as.numeric(input.par$ppcv)*100),"%"),
                         paste0(sprintf('%.1f',as.numeric(input.par$cpcv)*100),"%"),
                         round(input.par$hr1cv,3),round(input.par$zcut.back,3))

      input.table<-rbind(colnames(input.table), input.table)
      out.table<-cbind(Output=rownames(out), data.frame(out,row.names=NULL))
      out.table<-rbind(colnames(out.table), out.table)
      
      dl.table<-matrix("",nrow=nrow(out.table)+nrow(input.table)+1,ncol=max(ncol(out.table),ncol(input.table)))
      dl.table<-data.frame(dl.table)
      dl.table[1:nrow(out.table),1:ncol(out.table)]<-out.table
      dl.table[(nrow(out.table)+2):nrow(dl.table),1:ncol(input.table)]<-input.table
      
      colnames(dl.table)<-c("Output/Input", rep("",ncol(dl.table)-1))
      write.csv(dl.table,file,row.names=F,quote=F)
    }
  )
  
  output$help<-renderUI({
    #https://gist.github.com/aagarw30/d5aa49864674aaf74951
    tags$iframe(style="height: 750px; width:100%; scrolling=yes", src="FutilityUserGuide.pdf")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
