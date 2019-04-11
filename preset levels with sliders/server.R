##VERSION 7 SWITCHES TO A GAMMA DISTRIBUTION FOR BETWEEN SEASON VARIABILITY
#Version 7A switches to autoregressive noise, where the observation noise slider controls innovations
#and season shock adds additional variations every 12 months

##TO ESTIMATE AR and noise components from real data: ar.test<- ar(test, aic = TRUE, order.max = 1)

#Clear workspace to get rid of old junk
rm(list=ls(all=TRUE))
library(shiny)
library(RColorBrewer)
library(lme4)
library(plotly)
# Define server logic required to draw a histogram

shinyServer(function(input, output, clientData, session) {

###################PART OF THE PROGRAM THAT RERUNS WHEN SLIDERS CHANGE
output$ui.pct.decline<- renderUI({
    if (is.null(input$setting))
      return()

 ###BASE INPUT IN THIS VERSION ON THE SID DATA FROM COLORADO FOR 0-1 year OLD CHILDREN

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
      "IPD (any serotype)" = 
 	  sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = -30.15) %>% sliderValue(live=TRUE, delayed=TRUE),
	"Pneumococcal pneumonia (any serotype)"=
	   sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = -13.3) %>% sliderValue(live=TRUE, delayed=TRUE),
	"All cause pneumonia"=
	   sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = 0.13) %>% sliderValue(live=TRUE, delayed=TRUE)
			#actual value for this is 0.13
		)
	})	

output$ui.season.amp<- renderUI({
  if (is.null(input$setting)  )
      return()
  
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
       sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.54) %>% sliderValue(live=TRUE, delayed=TRUE),
		"Pneumococcal pneumonia (any serotype)"=
	  sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.91) %>% sliderValue(live=TRUE, delayed=TRUE),
	"All cause pneumonia"=
	 sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.86) %>% sliderValue(live=TRUE, delayed=TRUE)
	)
})	

output$ui.n.cases<- renderUI({
      if (is.null(input$setting))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
	 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(1.12)),
	"Pneumococcal pneumonia (any serotype)"=
 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(0.93) ),
	"All cause pneumonia"=
 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(4.64)	)
	)
	})	

output$ui.rand.noise<- renderUI({
      if (is.null(input$setting))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.33),
	"Pneumococcal pneumonia (any serotype)"=
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.25),
	"All cause pneumonia"=
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.23)
	)
	})	

##########################
#######################

liveInput = reactiveValues(
  secular.trend.yr=NA,
  pct.decline.yr=NA,
  season.amp=NA
)

delayedInput = reactiveValues(
  secular.trend.yr=NA,
  pct.decline.yr=NA,
  season.amp=NA
)

observe({
  liveInput$secular.trend.yr <- input$secular.trend.yr$live
  delayedInput$secular.trend.yr <- input$secular.trend.yr$delayed
})

observe({
  liveInput$secular.trend.yr <- input$secular.trend.yr$live
  delayedInput$secular.trend.yr <- input$secular.trend.yr$delayed
  liveInput$pct.decline.yr <- input$pct.decline.yr$live
  delayedInput$pct.decline.yr <- input$pct.decline.yr$delayed
  liveInput$season.amp <- input$season.amp$live
  delayedInput$season.amp <- input$season.amp$delayed
})

overview.data <- function(years.pre, years.post, trend.secular, trend.vax, season.amp, mean.cases) {
  irr.secular <- 1 + (trend.secular / 100)
  irr.vax <- 1 + (trend.vax / 100)

  df = data.frame(t=1:(12 * (years.pre + years.post)))
  vax.change <- df$t - years.pre * 12
  vax.change [ vax.change < 0 ] <- 0
  season.component <- season.amp * cos(2 * pi * df$t / 12 + pi)
  constant.component <- log(irr.secular) / 12 * df$t + log(irr.vax)/12 * vax.change
  
  constant.exp = exp(constant.component)
  
  beta0 <- log(mean.cases) - (mean(constant.component) + mean(season.component))
  
  df$cases.min <- constant.exp / exp(season.amp) * exp(beta0)
  df$cases.max <- constant.exp * exp(season.amp) * exp(beta0)
  df$cases <- constant.exp * exp(season.component) * exp(beta0)
  
  return(df)
}

overview <- reactive({
  return(overview.data(input$years.pre.pcv.data, input$years.post.pcv.data, 
                       delayedInput$secular.trend.yr, delayedInput$pct.decline.yr, 
                       delayedInput$season.amp, input$mean.cases.n))
})

analysis <- reactive({
  n.year.post.PCV <- 		input$years.post.pcv.data      #how many years post-pcv data?
  n.year.pre.PCV  <-		input$years.pre.pcv.data     #how many years pre-PCV data?
  n.years.fill <-           	n.year.pre.PCV + n.year.post.PCV 
  year.introduce.vax.fill.vec<- n.years.fill -n.year.post.PCV  #What year is vaccine introduced?
  irr.yr.vax                 <-  1+(delayedInput$pct.decline.yr/100)
  irr.sec.trend.yr			<- 1 + (delayedInput$secular.trend.yr/100)
  vax.change.coeff.fill.vec <-   irr.yr.vax #What is the IRR per year?
  std.dev.year.fill.vec <-       input$year.noise #Variation between years
  mean.cases.vec<-               input$mean.cases.n  #What is the intercept (cases/month)?
  obs.noise.fill.vec <-          input$observation.noise #How much random noise (note there is also poisson observation noise added to the data separately?
  
  n.reps<-input$sim.n
  
  coeffs.end<- matrix(ncol=1, nrow=n.reps) #Create empty data frame
  
  
  #CHECKBOXES
  if (input$model.season) include.seasonality <- 1 else include.seasonality <- 0
  if (input$model.secular.trend) include.secular.trend <-1 else include.secular.trend <-0      
  
  sim.function <-function(n.years.fill2,year.introduce.vax.fill, vax.change.coeff.fill,mean.cases, obs.noise.fill)
  {
    
    #monthly data for 10 years
    n.years<- n.years.fill2
    
    #Year vaccine introduced
    year.introduce.vax <-year.introduce.vax.fill
    
    #coeff for vaccine change per month is ln(IRR)/12, so a 10%/year decline is ln(0.9)/12, 30%/year would be ln ln(0.7)/12  try 0.8
    vax.change.coeff=log(vax.change.coeff.fill)/12
    
    #Standard deviation in mean between years...try 0.2
    #std.dev.year<-std.dev.year.fill
    
    #Observation noise...
    obs.noise=obs.noise.fill
    
    #Vector of monthly values
    t <- 1:(12*n.years)
    
    #Seasonal variation
    season.component <-  delayedInput$season.amp * cos(2*3.14159*t/12+3.14159)
    
    #Year to year variation in mean levels, centered at mean with a certain stddev
    year.index=1:n.years
    #year.noise <- rgamma(n.years, shape=std.dev.year, scale = 0.15)
    #year.noise.tab<-cbind.data.frame(year.noise, year.index)
    
    #merge in yearly noise with seasonal variations
    d <- cbind.data.frame(t,season.component)
    d$year <-ceiling(t/12)
    
    d2<-d
    #d2<-merge(d,year.noise.tab, by.x="year", by.y="year.index")
    
    #Random observation-level noise
    d2$obs.noise <-rnorm(nrow(d2),mean=0, obs.noise)
    
    #Autoregressive observation-level noise
    #e.start <-rnorm(25,sd=obs.noise.fill.vec)
    #e = rnorm(length(t),sd=obs.noise.fill.vec)  
    ##e[c(1,13,25,37,49,61,73,85,97,109,121,133,145, 157)]<-rnorm(14,sd=std.dev.year)  #Every 12 months, have a shock
    #d2$ar.noise <- as.numeric(arima.sim(model=list(ar=0.1), n=length(t), innov=e,start.innov=e.start))
    
    #vaccine introduced in year x and can be either a change in mean or changslope
    change.month<-(year.introduce.vax*12+1)
    #print(change.month)
    d2$meanchng<-0
    d2$meanchng[(year.introduce.vax*12+1):(nrow(d2)) ]  <-1
    
    d2$changslope<-d2$t -(year.introduce.vax*12)
    d2$changslope[ d2$changslope<0 ]<- 0
    
    d2$sin12.model <-include.seasonality*sin(2*3.14159*d2$t/12)
    d2$cos12.model <-include.seasonality*cos(2*3.14159*d2$t/12)
    
    secular.trend.model <-t*include.secular.trend 			
    
    #GENERATE THE SIMULATED TIME SERIES
    d2$exp.case.mean.no.int<-exp( season.component + log(irr.sec.trend.yr)/12*t +  d2$obs.noise + d2$changslope*vax.change.coeff)
    beta0 <- log(mean.cases) - mean(log(d2$exp.case.mean.no.int)) #calculate intercept as function of mena cases and model components
    d2$exp.case.mean <- exp(beta0)* d2$exp.case.mean.no.int #full model with intercept
    d2$exp.case <-rpois(length(t),d2$exp.case.mean)  #The observed number of cases is drawn from some true mean
    d2$TID <-as.factor(t)
    
    plot.df<-cbind.data.frame(	d2$t,d2$exp.case)
    
    #FIT MODEL
    glmm1 <-glmer(exp.case ~  secular.trend.model + sin12.model +  cos12.model + changslope + (1|TID )  , data=d2, family = poisson, nAGQ = 0, control = glmerControl(check.rankX = "stop.deficient"), silent = TRUE)
    summary.glmm1<-summary(glmm1)
    
    coef.cp<-as.data.frame(summary.glmm1$coefficients)["changslope",1]
    list.results<-list(summary.glmm1,coef.cp,plot.df)
    return(list.results)
  }
  
  irr.end<-NA
  est.pct.change.vax <-NA
  est.vax.change.irr <-NA
  
  sim.data <-data.frame(simn=numeric(),exp.case=numeric() )
  for (i in 1:n.reps){
    func<-sim.function(n.years.fill2=n.years.fill, year.introduce.vax.fill=year.introduce.vax.fill.vec, 
                       vax.change.coeff.fill=vax.change.coeff.fill.vec,
                       mean.cases=mean.cases.vec, obs.noise.fill=obs.noise.fill.vec)
    est.vax.change.irr<-exp(12*as.numeric(as.character(func[2])))
    irr.end[i] <-est.vax.change.irr  ##GIVES CHANGE PER YEAR POSTVAX
    
    sim.dataout<-cbind(i,unlist(func[[3]][1]),unlist(func[[3]][2]) )
    sim.data<-rbind(sim.data,sim.dataout)
  }
  
  est.pct.change.vax=100*(irr.end-1)
  quant.pct<- as.character(round(quantile(est.pct.change.vax,  probs = c(2.5, 50,97.5)/100),1))
  ci.vector<-paste("95%CI:", quant.pct[1], "," , quant.pct[3], sep='')			
  median.vector<-paste("Median:", quant.pct[2],sept="")
  mean.vector<-paste("Mean:", mean(est.pct.change.vax),sept="")
  std.dev.vector <- paste("Standard Deviation:", round(sqrt(var(est.pct.change.vax)),digits=2)  )
  
  names(sim.data)<-c("i","month", "cases")

  return(list(
    sim.data=sim.data, 
    est.pct.change.vax=est.pct.change.vax, 
    n.reps=n.reps,
    median.vector=median.vector,
    mean.vector=mean.vector,
    ci.vector=ci.vector,
    std.dev.vector=std.dev.vector
  ))
})

output$distPlot <- renderPlotly({
  if (is.null(delayedInput$pct.decline.yr)){
	  return()
  }
  
  sim.data = analysis()$sim.data
  overview = overview()

  ymaxval<- max(sim.data$cases)
  
  plot_ly(sim.data, x = ~month, y = ~cases) %>%
    group_by(i) %>%
    add_lines(line=list(width=0.1)) %>%
    layout(
      xaxis=list(
        tick0=0,
        dtick=12,
        title="Time (months)"
      ),
      yaxis=list(
        range=c(0, round(ymaxval * 1.1)),
        nticks=11,
        title="Cases"
      )
    ) %>%
    add_lines(data=overview(), x=~t, y=~cases) %>%
    add_lines(data=overview(), x=~t, y=~cases.min) %>%
    add_lines(data=overview(), x=~t, y=~cases.max)
})

output$evalPlot <- renderPlot({
  if (is.null(delayedInput$pct.decline.yr)){
    return()}
  
  m <- rbind(c(1, 2))
  layout(m)
  par(mar = c(3, 3, 2, 1))
  par(xaxs="i",  yaxs="i") # OPTION ENSURES X AND Y AXES MEET AT 0,0
  d <- density(analysis()$est.pct.change.vax) # returns the density data 
  plot(d, bty="l",xlim=c(-90, 100), main="", lwd=2, col="red")
  abline(v=0,lty="dotted",col="darkgray")
  abline(v=delayedInput$pct.decline.yr,lty="dashed",col="darkblue")
  text(delayedInput$pct.decline.yr,0.0,"Actual change (%)",srt=270, adj=c(1.0,1.00), cex=1.5 , col="darkgray")
  
  title(main="Estimates of % change from different simulations", cex.main=1.5)
  
  plot(1:10, 1:10,col="white", axes=FALSE)
  text(2,10, analysis()$median.vector , cex=3,adj=c(0, 0.5), xpd=NA  ) #Adds the median of the density plot
  #text(2,9, analysis()$mean.vector , cex=3,adj=c(0, 0.5), xpd=NA  ) #Adds the median of the density plot
  text(2,9, analysis()$ci.vector , cex=3,adj=c(0, 0.5), xpd=NA ) #Adds the median of the density plot
  text(2,8, analysis()$std.dev.vector , cex=3,adj=c(0, 0.5), xpd=NA ) #Adds the median of the density plot
})

observe({
  # Hold of if any values are NULL (it means we're still setting up the UI)
  years.post <- input$years.post.pcv.data
  years.pre <- input$years.pre.pcv.data 
  trend.secular <- liveInput$secular.trend.yr
  trend.vax <- liveInput$pct.decline.yr
  season.amp <- liveInput$season.amp
  mean.cases <- input$mean.cases.n
  if (is.null(years.post) || is.null(years.pre) || is.null(trend.secular) || is.null(trend.vax) || is.null(season.amp) || is.null(mean.cases)) {
    return()
  }
  data <- overview.data(years.pre, years.post, trend.secular, trend.vax, season.amp, mean.cases)
  plotlyProxy("distPlot", session) %>%
    plotlyProxyInvoke(
      "restyle", 
      "y",
      list(data$cases.min, data$cases, data$cases.max),
      list(1, 2, 3)
    )
})

})
