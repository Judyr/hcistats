# Functions to generate datasets
generateUIDs <- function(n){
	UID <- sapply(1:n, digest, algo="md5")
	return(UID)
}


### TEAM ASSIGN
assignTeam <- function(teams, data, p=rep(1/length(teams), length(teams))){
	data$team <- rbinom(dim(data)[1], length(teams)-1, p) + 1
	data$team <- as.factor(teams[data$team])
	return(data)
}



### SUSScale DATA
monthlySUSScaleData <- function(id, time, m0 = 2.5, within.effect=.4){  # team hard-coded
	
	# Create long form
	data <- ddply(data.frame(id), .(id), function(x, t){
		return(data.frame("ID"=rep(x$id,t), "Time"=c(1:t)))
	}, t=time)[,2:3]
	
	# Add scale data
	data <- ddply(data, .(Time), function(x, m0, within){
		x <- generatePsychometricData(x$ID, mean=(m0+x$Time*within), nvar=10, nfact=2, g=.3, r=.3, store=FALSE)
	}, m0=m0, within=within.effect)
	data <- data[order(data$ID,data$Time),]
	
	# clean names:
	data[c(1,2)] <- data[c(2,1)]
	names(data) <- c("ID", "Time", paste("SUS", c(1:10), sep=""))
	
	return(data)
}

generatePsychometricData <- function(uid, mean=3, nvar=9, nfact=3, g=.3, r=.3, store=FALSE, smin=1, smax=7, ...){
	n <- length(uid)
	raw.dat <- sim.general(nvar,nfact, g=g,r=r,n=n)
	scale.dat <- round(raw.dat+mean)
	scale.dat <- ifelse(scale.dat<smin,1,scale.dat)
	scale.dat <- ifelse(scale.dat>smax,5,scale.dat)
	ret.dat <- data.frame("ID"=uid, scale.dat)
	return(ret.dat)
}


## Average email response time


emailResponseTime <- function(data, t, m0=360, within=-20, between=-5, wb=-40, sigma=5){  # team hard-coded
	
	data <- ddply(data, .(), function(x, t){
		return(data.frame(data[rep(seq_len(nrow(data)), each=t),], "Time"=c(1:t)))
	}, t=t)
	data <- data[-1]
	
	data$team.n <- ifelse(data$Team=="Team A", 0, 1)
	
	data <- ddply(data, .(team.n, Time), function(x, m0, within, between, wb, sigma){
		x  <- data.frame(x, responseTime=rnorm(length(x$ID), (m0+x$Time*within+x$team.n*between+x$Time*x$team.n*wb), sigma))
		return(x)
	}, m0=m0, within=within, between=between, wb=wb, sigma=sigma)
	
	# No values smalle then 0
	data$responseTime <- ifelse(data$responseTime < 0, 0, data$responseTime)
	
	# Clean and order
	data <- data[order(data$Team,data$Time),]
	data <- data[c(1,2,3,5)]
	
	return(data)
}



