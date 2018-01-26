library(pracma)
library(reshape2)
library(tidyverse)

foodpref <- function(poi, dataspec, normp = 2, weights = NULL, nlmin = "zero", nlmax = "maxobs") {
        df <- dataspec
        
        ## Test for presence of POI in data frame
        
        if (poi %in% df$name) {
                row.names(df) <- df$name
                df <- select(df, -name)
                
                ## Set/check the weights vector        
                if (is.null(weights)) {
                        weights <- rep(1, (ncol(df)))
                } else if (length(weights) != ncol(df)) {
                        stop("Invalid argument: Length of weights vector must be equal to number of food and beverage items")
                } else {
                        weights <- weights
                }
                
                ## Create df of weighted scores       
                df2 <- mapply('*',weights,df)
                df2 <- as.data.frame(df2)
                row.names(df2) <- row.names(df)
                out <- dist(df2, method = "minkowski", p = normp)
                
                dfdist <- melt(as.matrix(out),varnames=c("Row","Col"))
                
                dfpoi <- filter(dfdist,Col == poi & Row != poi)
                
                dfpoi <- arrange(dfpoi, value)
                
                ## Set axis minimum
                if (nlmin == "zero") {
                        nlmin <- 0
                } else if (nlmin == "minobs") {
                        nlmin <- min(dfpoi$value)
                } else {
                        stop("Invalid argument: nlmin must be either \"zero\" or \"minobs\"")
                }
                
                ## Set axis maximum
                if (nlmax == "maxobs") {
                        nlmax <- max(dfpoi$value)
                } else if (nlmax == "theomax") {
                        theo <- ((5*weights-1*weights)^normp)
                        nlmax <- nthroot(sum(theo),normp)
                } else {
                        stop("Invalid argument: nlmax must be either \"maxobs\" or \"theomax\"")
                }
                
                ## Create number line
                xlim <- c(nlmin,nlmax)
                ylim <- c(0,45)
                px <- dfpoi$value
                py <- rep(0,length(dfpoi$value))
                lx.buf <- 1
                lx <- seq(xlim[1]+lx.buf,xlim[2],len=length(px))
                ly <- 15
                
                par(xaxs='i',yaxs='i',mar=c(5,3,3,3))
                plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F)
                axis(1, at = seq(0,ceiling(nlmax),by=1))
                
                segments(px,py,lx,ly,lty=3)
                points(px,py,pch=16,col="maroon")
                text(lx,ly,dfpoi$Row,pos=3,srt=90,col="maroon")
                
        } else {
                stop("Invalid argument: Person of interest does not exist in data frame")
        }
}


data <- read_csv("foodpref-data.csv")
poi <- "Rachel"
