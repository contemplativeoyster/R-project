# Comparison of metaphorical usages of adjectives with the meaning “round” in British English and Castillian Spanish
## current work is devoted to comparison of metaphorical usages of adjectives in collocations on the example of British English and Castillian Spanish. We analyze the first 100 popular collocations with an adjective with the meaning "round" in both corpora. To each of them we have given a label "1" or "0" depending on the fact wheter the adjective with the meaning "round" was used in its indirect meaning or not. So metaphorical and non-metaphorical collocations got the labels "1" and "0" respectively.
library(ggplot2)
## 1.First let us consider metaphorical usage of an adjective "round" in English and "redondo" in Spanish collocation
### set working directory  

    setwd ("C:/Users/Íàñòÿ/Desktop/Ó÷¸áà/Databases")

### read the file with english collocations
    
    eng <- read.csv("british.csv")

### read the file with spanish collocations

    esp <- read.csv ("spanish.csv")

### read files in the directory to R
    
    myfiles  <- lapply(list.files(), read.delim)

### merge files into one dataframe
    
    result_df <- rbind(eng, esp)

### create labels 
    
    metaphor <- c("English", "Spanish")
    result_df$label <- rep(metaphor, sapply(myfiles, nrow))

### draw the plot 
    counts <- table(result_df$metaphor, result_df$label)
    barplot(counts, main= "Metaphorical usage of the adjective with the meaning round",
        xlab="Languages", col= c("green","orange"),
        legend = c("direct", "metaphor"))

## 2. Here we are going to observe frequencies of metaphorical and direct usages in English and Spanish
### 2.1 We shall start with english collocations 
    eng <- read.csv("british.csv")

### Dataframes to visualize: the whole sample, metaphorical collocations and collocations with the adjective "redondo" in its direct meaning
    met_eng <- subset(eng, metaphor=='1')
    dir_eng <- subset(eng, metaphor=='0')

### draw histograms with a normal curve 
    a <- eng$frequency
    b <- met_eng$frequency
    c <- dir_eng$frequency

    grid <- matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE)  
    layout(grid)

    h <- hist(a,breaks=10,  col="blue", xlab="Frequency in corpus",
     main="Collocations with the adjective round") 
    xfit<-seq(min(a),max(a),length=40) 
    yfit<-dnorm(xfit,mean=mean(a),sd=sd(a)) 
    yfit <- yfit*diff(h$mids[1:2])*length(a) 
    lines(xfit, yfit, col="red", lwd=2)

    i <- hist(b,breaks=10,  col="blue", xlab="Frequency in corpus",
     main="Metaphorical collocations") 
    xfit<-seq(min(b),max(b),length=40) 
    yfit<-dnorm(xfit,mean=mean(b),sd=sd(b)) 
    yfit <- yfit*diff(i$mids[1:2])*length(b) 
    lines(xfit, yfit, col="red", lwd=2)

    j <- hist(c,breaks=10,  col="blue", xlab="Frequency in corpus",
     main="Direct meaning") 
    xfit<-seq(min(c),max(c),length=40) 
    yfit<-dnorm(xfit,mean=mean(c),sd=sd(c)) 
    yfit <- yfit*diff(j$mids[1:2])*length(c) 
    lines(xfit, yfit, col="red", lwd=2)

## Seems that we have managed to create 3 histograms with the frequencies of collocations with the adjective "round" in different roles
### 2.2 Now it is time to repeat the excercise on the base of Spanish language

    esp <- read.csv ("spanish.csv")

### again forming 3 dataframes to work at
    met_esp <- subset(esp, metaphor == '1')
    dir_esp <- subset(esp, metaphor == '0')

### Did you miss the histograms? Here they are! 
    x <- met_esp$frequency
    y <- esp$frequency
    z <- dir_esp$frequency

    grid <- matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE)  
    layout(grid)

    k <- hist(y,breaks=10,  col="red", xlab="Frequency in corpus",
     main="Collocations with the adjective redondo") 
    xfit<-seq(min(y),max(y),length=40) 
    yfit<-dnorm(xfit,mean=mean(y),sd=sd(y)) 
    yfit <- yfit*diff(k$mids[1:2])*length(y) 
    lines(xfit, yfit, col="blue", lwd=2)

    l <- hist(x, breaks=10,  col="red", xlab="Frequency in corpus",
     main="Metaphorical collocations") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="blue", lwd=2)

    hist(z,breaks=10,  col="red", xlab="Frequency in corpus",
     main="Direct meaning") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="blue", lwd=2)

### 3.3 Now we are eager to compare metaphorical and direct usages of adjectives with the meaning "round" in both languages
    grid <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)  
    layout(grid)
    hist(b,breaks=10,  col="blue", xlab="Frequency in corpus",
     main="Metaphorical round") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="red", lwd=2)

    hist(x, breaks=10,  col="red", xlab="Frequency in corpus",
     main="Metaphorical redondo") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="blue", lwd=2)

    hist(c,breaks=10,  col="blue", xlab="Frequency in corpus",
     main="Direct meaning - round") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="red", lwd=2)

    hist(z,breaks=10,  col="red", xlab="Frequency in corpus",
     main="Direct meaning - redondo") 
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x) 
    lines(xfit, yfit, col="blue", lwd=2)
