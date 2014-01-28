#index <- readLines("../Data/junq2.txt", encoding = "UTF-8")

findWoAprox <- function(word, index) {
    indVector <- agrep(word, index, ignore.case = TRUE)
    infoVector <- c();
    info <- c();
    for (ind in indVector) {
        if(grepl("[A-Z]",substr(index[ind],1,1)) == TRUE){
            info <- index[ind];
            k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        else {
            info <- index[ind];
            k <- ind - 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(index[k], info, sep = " ");
                k <- k-1;
            }
                k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        infoVector <- c(infoVector, info);
    }
    return(paste(infoVector, collapse = "\n", sep = ""));
}

findWo <- function(word, index) {
    indVector <- grep(word, index, ignore.case = TRUE)
    infoVector <- c();
    info <- c();
    for (ind in indVector) {
        if(grepl("[A-Z]",substr(index[ind],1,1)) == TRUE){
            info <- index[ind];
            k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        else {
            info <- index[ind];
            k <- ind - 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(index[k], info, sep = " ");
                k <- k-1;
            }
                k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        infoVector <- c(infoVector, info);
    }
    return(paste(infoVector, collapse = "\n", sep = ""));
}

findWord <- function(word, index) {
    indVector <- grep(paste("\\b",word,"\\b", sep=""), index, ignore.case = TRUE)
    infoVector <- c();
    info <- c();
    for (ind in indVector) {
        if(grepl("[A-Z]",substr(index[ind],1,1)) == TRUE){
            info <- index[ind];
            k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        else {
            info <- index[ind];
            k <- ind - 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(index[k], info, sep = " ");
                k <- k-1;
            }
                k <- ind + 1;
            while(grepl("[a-z]",substr(index[k],1,1)) == TRUE) {
                info <- paste(info, index[k], sep = " ");
                k <- k+1;
            }
        }
        infoVector <- c(infoVector, info);
    }
    return(paste(infoVector, collapse = "\n", sep = ""));
}

findWosAprox <- function(words, index) {
    infoVector <- c();
    wo <- c();
    for(word in words){
        info <- findWoAprox(word, index);
        if(info != "") {
            infoVector <- c(infoVector, info);
            wo <- c(wo, word);
        }
        
    }
    woFrame <- data.frame(words = character(length(wo)), index = character(length(wo)));
    woFrame[,"words"] <- wo;
    woFrame[,"index"] <- infoVector;
    return(woFrame);
}

findWos <- function(words, index) {
    infoVector <- c();
    wo <- c();
    for(word in words){
        info <- findWo(word, index);
        if(info != "") {
            infoVector <- c(infoVector, info);
            wo <- c(wo, word);
        }
        
    }
    woFrame <- data.frame(words = character(length(wo)), index = character(length(wo)));
    woFrame[,"words"] <- wo;
    woFrame[,"index"] <- infoVector;
    return(woFrame);
}

findWords <- function(words, index) {
    infoVector <- c();
    wo <- c();
    for(word in words){
        info <- findWord(word, index);
        if(info != "") {
            infoVector <- c(infoVector, info);
            wo <- c(wo, word);
        }
        
    }
    woFrame <- data.frame(words = character(length(wo)), index = character(length(wo)));
    woFrame[,"words"] <- wo;
    woFrame[,"index"] <- infoVector;
    return(woFrame);
}

#source("mySQL_curl.r");
#thw <- read.csv("../Data/thw2.csv");
ro <- thw[, "ro"];
ro<-as.character(ro);
len <- nchar(ro);
ro <- ro[len>3];
ro<-ro[6:length(ro)];
ro[1] <- "lamină";
ro <- ro[ro != c("[ufc]", "[fsc]")];
junqWoAprox <- findWosAprox(ro, index);
junqWo <- findWos(ro, index);
junqWords <- findWords(ro, index);
write.csv(junqWoAprox, "../Data/junqWoAprox.csv");
write.csv(junqWo, "../Data/junqWo.csv");
write.csv(junqWords, "../Data/junqWos.csv");
