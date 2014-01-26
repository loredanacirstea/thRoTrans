#mysql connection
#install.packages("RMySQL");
library(RMySQL);
#mydb = dbConnect(MySQL(), user='root', password='', dbname='thesis', host='127.0.0.1');
#tables = dbListTables(mydb);
mydb = dbConnect(MySQL(), user='root', password='', dbname='chr', host='127.0.0.1');

#install.packages("translate");
library(translate);
set.key("AIzaSyC5nT8bwUjdNXJxRbiloQhy6qhybDsdPNo")

#first paragraph from wiki into mysql table
#install.packages("XML");
#library(XML);
#url = "http://en.wikipedia.org/wiki/Gaia_(spacecraft)";
#desc = htmlTreeParse(url, useInternalNodes = T);
#tex = xpathSApply(desc, "//p[1]", xmlValue);
#tex <- gsub("'", "\\'", tex, fixed = TRUE);
#tex <-gsub("[]", "", tex, fixed = TRUE);
#ins <- dbSendQuery(mydb, paste("insert into `", tables[5], "` set `description`= \"", tex, "\", `url`=\"", url, "\"", sep=""));

#set encoding for future query results as utf8
dbSendQuery(mydb, "SET NAMES 'utf8'");
#dataframe with all terminologies and words:
#allQuery = dbSendQuery(mydb, "select * from `term`");
#tm = fetch(allQuery, n = -1);

#extracting specific terms from origin(term_id)
#outputs vector of ids = path starting from origin
tree <- function(origin, path = c()) {
    path <- c(path, origin);
    kids <- children(origin, returnIds = TRUE);
    for(kid in kids) {
        path <- tree(kid, path);
    }
    return(path);
}

#initialising data frame with terminology from given origin - from term
initialise <- function(origin) {
    idPath <- tree(origin);
    idPathChar <- paste(idPath, collapse = ", ", sep = "");
    dbSendQuery(mydb, "SET NAMES 'utf8'");
    dataFrame <- dbGetQuery(mydb, paste("select * from `term` where `term_id` in (", idPathChar, ")", sep = ""));
    return(dataFrame);
}

#dataframe with all the relations term-term, word-term
#have to do initialise
relQuery = dbSendQuery(mydb, "select * from `term_relation`"); #dbGetQuery might be better
rel = fetch(relQuery, n = -1);
#lower case for all terms except t language
#tm[,"term"] <- c(tolower(tm[tm$lang != "t","term"]), tm[tm$lang == "t","term"]);

#translation function - example: translate("term", "ro") (! attention term with language!! different terminologies gives only the first)
translateT <- function(term, lang) {
    term = tolower(term);
    termId = tm[tm$term == term, "term_id"];
    termId <- termId[!is.na(termId)][1];
    return(tm[tm$term_id == termId & tm$lang == lang, "term"]);
    }
#translate cu termId!

#usedIn function for words; returns a numeric vector with all id's of the terms which use the given word or character vector with the terms
usedIn <- function(word, lang) {
    termId = tm[tm$term == word & tm$lang == lang, "term_id"][1];
    #return(rel[rel$term1 == termId & rel$relation == 3,"term2"]);
    termsIds = rel[rel$term1 == termId & rel$relation == 3,"term2"];
    termsNames = c();
    for(id in termsIds) {
        termsNames = c(termsNames, tm[tm$term_id == id & tm$lang == lang, "term"]);
        }
     return(termsNames);
    }
#usedIn cu origin optional

#children - mai pun limba? numeric vs character?
children <- function(term, lang, returnIds = FALSE) {
    if(class(term) == "numeric") {
        termId = term;
    }
    else {
        termId = tm[tm$term == term, "term_id"];
        termId <- termId[!is.na(termId)][1];
    }
    kidsIds = rel[rel$term2 == termId & rel$relation == 1,"term1"];
    if(returnIds == TRUE) {
        return(kidsIds);
        }
    else {
        kids = c();
        for(id in kidsIds) {
            termN = tm[tm$term_id == id & tm$lang == lang, "term"];
            termN <- termN[!is.na(termN)][1];
            kids = c(kids, termN);
            }
        return(kids);
        }
    }

#ancestry function from origin(id): (!origin)
ancestry <- function(term, lang, origin = 9000, returnIds = FALSE) {
    if(class(term) == "numeric") {
        termId = term;
    }
    else {
        termId = tm[tm$term == term, "term_id"];
        termId <- termId[!is.na(termId)][1];
    }
    path = termId;
    id = termId;
    while((id != origin) == TRUE) {
        parent = rel[rel$term1 == id & rel$relation == 1,"term2"];
        path = c(path, parent);
        id = parent;
    }
    if(returnIds == TRUE) {
        return(path);
    }
    else {
        termsNames = c();
        for(id in path) {
            termsNames = c(termsNames, tm[tm$term_id == id & tm$lang == lang, "term"]);
        }
        return(termsNames);
    }
}

#output: list of the equivalent terms in all languages of the terminology
langTerms <- function(term, lang) {
    if(class(term) == "numeric") {
        termId = term;
    }
    else {
        termId = tm[tm$term == term, "term_id"];
        termId <- termId[!is.na(termId)][1];
    }
    allTerms <- tm[tm$term_id == termId, c("term", "lang")];
    return(allTerms);
}

#makes a word vector from initialized terminology - +origin? (! [],no )
makeWords <- function(lang) {
    allTerms = tm[tm$lang == lang, "term"];
    #wordsList = strsplit(allTerms,"\s|,|\(|\)|\[|\]|[0-9]|;");
    #words = unlist(wordsList);
    allTerms <- allTerms[!is.na(allTerms)];
    wordsList = strsplit(allTerms," ");
    words = unlist(wordsList); #makes it a vector
    words = strsplit(words,",");
    words = unlist(words);
    words = strsplit(words,"(", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,")", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,";");
    words = unlist(words);
    words = strsplit(words,"[", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,"]", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,"{", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,"}", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,"/", fixed = TRUE);
    words = unlist(words);
    words = strsplit(words,"[0123456789]");
    words = unlist(words);
    words = unique(words); #removes doubles
    return(words);
}

#creates dataframe with words in the provided languages, same structure with terminology dataframe; input: vector of languages, output: data frame.
wordFrame<- function(langs) {
    allWords = c();
    allLangs = c();
    for(lang in langs) {
        words = makeWords(lang);
        l = length(words);
        allWords = c(allWords, words);
        langV = rep(lang,l);
        allLangs = c(allLangs, langV);
    }
    len <- length(allWords);
    wo <- data.frame(id = numeric(len), term_id = numeric(len), lang = character(len), part_speech = character(len), gender = character(len), term = character(len), description = character(len), wiki = character(len)) #not good - it should autom. take the original structure
    wo[,"term"] <- allWords;
    wo[,"lang"] <- allLangs;
    startId <- nrow(tm)+30000; #tm used! attention to init
    ids <- seq(startId,startId + len-1); 
    wo[,"term_id"] <- ids;
    wo[,"id"] <- ids;
    return(wo);
}

#ancestry, children, names in all lang - present function
present <- function(dataf) {
    
}

#pt browse: cat("What's your name? ") x <- readLines(file("stdin"),1) print(x) pui asa ceva in a while loop ! origin ! same term in diff languages
#browse() - root-ul in en + info
#browse("term") - multipl term+lang
#browse(id)
#browse(..., orig, lang = "en")
browse <- function(term, lang) {
    input <- 0;
    while( input != "." ) {
        if(class(term) == "numeric") {
            termId = term;
        }
        else {
            termId = tm[tm$term == term, "term_id"];
            termId <- termId[!is.na(termId)];
        }
        if(length(termId) > 1) {
            cat("This term belongs to multiple terminologies:", "\n");
            for(i in 1:length(termId)) {
                cat(i, " term id: ", termId[i], ". Ancestry: ", sep = "");
                parents <- ancestry(termId[i], lang);
                for(i in length(parents):2) {
                    cat(parents[i], "->");
                }
                cat(parents[1], "\n");
            }
            cat("Write the digit/number of the chosen term:");
            opt <- readLines(n=1);
            termId <- termId[as.numeric(opt)];
        }
        cat("Ancestry:","\n");
        parents <- ancestry(termId, lang);
        no <- 1;
        pPares <- list()
        for(i in length(parents):2) {
            cat(paste(letters[no], parents[i]), "\n");
            pPares[letters[no]] = parents[i];
            no <- no+1;
        }
        equiv <- langTerms(parents[1]); 
        cat("Term:", "\n");
        cat("Term id: ", termId, "\n"); 
        for(i in 1:nrow(equiv)) {
            cat(paste(equiv[i, "lang"], ": ", equiv[i, "term"], sep = ""), "\n");
        }
        cat("Children:", "\n");
        kids <- children(termId, lang);
        if(length(kids) == 0) {
            cat("No children.", "\n");
        }
        no <- 1;
        kPares <- list()
        for(kid in kids) {
            cat(paste(no, kid), "\n");
            kPares[as.character(no)] = kid;
            no <- no+1;
        }
        cat("Write the number or letter of the chosen term (or \".\" for closing browse) and press Enter:");
        input <- readLines(n=1);
        if(input %in% names(pPares)) {
            term <- pPares[input][[1]];
        }
        else if(input %in% names(kPares)) {
                term <- kPares[input][[1]];
              }
    }   
}

#update data frame with same structure as the terminologies
#updates existing row or inserts a new one (new = TRUE)
#maybe add the findTerm function?
updateFrame <- function(dataF, term, lang, termId = NA, new = FALSE, partSpeech = NA, gender = NA, descript = NA, wiki = NA) {
    if(new == FALSE) {
        dataF[dataF$term_id == termId, "term"] <- term;
    }
    if(new == TRUE) {
        if(is.na(termId)) {
            termId <- max(dataF[,"term_id"])+1;
        }
        newRow <- data.frame(id = max(dataF[,"id"])+1, term_id = termId, lang = lang, part_speech = partSpeech, gender = gender, term = term, description = descript, wiki = wiki); #not good - it should autom. take the original structure
        dataF = rbind(dataF, newRow);
        return(dataF);
    }
}

#translate one word at a time and append to data frame provided
transWord <- function(dataF, termId, lang, newLang) {
    langN <- c(lang, "->", newLang);
    dataF <- updateFrame(dataF, translate(dataF[dataF$term_id == termId & dataF$lang == lang, "term"], lang, newLang), langN, termId, new = TRUE);
    return(dataF);
}

# ! term_id in words same id for translations
#transWords: input: data frame with words from wordFrame, vector of languages
#output: same data frame with appended translated words / separate data frame (new=TRUE)
#!! termIds are constructed separately from terms - problems might arrise
transWords <- function(words,langs, newLang, new = FALSE) {
    #words<-update(words, term, newLang, termId, new = TRUE);
    for(lang in langs) {
        trans <- words[words$lang == lang, "term"];
        n <- floor(length(trans)/ 2000);
        transVector <- c();
        if(n > 0) {
            for(i in 1:n) {
                transVector <- c(transVector, paste(trans[i:(i+1999)], collapse = "\n", sep = ""));
            }
        }
        transVector <- c(transVector, paste(trans[i:length(trans)], collapse = "\n", sep = ""));
        translated <- c();
        for(string in transVector) {
            translated <- c(translated, unlist(strsplit(translate(string, lang, newLang), sep = "\n")));
        }
        len <- length(translated);
        wot <- data.frame(id = numeric(len), term_id = numeric(len), lang = character(len), part_speech = character(len), gender = character(len), term = character(len), description = character(len), wiki = character(len)) #not good - it should autom. take the original structure
        wot[,"id"] <- seq(nrow(words)+1, nrow(words)+len);
        wot[,"term_id"] <- words[words$lang == lang, "term_id"];
        wot[,"lang"] <- lang;
        wot[,"term"] <- translated;
        words <- rbind(words, wot);        
    }
    return(words);
}

#not for thesis
thdb <- function(th) {
    len = nrow(th[th$lang == "t",
}

tm<-initialise(10001);
#wo<-wordFrame("la","en");
#wo<-transWords(wo, "la", "ro");
#wo<-transWord(wo, 47032, "la", "ro")

#dbDisconnect(mydb)
#cons <- dbListConnections(MySQL());#for(con in cons) {#   dbDisconnect(con);
#}