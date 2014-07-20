#install.packages("RCurl");
library(RCurl);

#install.packages("translate");
#library(translate);
#set.key("AIzaSyC5nT8bwUjdNXJxRbiloQhy6qhybDsdPNo")

#first paragraph from wiki into mysql table
#install.packages("XML");
#library(XML);
#url = "http://en.wikipedia.org/wiki/Gaia_(spacecraft)";
#desc = htmlTreeParse(url, useInternalNodes = T);
#tex = xpathSApply(desc, "//p[1]", xmlValue);
#tex <- gsub("'", "\\'", tex, fixed = TRUE);
#tex <-gsub("[]", "", tex, fixed = TRUE);
#ins <- dbSendQuery(mydb, paste("insert into `", tables[5], "` set `description`= \"", tex, "\", `url`=\"", url, "\"", sep=""));

#extracting specific terms from origin(term_id)
#outputs vector of ids = path starting from origin
tree <- function(allTerms, allRel, origin, path = c()) {
  path <- c(path, origin);
  kids <- children(allTerms, allRel, origin, returnIds = TRUE);
  for(kid in kids) {
    path <- tree(allTerms, allRel, kid, path);
  }
  return(path);
}

#initialising data frame with terminology from given origin - from term
initialise <- function(allTerms, allRel, origin, langs){
  df<-data.frame(t(rep(NA,length(names(allTerms)))));
  names(df)<-names(allTerms);
  df<-df[-1,];
  reldf <- data.frame(t(rep(NA,length(names(allRel)))));
  names(reldf) <- names(allRel);
  reldf <- reldf[-1,];
  termIds<-c();
  termIds<-tree(allTerms, allRel, origin);
  df<-allTerms[allTerms$term_id %in% termIds & allTerms$lang %in% langs,];
  reldf <- allRel[allRel$term1 %in% termIds | allRel$term2 %in% termIds,];
  tm <<- df;
  rel <<- reldf;
}

#dataframe with all the relations term-term, word-term
#have to do initialise
#relQuery = dbSendQuery(mydb, "select * from `term_relation`"); #dbGetQuery might be better
#rel = fetch(relQuery, n = -1);
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
children <- function(terms, rels, term, lang, returnIds = FALSE) {
    if(class(term) == "numeric" || class(term) == "integer") {
        termId = term;
    }
    else {
        termId = terms[terms$term == term, "term_id"];
        termId <- termId[!is.na(termId)][1];
    }
    kidsIds = rels[rels$term2 == termId & rels$relation == 1,"term1"];
    if(returnIds == TRUE) {
        return(kidsIds);
        }
    else {
        kids = c();
        for(id in kidsIds) {
            termN = terms[terms$term_id == id & terms$lang == lang, "term"];
            termN <- termN[!is.na(termN)][1];
            kids = c(kids, termN);
            }
        return(kids);
        }
    }

#ancestry function from origin(id): (!origin)
ancestry <- function(terms, rels, term, lang, origin = 9000, returnIds = TRUE) {
    if(class(term) == "numeric" || class(term) == "integer") {
        termId = term;
    }
    else {
        termId = terms[terms$term == term, "term_id"];
        termId <- termId[!is.na(termId)][1];
    }
    path = termId;
    if(termId == origin) {
      return();
    }
    id = termId;
    while((id != origin) == TRUE) {
        parent = rels[rels$term1 == id & rels$relation == 1,"term2"];
        path = c(path, parent);
        id = parent;
    }
    if(returnIds == TRUE) {
        return(path);
    }
    else {
        termsNames = c();
        for(id in path) {
            termsNames = c(termsNames, terms[terms$term_id == id & terms$lang == lang, "term"]);
        }
        return(termsNames);
    }
}

#output: list of the equivalent terms in all languages of the terminology
langTerms <- function(term, lang) {
    if(class(term) == "numeric" || class(term) == "integer") {
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
makeWords <- function(tm,lang) {
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

#update data frame with same structure as the terminologies
#updates existing row or inserts a new one (new = TRUE)
#maybe add the findTerm function?
#update words also
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

#pt browse: cat("What's your name? ") x <- readLines(file("stdin"),1) print(x) pui asa ceva in a while loop ! origin ! same term in diff languages
#browse() - root-ul in en + info
#browse("term") - multipl term+lang
#browse(id)
#browse(..., orig, lang = "en")
startOptions <- function() {
  cat("1 Show terminologies", "\n");
  cat("2 Load from known ID", "\n");
  cat("3 Load from known term", "\n");
  cat("4 Exit interface and use API", "\n");
  cat("Write number: ");
  opt <- readLines(n=1);
  if(opt == 1 || opt == 2 || opt == 3 || opt == 4) { return(opt); }
  else {
    cat("Invalid choice. Write one of the above numbers: ");
    return(startOptions());
  }
}
readChoice <- function(type) {
  cat("Enter ", type, " or go back ( write < ) and press Enter: ", "\n");
  choice <- readLines(n=1);
  if(choice == "<") { ontobrowse(); }
  else { return(choice); }
}
dataOptions <- function(){
  cat("1 Browse and search data", "\n");
  cat("2 Update data", "\n");
  cat("3 Insert new data", "\n");
  cat("4 Translate terms or text", "\n");
  cat("5 Verify terms in text", "\n");
  cat("6 Browse data", "\n");
}
ontobrowse <- function() {
    opt <- startOptions();
    if(opt == 4) {
      return(cat("User interface exited. Check documentation for direct function use."));
    }
    
    #for 1,2,3 it loads the source data from the csv file
    else {
      cat("Connecting to database ...");
      time <- 1;
      if(time == 1) {
        #x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv");
        allTerms <- read.csv(text = x);
        #y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv");
        allRel <- read.csv(text = y);
        time <- 2;
      }

      #opt==3 -> load from term
      if(opt == 3){
        valid <- 0;
        while(valid == 0) {
          choice <- readChoice("term");
          id <- allTerms[tolower(allTerms$term) == tolower(choice), "term_id"];
          if(length(id) == 0) { cat("There is no such term. Try again."); valid <- 0; }
          else { valid <- 1; }
        }
        
        #same term for different concepts or same term in different languages for the same concept:
        if(length(id) > 1) { 
          ids <- factor(id);
          concepts <- levels(ids);
          
          #one concept, same term in different languages
          if(length(concepts) == 1) {id <- id[1];} 
          
          #multiple concepts - choice:
          else {
            cat("This term defines multiple concepts:", "\n");
            for(i in 1:length(concepts)) {
              cat(i, " term ID: ", concepts[i], ". Ancestry: ", sep = "");
              parents <- ancestry(allTerms, allRel, as.integer(concepts[i]));
              for(i in length(parents):2) {
                cat(as.character(allTerms[allTerms$term_id == parents[i] & allTerms$lang == "la", "term"]), " (term ID: ", parents[i], ") ", "->");
              }
              cat(as.character(allTerms[allTerms$term_id == parents[1] & allTerms$lang == "la", "term"]), "\n");
            } 
            cat("Write the digit/number of the chosen concept:");
            conc <- readLines(n=1);
            id <- as.numeric(concepts[as.numeric(conc)]);
          } #/concept choice
        } #/length(id)>1 - multiple term Ids
      } #/opt==3
      
      #opt==2 -> load from id
      if(opt == 2){
        valid <- 0;
        while(valid == 0) {
          choice <- readChoice("term ID");
          id <- as.integer(choice);
          if(length(allTerms[allTerms$term_id == id,]) == 0) { 
            cat("There is no such id. Try again.");
            readChoice("term ID");
          }
          else { valid <- 1; }
        }
      } #/opt==2
      
      #opt==1 -> load from terminologies
      if(opt == 1) {
        terminologies <- allRel[allRel$term2 == 9000, "term1"];
        for(terminology in terminologies) {
          name <- allTerms[allTerms$term_id == terminology & allTerms$lang == "la", "term"];
          cat("term ID: ", terminology, "term: ", as.character(name), "\n");
        }
        cat("Write the term ID for the chosen terminology or go back (write < ) and press Enter: ", "\n");
        choice <- readLines(n=1);
        if(choice == "<") { ontobrowse();}
        else {
          id <- as.integer(choice);
          if(length(allTerms[allTerms$term_id == id,]) == 0) { 
            cat("There is no such term. Try again.");
            id <- integer(0);
          }
        }
      }
      
      #choose languages to load
      langs <- allTerms[allTerms$term_id == id, "lang"];
      langs <- as.character(langs);
      cat("1 Load all languages: ", paste(langs, sep = " "), "\n");
      cat("2 Choose languages", "\n");
      opt2 <- readLines(n=1);
      if(opt2 == 2) {
        cat("Write the chosen languages separated by one space character. Choose from: ", 
            paste(langs, sep = " "), "\n");
        
        langs <- unlist(strsplit(readLines(n=1), " "));
      }
    } #/options 1-3
    
    # initialise data frames
    initialise(allTerms, allRel, id, langs);
    #choice <- dataOptions();

  kk<-0
  if(kk == 1){
  input <- 0;
  while( input != "." ) {
    if(class(term) == "numeric" || class(term) == "integer") {
      termId = term;
    }
    else {
      termId = tm[tm$term == term && tm$lang == lang, "term_id"];
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
}

#global data frames: terms and relations
tm <- data.frame();
rel <- data.frame();
#ontobrowse();

#wo<-wordFrame("la","en");
#wo<-transWords(wo, "la", "ro");
#wo<-transWord(wo, 47032, "la", "ro")
