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

#dataframe with all the relations term-term, word-term
#lower case for all terms except t language
#tm[,"term"] <- c(tolower(tm[tm$lang != "t","term"]), tm[tm$lang == "t","term"]);

#translation function - example: translate("term", "ro") (! attention term with language!! different terminologies gives only the first)
translateT <- function(term, lang) {
    term = tolower(term);
    termId = tm[tm$term == term, "term_id"];
    termId <- termId[!is.na(termId)][1];
    return(tm[tm$term_id == termId & tm$lang == lang, "term"]);
    }
#usedIn function for words; returns a numeric vector with all id's of the terms which use the given word or character vector with the terms; origin?
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
uniqueWordsVector <- function(terms = tm,lang) {
    allTerms = terms[terms$lang == lang, "term"];
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
sourceWordsList <- function(terms = tm,lang, sourc = 0) {
  allTerms = c();
  termids <- unique(as.character(terms[,"term_id"]));
  #termids <- as.character(terms[,"term_id"]);
  if(sourc != 0) {
    for(termid in termids) {
      allTerms[termid] <- as.character(terms[terms$lang == lang & terms$term_id == termid & terms$source == sourc, "term"]);
    }
  }
  else {
    for(termid in termids) {
      allTerms[termid] <- as.character(terms[terms$lang == lang & terms$term_id == termid, "term"]);
    }
  }
  #allTerms <- allTerms[!is.na(allTerms)];
  allTerms[termids[1]] <- strsplit(allTerms[1], " ");
  for(termid in termids) {
    if(allTerms[[termid]] != ""){
      if(termid != termids[1]) { allTerms[[termid]] <- strsplit(allTerms[[termid]], " "); }
      allTerms[[termid]] <- unlist(allTerms[[termid]]);
      for(char in c(",", "(", ")", ";", "[", "]", "{", "}", "/")) {
        allTerms[[termid]] <- unlist(strsplit(allTerms[[termid]], char, fixed = TRUE));
      }
      allTerms[[termid]] <- unlist(strsplit(allTerms[[termid]], "[0123456789]"));
    }
  }
  return(allTerms);
}
sourceWordsList2 <- function(terms = tm,lang, sourc = 0) {
  allTerms = list();
  if(sourc == 0) {
    termsV <- as.character(terms[terms$lang == lang,"term"]);
    termids <- as.character(terms[terms$lang == lang,"term_id"]);
  }
  else {
    termsV <- as.character(terms[terms$lang == lang & terms$source == sourc,"term"]);
    termids <- as.character(terms[terms$lang == lang & terms$source == sourc,"term_id"]);
  }
  for(i in 1: length(termsV)){
    t<-termsV[i];
    if(length(allTerms[[termids[i]]]) == 0 || is.na(allTerms[[termids[i]]]) == TRUE || is.null(allTerms[[termids[i]]]) ==  TRUE){ allTerms[termids[i]] <- termsV[i]; }
    else { 
      if(length(allTerms[[termids[i]]]) == 1){ allTerms[[termids[i]]] <- list(allTerms[[termids[i]]], termsV[i]); } 
      else { allTerms[[termids[i]]] <- c(allTerms[[termids[i]]], termsV[i]); }
    }
    tt<-allTerms[[termids[i]]];
  }
  #allTerms <- allTerms[!is.na(allTerms)];
  allTerms[[termids[1]]] <- strsplit(as.character(allTerms[[termids[1]]]), " ");
  allTerms[[termids[1]]] <- unlist(allTerms[[termids[1]]]);
  if(length(allTerms[[termids[1]]]) == 0) { allTerms[[termids[1]]] <- "";}
  for(termid in termids) {
    if(allTerms[[termid]] != "" && termid != 1){
      if(is.list(allTerms[[termid]]) == TRUE) {
        for(n in 1: length(allTerms[[termid]])) {    
            allTerms[[termid]][[n]] <- unlist(strsplit(as.character(allTerms[[termid]][[n]]), " "));
            for(char in c(",", "(", ")", ";", "[", "]", "{", "}", "/")) { allTerms[[termid]][[n]] <- unlist(strsplit(allTerms[[termid]][[n]], char, fixed = TRUE)); }
            allTerms[[termid]][[n]] <- unlist(strsplit(allTerms[[termid]][[n]], "[0123456789]"));
            }
      }
      else{
        allTerms[[termid]] <- unlist(strsplit(as.character(allTerms[[termid]]), " "));
        for(char in c(",", "(", ")", ";", "[", "]", "{", "}", "/")) { allTerms[[termid]] <- unlist(strsplit(allTerms[[termid]], char, fixed = TRUE)); }
        allTerms[[termid]] <- unlist(strsplit(allTerms[[termid]], "[0123456789]"));
      }
    }
  }
  return(allTerms);
}
#creates dataframe with words in the provided languages, same structure with terminology dataframe; input: vector of languages, output: data frame.
uniqueWordFrame<- function(terms = tm, langs, add = 0) {
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
    wo <- data.frame(id = numeric(len), term_id = numeric(len), lang = character(len),
                     part_speech = character(len), gender = character(len), term = character(len),
                     source = character(len), description = character(len), wiki = character(len), pages = character(len)); #not good - it should autom. take the original structure
    wo[,"term"] <- allWords;
    wo[,"lang"] <- allLangs;
    #startId <- nrow(tm)+30000; #tm used! attention to init
    startId <- 1;
    ids <- seq(startId,startId + len-1); 
    wo[,"term_id"] <- ids;
    wo[,"id"] <- ids;
    return(wo);
}
sourceWordFrame<- function(terms = tm, langs, sources = 0) {
  allWords = c();
  allLangs = c();
  sourceList <- list();
  if(sources == 0){
    for(lang in langs) {
      words <- makeSourceWords(terms, lang);
    }
    
    l = length(words);
    allWords = c(allWords, words);
    langV = rep(lang,l);
    allLangs = c(allLangs, langV);
  }
  len <- length(allWords);
  wo <- data.frame(id = numeric(len), term_id = numeric(len), lang = character(len),
                   part_speech = character(len), gender = character(len), term = character(len),
                   source = character(len), description = character(len), wiki = character(len), pages = character(len)); #not good - it should autom. take the original structure
  wo[,"term"] <- allWords;
  wo[,"lang"] <- allLangs;
  #startId <- nrow(tm)+30000; #tm used! attention to init
  startId <- 1;
  ids <- seq(startId,startId + len-1); 
  wo[,"term_id"] <- ids;
  wo[,"id"] <- ids;
  return(wo);
}
#update data frame with same structure as the terminologies
#updates existing row or inserts a new one (new = TRUE)
#maybe add the findTerm function?
#update words also
updateFrame <- function(dataF, term, lang, termId = NA, new = FALSE, partSpeech = NA, gender = NA, descript = NA, wiki = NA) {
    if(new == FALSE) {
        dataF[dataF$term_id == termId & dataF$lang == lang, "term"] <- term;
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
oldbrowse<-function(){ 
  kk<-0;
  if(kk == 1){
    cat("Write the digit/number of the chosen term:");
    opt <- readLines(n=1);
    termId <- termId[as.numeric(opt)];
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

tree <- function(allTerms = tm, allRel = rel, origin = orig, path = c()) {
  path <- c(path, origin);
  kids <- children(allTerms, allRel, origin, returnIds = TRUE);
  for(kid in kids) {
    path <- tree(allTerms, allRel, kid, path);
  }
  return(path);
}
initialise <- function(allTerms = tm, allRel = rel, origin = orig, langs){
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
children <- function(terms = tm, rels = rel, term = orig, lang = displayLg, returnIds = TRUE) {
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
siblings <- function(terms = tm, rels = rel, term = orig, lang = displayLg, returnIds = TRUE) {
  if(returnIds == TRUE) { sibs <- children(terms, rels, rels[rels$term1 == term, "term2"]);}
  else { sibs <- children(terms, rels, rels[rels$term1 == term, "term2"], lang, returnIds = FALSE);}
  return(sibs);
}
ancestry <- function(terms = tm, rels = rel, term, lang = displayLg, origin = orig, returnIds = TRUE) {
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
browse <- function(terms = tm, rels = rel, term = orig, langs, lg = "la", origin = orig){
  cat("Term ID: ", term, "\n");
  for(lang in langs){ cat(lang, ": ", as.character(terms[terms$term_id == term & terms$lang == lang, "term"]), "\n"); }
  cat("Ancestry: ", "\n");
  parents <- ancestry(terms, rels, as.integer(term), lg, origin);
  if(length(parents) == 0) { cat("No hierarchical path available in your loaded data frame.", "\n"); }
  else {
    for(i in length(parents):2) {
      cat(as.character(terms[terms$term_id == parents[i] & terms$lang == lg, "term"]), "(id: ", parents[i], ")", " -> ");
    }
    cat(as.character(terms[terms$term_id == parents[1] & terms$lang == "la", "term"]), "(id: ", parents[1], ")", "\n");
  }
  cat("Children:", "\n");
  kids <- children(terms, rels, term = term, lang = lg);
  if(length(kids) == 0) { cat("No children.", "\n");}
  else {
    for(i in 1:(length(kids)-1)){
      cat(as.character(terms[terms$term_id == kids[i] & terms$lang == lg, "term"]), "(id:", kids[i], "); ");
    }
    cat(as.character(terms[terms$term_id == kids[length(kids)] & terms$lang == lg, "term"]), "(id:", kids[length(kids)], ")", "\n");
  }
  cat("Siblings: ", "\n");
  sibs <- siblings(terms, rels, term);
  if(length(sibs) == 1) { cat("No siblings.", "\n");}
  else {
    for(i in 1:(length(sibs)-1)){
      cat(as.character(terms[terms$term_id == sibs[i] & terms$lang == lg, "term"]), "(id:", sibs[i], "); ");
    }
    cat(as.character(terms[terms$term_id == sibs[length(sibs)] & terms$lang == lg, "term"]), "(id:", sibs[length(sibs)], ")", "\n");
  }
}
search <- function(term, terms = tm, rels = rel, output = 0, lang = displayLg) {
  rows <- agrep(term, terms[,"term"], ignore.case = TRUE);
  termIds <- c();
  langs <- c();
  for(row in rows) {
    termIds <- c(termIds, as.numeric(terms[row, "term_id"]));
    langs <- c(langs, as.character(terms[row, "lang"]));
  }
  if(output == 0) { return(termIds); }
  else if(output == 1) { return(as.character(terms[terms$term_id %in% termIds, "term"])); }
  else if(output == 2) {
    showTerms(termIds, lang, terms, rels);
  }
  else { cat("No valid output value."); }
  
}
showTerms <- function(termIds, lang = displayLg, terms = tm, rels = rel) {
  for(termId in termIds) {
    cat("id:", termId, ", ", lang, ": ", as.character(terms[terms$term_id == termId & terms[terms$lang == lang], "term"]), "/n");
  }
}
newTranslation <- function(lang, terms = tm, rels = rel, origin = orig){
  cat("If you are starting a new translation enter: 1" , "\n", "If you are continuing a started process enter: 2", "\n");
  start <- as.numeric(readLines(n=1));
  if(start == 1){
    langs <- terms[terms$term_id == origin, "lang"];
    outputT <- data.frame();
    outputT["term_id"] <- terms[ terms$lang == langs[1], "term_id"];
    for(lg in langs) { outputT[lg] <- term[, term$lang == lg]; }
    outputT[lang] <- c();
    outputT["source"] <- c();
    outputT["page"] <- c();
    write.csv(outputT, "terms.csv");
    cat("terms.csv has been created in the work directory. It contains the official translations in the selected languages.", "\n");
    cat("Replace terms.csv in your working directory after finishing the translation.", "\n");
    cat("Return to \"Add new language translation\" option and continue the started process");
  }
  else {
    cat("Press Enter to continue. Be sure you have replaced the old terms.csv with the new one.");
    op <- readLines(n=1);
    newTerms <- read.csv("terms.csv");
    termsLa <- data.frame(t(rep(NA,length(names(terms)))));
    names(termsLa)<-names(terms);
    termsLa<-termsLa[-1,];
    termsLa[]
    for(row in row.names(newTerms)) {
      termsLa[]
    }
  }
}
csvtodf <- function(df, lang = "ro", langNo = 2, off = FALSE) {
  if(off == FALSE){
    samplet <- data.frame(t(rep(NA,length(names(tm))+1)));
    names(samplet)<-c(names(tm), "pages");
    samplet<-samplet[-1,];
    srow <- 1;
    for(row in row.names(df)) {
      nrcol <- 1;
      for(col in (langNo +2): length(names(df))) {
        if(nrcol %% 2 == 1) {
          samplet[srow,] <- c(srow, NA, as.numeric(df[row, "id"]), lang, NA, NA, as.character(df[row, names(df)[col]]),
                              names(df)[col], NA, NA, NA, as.character(df[row, names(df)[col+1]]));
          srow <- srow + 1;
        }
        nrcol <- nrcol +1;
      }
    }
    samplet <- separateTerms(samplet);
    return(samplet);
  }
  else {
    term_id <- c(as.character(df[,"id"]), as.character(df[,"id"]));
    term <- c(as.character(df[,"la"]), as.character(df[,"en"]));
    pages <- rep(NA, length(term_id));
    id <- c();
    for(i in 1: length(term_id)) { id <- c(id, i); }
    upd <- rep(NA, length(term_id));
    lang <- c(rep("la", length(term_id)/2), rep("en", length(term_id)/2));
    part_speech <- rep(NA, length(term_id));
    gender <- rep(NA, length(term_id));
    description <- rep(NA, length(term_id));
    wiki <- rep(NA, length(term_id));
    email <- rep(NA, length(term_id));
    source <- rep(NA, length(term_id));
    df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
    return(df2);
  }
}
separateTerms <- function(df, sources = TRUE) {
  for(row in row.names(df)) {
    if(as.character(df[row, "term"]) != "") {
      nosep <- length(grep(";", as.character(df[row, "term"]), fixed = TRUE));
      if(nosep > 0) {
        synon <- unlist(strsplit(as.character(df[row,"term"]), c(";", " ;", " ; ", "; "), fixed=TRUE));
        if(sources == TRUE) {
          pgs <- unlist(strsplit(as.character(df[row,"pages"]), c(";", " ;", " ; ", "; "), fixed=TRUE));
          if(length(pgs) == 1) {
            pgs <- rep(pgs, nosep+1);
          }
        }
        #str(synon);
        #str(pgs);
        for(i in 2:length(synon)) {
          end <- length(row.names(df));
          df[end + 1, ] <- df[row,];
          df[end + 1, "term"] <- synon[i];
          if(sources == TRUE) { df[end + 1, "pages"] <- pgs[i]; }
        }
        df[row, "term"] <- synon[1];
        if(sources == TRUE) { df[row, "pages"] <- pgs[1]; }
      }
    }
  }
  return(df);
}
referenceTrans <- function(df){
  lang <- as.character(df[1,"lang"]);
  termids <- unique(df[, "term_id"]);
  row <- length(row.names(df)) + 1;
  for(termid in termids) {
    concepts <- as.character(df[df$term_id == termid & df$lang == lang & as.character(df$term) != "", "term"]);
    no <- 0;
    no <- rep(no, length(unique(concepts)));
    names(no) <- unique(concepts);
    for(concept in concepts) {
      no[concept] <- no[concept] + 1;
    }
    max <- max(no);
    for(i in names(no)) { if(no[i] == max) { term <- i; }}
    df[row,] <- c(row, NA, termid, lang, NA, NA, term, "reference", NA, NA, NA, NA);
    row <- row + 1;
  }
  return(df);
}
matchid <- function(sample = sample500){
  for(row in 1:length(row.names(sample))){
    id <- as.numeric(tm[tm$lang == "t" & tm$term == as.character(sample[row, "t"]), "term_id"]);
    if(length(id) != 1) {
      if(length(id) == 2 && id[1] == id[2]) { id <- id[1]; }
      else {
        cat(id);
        id <- as.numeric(readLines(n=1));
      }
    }
    sample[row, "id"] <- id;
  }
  return(sample);
}
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
  cat("1 Browse data", "\n");
  cat("2 Update data", "\n"); #modify data
  cat("3 Insert new data", "\n"); #modify data
  cat("4 Translate terms or text", "\n");
  cat("5 Verify terms in text", "\n");
  cat("6 Add new language translation", "\n");
  cat("Write number: ");
  opt <- readLines(n=1);
  if(opt == 1 || opt == 2 || opt == 3 || opt == 4) { return(opt); }
  else {
    cat("Invalid choice. Write one of the above numbers: ");
    return(startOptions());
  }
}
ontobrowse <- function() {
    opt <- startOptions();
    if(opt == 4) {
      return(cat("User interface exited. Check documentation for direct function use."));
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
    
    # initialise data frames
    cat("Loading data ...");
    initialise(allTerms, allRel, id, langs);
    cat("Enter preferred language for display: ");
    lg <- readLines(n=1);
    cho <- 0;
    while(cho == 0) {
      choice <- dataOptions();
      endbrowse <- 0;
      while(endbrowse == 0){
        if(choice == 1){ browse(tm, rel, id, langs, lg); }
        cat("Enter term ID, < to go back to options or ? to search for a term. Press Enter.");
        opt <- readLines(n=1);
        if(opt == "<") { endbrowse <- 1;}
        else if(opt == "?") {
          cat("Enter term/word: ");
          wo <- readLines(n=1);
          search(wo, output = 2);
          cat("Chosen term id: ");
          id <- as.numeric(readLines(n=1));
        }
        else {
          int <- as.integer(opt);
          if(length(terms[terms$term_id == int,]) != 0) { id <- int; }
          else { cat("Invalid id."); }
        }
        if(choice == 2) {
          browse(tm, rel, id, langs, lg);
          cat("Enter labels to be updated (term, lang, part_speech, gender, source, description, wiki, email), separated by space: ");
          labels <- readLines(n=1);
          cat("Enter new values, separated by space: ");
          values <- readLines(n=1);
          if("term" %in% labels) { oo<-0;
          }
          
        }
      }
      if(choice == 2) {
        cat("Enter term id or < (back) or ? (search)");
      }
      if(choice == 3) {}
      if(choice == 4) {}
      if(choice == 5) {}
      if(choice == 6) { 
        cat("Enter new language (2 letters): ");
        la <- readLines(n=1);
        newTranslation(la); }
    }  
    
}
graphdemo <- function(){
  samplet <- data.frame(t(rep(NA,length(names(tm)))));
  names(samplet)<-names(tm);
  samplet<-samplet[-1,];
  term_id <- c(13459, 13459, 13459, 13602, 13602, 13602, 10738, 10738, 10738);
  lang <- c("ro", "ro", "ro", "ro", "ro", "ro", "ro", "ro", "ro");
  term <- c("celulă în coșuleț", "celulă cu coșuleț", "neuron în coșuleț", "sistem nervos periferic", "sistem nervos periferic", "sistem nervos periferic", "adipocit", "celulă adipoasă", "adipocit");
  sources <- c(1, 2, 3, 1, 2, 3, 1, 2, 3);
  samplet <- data.frame(term_id, lang, term, sources);
  add <- c();
  for(so in sources) { add[so] <- 0;}
  for(row in row.names(samplet)) {
    if(as.character(samplet[row, "term"]) == as.character(samplet[samplet$term_id == samplet[row, "term_id"] & samplet$sources == 1, "term"])) {
      add[samplet[row,"sources"]] <- add[samplet[row,"sources"]] +1;
    }
  }
  ref <- add[1];
  for(elem in 1:length(add)){ add[elem] <- add[elem]*100/ref; }
  #prop = prop.table(add, margin = 2);
  g <- barplot(add, main = "Procentul de suprapunere a variantelor de traducere peste referință", 
               xlab = "Variantele de traducere", ylab = "Procent de suprapunere %", ylim = c(0, 100),
               names.arg = unique(sources), col = c("lightblue", "mistyrose", "lavender"),);
  text(g, 0, round(add, 2), cex=1, pos=3);
  return(g);
  
}
horizNoTerms <- function(df){
  add <- c();
  noconcepts <- c();
  #bysource <- c();
  sources <- factor(df[, "source"]);
  for(so in levels(sources)) {
    add[so] <- 0;
    noconcepts[so] <- length(unique(final[final$source == so & as.character(final$term) != "", "term_id"]));
    #bysource[so] <- character(0);
  }
  noconcepts <- sort(noconcepts, decreasing = TRUE);
  for(row in row.names(df)) {
    if(as.character(df[row, "term"]) != "" | length(as.character(df[row, "term"]) != 0)) {
      if(as.character(df[row, "term"]) == as.character(df[df$term_id == df[row, "term_id"] & df$source == "reference", "term"])) {
        add[df[row,"source"]] <- add[df[row,"source"]] +1;
        #bysource[df[row,"source"]] <- c(bysource[df[row,"source"]], as.character(df[row, "term"]));
      }
    }
  }
  correct <- c();
  for(so in names(noconcepts)) {
    correct[so] <- add[so];
  }
  data <- matrix(c(noconcepts, correct), nrow = 2, ncol = 6, byrow = TRUE,
                 dimnames = list(c("concepte găsite", "termeni referință incluși \nîn conceptele găsite"),
                                 as.character(names(noconcepts))));
  ycoord <- c();
  for(i in 1:6) { ycoord <- c(ycoord, data[1,i], data[2,i]); }
  #for(elem in 1:length(add)){ add[elem] <- add[elem]*100/ref; }
  #prop = prop.table(add, margin = 2);
#  g <- barplot(noconcepts, main = "Procentul de suprapunere a variantelor de traducere peste referință", 
#                xlab = "Variantele de traducere", ylab = "Procent de suprapunere %", ylim = c(0, 500),
#                names.arg = names(noconcepts), col = c("lightblue", "mistyrose", "lavender"),);
   title <- "Valori comparate ale conceptelor găsite și ale \ntermenilor referință incluși în acestea \npentru fiecare sursă";
   par(mar = c(5.1, 7.1, 5.1, 7.1), xpd = TRUE, las = 1);
   g <- barplot(data, main = title,
                xlab = "Numărul de concepte/termeni",
                ylab = "Sursele folosite pentru traducerea termenilor",
                col = c("lightblue", "mistyrose"), beside = TRUE, 
                horiz = TRUE, width = 2, ann = FALSE);
   legend("topright", inset = c(0, 0), fill = c("lightblue", "mistyrose"), 
          legend = rownames(data), xjust=1, yjust=1, cex = 0.9, pt.cex = 1);
   text(ycoord , g, round(ycoord, 2), cex=1, pos=4);
   #heat.colors(length(rownames(data)))
   return(g);
}
vertNoTerms <- function(df){
  add <- c();
  noconcepts <- c();
  sources <- factor(df[, "source"]);
  for(so in levels(sources)) {
    add[so] <- 0;
    noconcepts[so] <- length(unique(final[final$source == so & as.character(final$term) != "", "term_id"]));
  }
  noconcepts <- sort(noconcepts, decreasing = TRUE);
  for(row in row.names(df)) {
    if(as.character(df[row, "term"]) != "" | length(as.character(df[row, "term"]) != 0)) {
      if(as.character(df[row, "term"]) == as.character(df[df$term_id == df[row, "term_id"] & df$source == "reference", "term"])) {
        add[df[row,"source"]] <- add[df[row,"source"]] +1;
      }
    }
  }
  correct <- c();
  for(so in names(noconcepts)) { correct[so] <- add[so]; }
  data <- matrix(c(noconcepts, correct), nrow = 2, ncol = 6, byrow = TRUE,
                 dimnames = list(c("concepte găsite", "termeni referință incluși \nîn conceptele găsite"),
                                 as.character(names(noconcepts))));
  ycoord <- c();
  for(i in 1:6) { ycoord <- c(ycoord, data[1,i], data[2,i]); }
  title <- "Valori comparate ale conceptelor găsite și ale \ntermenilor referință incluși în acestea \npentru fiecare sursă";
  par(mar = c(5.1, 7.1, 5.1, 7.1), xpd = TRUE, las = 1);
  g <- barplot(data, main = title,
               xlab = "Sursele folosite pentru traducerea termenilor",
               ylab = "Numărul de concepte/termeni",
               col = c("lightblue", "mistyrose"), beside = TRUE, width = 2);
  leg <- legend("topright", inset = c(0, 0), fill = c("lightblue", "mistyrose"), 
         legend = rownames(data), xjust=1, yjust=0, cex = 0.9, pt.cex = 1);
  text(g, ycoord, round(ycoord, 2), cex=1, pos=3);
  #heat.colors(length(rownames(data)))
  return(g);
}
vertSinTerms <- function(df){
  
}
comp <- function(df1,df2){
  for(row in length(row.names(df1))) {
    if(df1[row,] != df2[row,]) {
      str(df1[row,]);
      cat("\n");
      str(df2[row,]);
      cat("\n ; ");
    }
  }
}
cleardf <- function(df) {
  for(row in 1:length(row.names(df))) {
    for(col in names(df)) {
      df[row, col] <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", df[row, col]);
    }
  }
  return(df);
}
clearoff <- function(df) {
  df[,"term"] <- sub("(", "", df[,"term"], fixed=TRUE);
  df[,"term"] <- sub(")", "", df[,"term"], fixed=TRUE);
  #df[,"term"] <- sub("\{[[:alnum:]]\}", "", df[,"term"], fixed=TRUE);
}
listtodf <- function(df, list, lang, sourc = NA, termdf = NA) {
  termids <- names(list);
  term <- character(0);
  term_id <- c();
  no <- 1;
  pages <- character(0);
  for(termid in termids) {
    if(is.list(list[[as.character(termid)]]) == TRUE) {
      for(i in 1: length(list[[as.character(termid)]])) {
        for(j in 1: length(list[[as.character(termid)]][[i]])) {
          term <- c(term, list[[as.character(termid)]][[i]][[j]]);
          term_id <- c(term_id, termid);
#           if(length(termdf) > 1){ #!pages=NA for reference
#             if(!is.na(source)) {
#               pages[no] <- c(pages, as.character(termdf[termdf$term_id == termid & termdf$lang == lang & termdf$source == sourc, "pages"]));
#               no <- no + 1;
#             }
#           }
        }
      }
    }
    else {
      for(i in 1: length(list[[as.character(termid)]])) {
        term <- c(term, list[[as.character(termid)]][i]);
        term_id <- c(term_id, termid);
#         if(length(termdf) > 1){ #!pages=NA for reference
#           if(!is.na(sourc)) {
#             pages[no] <- c(pages, as.character(termdf[termdf$term_id == termid & termdf$lang == lang & termdf$source == sourc, "pages"]));
#             no <- no + 1;
#           }
#         }
      }
    }
  }
  #if(length(termdf) == 0 || is.na(sourc)) { pages <- rep(NA, length(term)); }
  pages <- rep(NA, length(term)); #!pages=NA for reference
  id <- c();
  for(i in (length(row.names(df)) + 1) : (length(row.names(df)) + length(term)) ) { id <- c(id, i); }
  upd <- rep(NA, length(term));
  lang <- rep(lang, length(term));
  part_speech <- rep(NA, length(term));
  gender <- rep(NA, length(term));
  description <- rep(NA, length(term));
  wiki <- rep(NA, length(term));
  email <- rep(NA, length(term));
  source <- rep(sourc, length(term));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  df <- rbind(df, df2);
  return(df);
}
noElemList <- function(List, byElem = FALSE){
  no <- 0;
  n <- 0;
  noo <- c();
  for(i in 1: length(List)) {
    if(is.list(List[[i]]) == TRUE) { n <- noElemList(List[[i]], byElem = FALSE); no <- no + n; }
    else { n <- length(List[[i]]); no <- no + n; }
    if(byElem == TRUE){  noo <- c(noo, n); }
  }
  if(byElem == TRUE){ return(noo); }
  else {return(no);}
}
appenddf <- function(df, bigdf, lg = "la", sourc = NA, term_id = c()) {
  term <- as.character(df[, "V1"]);
  pages <- rep(NA, length(term));
  id <- c();
  for(i in (length(row.names(bigdf)) + 1) : (length(row.names(bigdf)) + length(term))) { id <- c(id, i); }
  upd <- rep(NA, length(term));
  lang <- rep(lg, length(term));
  part_speech <- rep(NA, length(term));
  gender <- rep(NA, length(term));
  description <- rep(NA, length(term));
  wiki <- rep(NA, length(term));
  email <- rep(NA, length(term));
  source <- rep(sourc, length(term));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  df <- rbind(bigdf, df2);
  return(df);
}
wordMatchCsv <- function(offTerms, newTerms, words, offLang = "la", newLang = "ro", sourc = "reference", baseTransl="gtLaRoWords"){
  termids <- as.integer(levels(offTerms[offTerms$lang == offLang, "term_id"]));
  term_id <- c();
  oTerms <- c();
  owo <- words[words$lang == offLang,];
  oWords <- as.character(words[order(owo[,"term_id"]),"term"]);
  basedf <- words[words$lang == newLang & words$source == baseTransl,];
  base <- as.character(basedf[order(basedf[,"term_id"]),"term"]);
  matchWords <- rep("", length(oWords));
  maxWo <- 0;
  for(id in termids){
    nn <- newTerms[newTerms$lang == newLang & newTerms$source == sourc & newTerms$term_id == id, "term"];
    noWo <- length(unlist(strsplit(newTerms[newTerms$lang == newLang & newTerms$source == sourc & newTerms$term_id == id, "term"], " ")));
    if(maxWo < noWo) { maxWo <- noWo; }
  }
  newList <- list();
  newList[[1]] <- rep("", length(oWords));
  for(col in 2:maxWo){ newList[[col]] <- rep("", length(oWords)); }
  for(id in termids) {
    terms <- as.character(offTerms[offTerms$lang == offLang & offTerms$term_id == id, "term"]);
    for(no in 1:length(terms)){
      wo <- length(unlist(strsplit(terms[no], " ")));
      term_id <- c(term_id, rep(id, wo));
      oTerms <- c(oTerms, rep(terms[no], wo));
      nTerms <- c(nTerms, rep(as.character(newTerms[newTerms$term_id == id & newTerms$lang == newLang & newTerms$source == sourc, "term"]), wo));
      nwo <- as.character(words[words$term_id == id & words$source == sourc & words$lang == newLang, "term"]);
      for(col in 1:length(nwo)){
        for(row in 1:wo){ newList[[as.numeric(col)]][[as.numeric(length(oTerms)-wo+row)]] <- nwo[col]; }
      }
    }
  }
  sheet <- data.frame(term_id, oTerms, nTerms, base, oWords, matchWords, newList);
  for(col in names(sheet)) { sheet[,col] <- as.character(sheet[,col]);}
  sheet[,"term_id"] <- as.integer(sheet[,"term_id"]);
  for(row in row.names(sheet)){
    for(i in (length(names(sheet)) - maxWo + 1 ) : length(names(sheet))){
      if(length(agrep(sheet[row, "base"], sheet[row, i], max.distance = 0.5)) != 0){
        sheet[row, "matchWords"] <- sheet[row, i];
      }
    }
  }
  return(sheet);
}
wordMatchCsv2 <- function(offTerms, newTerms, words, offLang = "la", newLang = "ro", sourc = "reference", baseTransl="gtLaRoWords"){
  termids <- as.integer(levels(offTerms[offTerms$lang == offLang, "term_id"]));
  term_id <- c();
  oTerms <- c();
  nTerms <- c();
  owo <- words[words$lang == offLang,];
  oWords <- as.character(words[order(owo[,"term_id"]),"term"]);
  base <- list();
  for(baseT in baseTransl){
    basedf <- words[words$lang == newLang & words$source == baseT,];
    base[[baseT]] <- as.character(basedf[order(basedf[,"term_id"]),"term"]);
  }
  matchWords <- rep("", length(oWords));
  maxWo <- 0;
  for(id in termids){
    nn <- newTerms[newTerms$lang == newLang & newTerms$source == sourc & newTerms$term_id == id, "term"];
    noWo <- length(unlist(strsplit(newTerms[newTerms$lang == newLang & newTerms$source == sourc & newTerms$term_id == id, "term"], " ")));
    if(maxWo < noWo) { maxWo <- noWo; }
  }
  newList <- list();
  newList[[1]] <- rep("", length(oWords));
  for(col in 2:maxWo){ newList[[col]] <- rep("", length(oWords)); }
  for(id in termids) {
    terms <- as.character(offTerms[offTerms$lang == offLang & offTerms$term_id == id, "term"]);
    wo2 <- 0;
    for(no in 1:length(terms)){
      wo <- length(unlist(strsplit(terms[no], " ")));
      wo2 <- wo2 + wo;
      term_id <- c(term_id, rep(id, wo));
      oTerms <- c(oTerms, rep(terms[no], wo));
      nTerms <- c(nTerms, rep(as.character(newTerms[newTerms$term_id == id & newTerms$lang == newLang & newTerms$source == "reference", "term"]), wo));
    }
    nwo <- as.character(words[words$term_id == id & words$source == sourc & words$lang == newLang, "term"]);
    if(length(nwo) != 0){
      for(col in 1:length(nwo)){
        for(row in 1:wo2){ 
          newList[[as.numeric(col)]][[as.numeric(length(oTerms)-wo2+row)]] <- nwo[col]; }
      }
    }
  }
  sheet <- data.frame(term_id, oTerms, referenceTerms = nTerms, base, oWords, matchWords, newList);
  for(col in names(sheet)) { sheet[,col] <- as.character(sheet[,col]);}
  sheet[,"term_id"] <- as.integer(sheet[,"term_id"]);
  for(row in row.names(sheet)){
    words <- as.character(sheet[row, (length(names(sheet)) - maxWo + 1 ) : length(names(sheet))]);
    if(length(which(words != "")) != 0){
      ind <- list();
      for(baseT in 1:length(names(base))){
        ind[baseT] <- c();
        if(length(sheet[row, names(base)[baseT]]) != 0 && sheet[row, names(base)[baseT]] != ""){
          ind[[baseT]] <- agrep(sheet[row, names(base)[baseT]], words, max.distance = 0.5, ignore.case=TRUE);
        }
      }  
      ind[[3]] <- agrep(sheet[row, "oWords"], words, max.distance = 0.5, ignore.case=TRUE);
      ind[[4]] <- which(nchar(words[which(words != "")]) == min(nchar(words[which(words != "")])));
      ind <- unlist(ind);
      ind <- table(ind);
      #cat(words, "\n");
      #print(ind);
      #cat("min(ind): ", min(ind), "\n");
      #cat("which(ind == min(ind)): ", which(ind == min(ind)),"\n");
      #cat("words[which(ind == min(ind)): ", words[which(ind == min(ind))[[1]]], "\n");
      #oola<-sheet[row, "oWords"];
      #oo<-words[which(ind == max(ind))[[1]]];
      choice <- as.integer(names(which(ind == max(ind))));
      if(length(choice) >1) {
        ok<-0;
        for(cho in choice) {
          if(words[cho] == sheet[row, "oWords"]){ ok <- cho; }
          else if(length(base) >1 && words[cho] == sheet[row, names(base)[2]]) { ok <- cho; }
          else if(length(base) == 1 && words[cho] == sheet[row, base]) { ok <- cho; }
        }
        if(ok == 0) { ok<- choice[1]; }
      }
      else { ok <- choice; }
      sheet[row, "matchWords"] <- words[ok];
    }
      
      
      
#      for(i in (length(names(sheet)) - maxWo + 1 ) : length(names(sheet))){
#        if(length(sheet[row, names(base)[baseT]]) != 0 && length(sheet[row, i]) != 0 && sheet[row, names(base)[baseT]] != "" && sheet[row, i] != ""){
#          if(length(agrep(sheet[row, names(base)[baseT]], sheet[row, i], max.distance = 0.5, ignore.case=TRUE)) != 0){
#            sheet[row, "matchWords"] <- sheet[row, i];
#          }
#        }
#      }
#    }
  }
  return(sheet);
}
woToTermsBase <- function(wodf, tmdf, offlang = "la", newLang = "ro", sourc = NA, base="base"){
  term_id <- c();
  term <- c();
  wodf[,"oTerms"] <- as.character(wodf[,"oTerms"]);
  wodf[,base] <- as.character(wodf[,base]);
  for(row in row.names(wodf)){
    if(wodf[row, "term_id"] %in% term_id == FALSE) {
      termid <- wodf[row, "term_id"];
      ter <- unique(wodf[wodf$term_id == termid, "oTerms"]);
      for(t in ter){
        term_id <- c(term_id, termid);
        term <- c(term, paste(wodf[wodf$term_id == termid & wodf$oTerms == t, base], collapse=" "));
      }
    } 
  }
  pages <- rep(NA, length(term_id));
  id <- c();
  for(i in (length(row.names(tmdf)) + 1) : (length(row.names(tmdf)) + length(term_id))) { id <- c(id, i); }
  upd <- rep(NA, length(term_id));
  lang <- rep(newLang, length(term_id));
  part_speech <- rep(NA, length(term_id));
  gender <- rep(NA, length(term_id));
  description <- rep(NA, length(term_id));
  wiki <- rep(NA, length(term_id));
  email <- rep(NA, length(term_id));
  source <- rep(sourc, length(term_id));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  df <- rbind(tmdf, df2);
  return(df);
}
woToTermsRef <- function(wodf, tmdf, offlang = "la", newLang = "ro", sourc = NA){
  term_id <- c();
  term <- c();
  wodf[,"oTerms"] <- as.character(wodf[,"oTerms"]);
  wodf[,"matchWords"] <- as.character(wodf[,"matchWords"]);
  for(row in row.names(wodf)){
    if(wodf[row, "term_id"] %in% term_id == FALSE) {
      termid <- wodf[row, "term_id"];
      ter <- unique(wodf[wodf$term_id == termid, "oTerms"]);
      tmm <- c();
      noW <- c();
      for(t in ter){
        tmm <- c(tmm, paste(wodf[wodf$term_id == termid & wodf$oTerms == t & wodf$matchWords != "", "matchWords"], collapse=" "));
        noW <- c(noW, length(unlist(strsplit(wodf[wodf$term_id == termid & wodf$oTerms == t & wodf$matchWords != "", "matchWords"]," "))));
      }
      term <- c(term, unique(tmm[which(noW == max(noW))]));
      term_id <- c(term_id, termid);
    } 
  }
  pages <- rep(NA, length(term_id));
  id <- c();
  for(i in (length(row.names(tmdf)) + 1) : (length(row.names(tmdf)) + length(term_id))) { id <- c(id, i); }
  upd <- rep(NA, length(term_id));
  lang <- rep(newLang, length(term_id));
  part_speech <- rep(NA, length(term_id));
  gender <- rep(NA, length(term_id));
  description <- rep(NA, length(term_id));
  wiki <- rep(NA, length(term_id));
  email <- rep(NA, length(term_id));
  source <- rep(sourc, length(term_id));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  df <- rbind(tmdf, df2);
  return(df);
}
autoTranslRef <- function(match){
  synon <- synoW(match);
  synonDif <- synDif(synon);
  match[,"matchWords"] <- as.character(match[,"matchWords"]);
  for(dif in synonDif){
    cat(dif, "\n");
    ind <- table(synon[[dif]]);
    choices <- names(which(ind == max(ind)));
    if(length(choices) > 1) {
      i <- agrep(dif, choices, max.distance = 0.5, ignore.case=TRUE);
      if(length(i) != 0) {
        if( length(i) == 1) { ok <- choices[i]; }
        else { ok <- choices[which(nchar(choices) == min(nchar(choices)))];
               if(length(ok) > 1) { ok<- choices[1]; }}
      }
    } else { ok <- choices; }
    match[match$matchWords == dif, "matchWords"] <- ok;
    cat(ok, "\n");
  }
  return(match);
}
automaticTransl <- function(offTerms, terms, words, matchwo, offLang = "la", newLang = "ro", sources){
  term_id <- c();
  oTerms <- c();
  oWords <- c();
  matchWords <- c();
  referenceMatch <- c();
  words[,"term"] <- is.character(words[,"term"]);
  words[,"lang"] <- is.character(words[,"lang"]);
  uniqWo <- unique(words[words$lang == offLang, "term"]);
  uniqList <- list();
  for(wo in uniqWo){
    uniqList[[wo]] <- list();
    uniqList[[wo]][["id"]] <-c();
    uniqList[[wo]][["oTerms"]] <-c();
    uniqList[[wo]][["oWords"]] <-c();
    uniqList[[wo]][["RefTerms"]] <-c();
    uniqList[[wo]][["RefWords"]] <-c();
    uniqList[[wo]][["matchWords"]] <-c();
    for(i in 1:length(sources)) {
    uniqList[[wo]][[sources[i]]] <-c();
    }
  }
  for(wo in uniqWo) {
    uniqList[[wo]][["id"]] <- as.character(words[words$term == wo & words$lang == offLang, "term_id"]);
    uniqList[[wo]][["oTerms"]] <- as.character(offTerms[offTerms$term_id %in% uniqList[[wo]][["id"]] & offTerms$lang == offLang, "term"]);
    uniqList[[wo]][["oWords"]] <- as.character(words[words$term_id %in% uniqList[[wo]][["id"]] & words$lang == "la", "term"]);
    uniqList[[wo]][["RefTerms"]] <- as.character(terms[terms$term_id %in% uniqList[[wo]][["id"]] & terms$source == "reference", "term"]);
    uniqList[[wo]][["RefWords"]] <- as.character(words[words$term_id %in% uniqList[[wo]][["id"]] & words$source == "reference", "term"]);
    for(i in 1:length(sources)){
      wos <- as.character(words[words$term_id %in% uniqList[[wo]][["id"]] & words$source == "junqueira", "term"]);
      ok <- c();
      ind <- c();
      for(s in wos){
        
        }
        
      }
    }
   
    uniqList[[wo]][["matchWords"]]
    agrep(sheet[row, "base"], sheet[row, i], max.distance = 0.5)
  
  df <- data.frame(refT, refW, offT, offW, matchW, junqueira, tm2009, tm2004, buc1987, craiova2006, sourceTerms);
  return(df);
}
autoTranslWordList <- function(terms, words, matchwo, offLang = "la", newLang = "ro", sources){
  uniqWo <- unique(as.character(words[words$lang == offLang, "term"]));
  uniq <- list();
  for(wo in uniqWo){
    uniq[[wo]] <- list();
    uniq[[wo]][["term_id"]] <- c();
    uniq[[wo]][["match"]] <- c();
    uniq[[wo]][["ind"]] <- c();
    uniq[[wo]][["ok"]] <- "";
  }
  for(id in unique(as.character(words[words$lang == offLang, "term_id"]))){
    wordsO <- as.character(words[words$lang == offLang & words$term_id == id, "term"]);
    wordsId <- c();
    ind <- c();
    for(so in sources){
      wordsId <- c(wordsId, as.character(words[words$lang == newLang & words$term_id == id & words$source == so & words$term != "", "term"]));
    }
    for(i in 1: length(wordsO)){
      wordRef <- as.character(matchwo[matchwo$term_id == id & matchwo$oWords == wordsO[i] & matchwo$matchWords != "", "matchWords"]);
      wordG <- as.character(matchwo[matchwo$term_id == id & matchwo$oWords == wordsO[i], "base"]);
      agr <- c();
      if(length(wordG) != 0){
        if(length(wordG) > 1) { wordG <- paste(wordG, sep = " ", collapse = " "); }
        agrG <- agrep(wordG, wordsId, max.distance = 0.4);
        if(length(agrG) != 0) { agr <- c(agr, agrG); }
      }
      for(diff in 1:5){
        agrO <- agrep(wordsO[i], wordsId, max.distance = 0.1*diff);
        if(length(agrO) != 0) { agr <- c(agr, rep(agrO, 6-diff)); }
        if(length(wordRef) != 0) {
          if(length(wordRef) > 1) { wordRef <- paste(wordRef, sep = " ", collapse = " "); }
          agrR <- agrep(wordRef, wordsId, max.distance = 0.1*diff);
          if(length(agrR) != 0) { agr <- c(agr, rep(agrR, as.integer(3-diff/2))); }
        }
      }
      indi <- unique(agr);
      for(l in indi){
        same <- grep(wordsId[l], wordsId, fixed=TRUE);
        if(length(same) > 1){
          
        }
      }
      if(length(agr) > 0){
        agrt <- table(agr);
        for(ag in names(agrt)){
          if(wordsId[as.integer(ag)] %in% uniq[[wordsO[i]]][["match"]]){
            pos <- which(uniq[[wordsO[i]]][["match"]] == wordsId[as.integer(ag)]);
            if(length(pos) > 1) { pos <- pos[1]; }
            uniq[[wordsO[i]]][["ind"]][[pos]] <- uniq[[wordsO[i]]][["ind"]][[pos]] + agrt[[ag]];
          } else {
            uniq[[wordsO[i]]][["match"]] <- c(uniq[[wordsO[i]]][["match"]], wordsId[as.integer(ag)]);
            uniq[[wordsO[i]]][["ind"]] <- c(uniq[[wordsO[i]]][["ind"]], agrt[[ag]]);
          }
        }
      }
      if(length(uniq[[wordsO[i]]][["match"]]) != 0){
        pos <- which(nchar(uniq[[wordsO[i]]][["match"]]) == min(nchar(uniq[[wordsO[i]]][["match"]])));
        uniq[[wordsO[i]]][["ind"]][pos] <- uniq[[wordsO[i]]][["ind"]][pos] + 2;
      }
      if(length(uniq[[wordsO[i]]][["match"]]) == 0){
        agrA <- c();
        for(difff in 6:10){
          agrA <- c(agrA, agrep(wordsO[i], wordsId, max.distance = 0.1*difff));
          if(length(agrA) != 0) {
            
            uniq[[wordsO[i]]][["match"]] <- wordsId[agrA];
            uniq[[wordsO[i]]][["ind"]] <- agrA;
          }
          else {
            uniq[[wordsO[i]]][["match"]] <- wordsId[1];
            uniq[[wordsO[i]]][["ind"]] <- 1;
          }
        }
      }
      uniq[[wordsO[i]]][["term_id"]] <- c(uniq[[wordsO[i]]][["term_id"]], id);
    }
  }
  for(wo in uniqWo){
    pos <- which(uniq[[wo]][["ind"]] == max(uniq[[wo]][["ind"]]));
    if(length(pos) > 1){
        inc <- 1;
        kk <- "";
        while(length(kk) == 0 && inc < 10){
          for(p in pos){
            kk <- agrep(wo, uniq[[wo]][["match"]][[p]], max.distance = 0.1*inc);
            if(length(kk) != 0) { pos <- p;}
          }
          inc <- inc + 1;
        }
      if(length(pos) > 1) {
        min <- 100;
        k <- 0;
        for(p in pos){ if(nchar(uniq[[wo]][["match"]][[p]])<min) { k <- p; }}
        pos <- k;
      }
    }
    uniq[[wo]][["ok"]] <- uniq[[wo]][["match"]][[pos]];
  }
  return(uniq);
}
autoTransW <- function(List){
  words <- c();
  for(wo in names(List)){
    words <- c(words, List[[wo]][["ok"]]);
  }
  return(words);
}
autoTransT <- function(List, words, terms, offLang = "la", newLang = "ro"){
  term_id <- unique(as.character(words[words$lang == offLang, "term_id"]));
  term <- c();
  for(id in term_id){
    wo <- as.character(words[words$lang == offLang & words$term_id == id, "term"]);
    tt <- as.character(terms[terms$lang == offLang & terms$term_id == id, "term"]);
    i <- 1;
    ok <- c();
    alt <- c();
    for(ter in 1:length((tt))){
      t <- c();
      for(w in 1:length(unlist(strsplit(tt[ter]," ")))){
        t <- c(t, List[[wo[i]]][["ok"]]);
        i <- i + 1;
      }
      t <- paste(t, sep = " ", collapse = " ");
      if(t %in% as.character(terms[terms$lang == newLang & terms$term_id == id, "term"])){
        ok[ter] <- t;
      }
      else { alt[ter] <- t; }
    }
    if(length(ok) != 0){
      term <- c(term, ok[1]);
    }
    else { term <- c(term, alt[1]); }
  }
  pages <- rep(NA, length(term_id));
  id <- c();
  for(i in 1:length(term_id)) { id <- c(id, i); }
  #for(i in (length(row.names(tmdf)) + 1) : (length(row.names(tmdf)) + length(term_id))) { id <- c(id, i); }
  upd <- rep(NA, length(term_id));
  lang <- rep(newLang, length(term_id));
  part_speech <- rep(NA, length(term_id));
  gender <- rep(NA, length(term_id));
  description <- rep(NA, length(term_id));
  wiki <- rep(NA, length(term_id));
  email <- rep(NA, length(term_id));
  source <- rep("auto", length(term_id));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  #df <- rbind(tmdf, df2);
  return(df2);
}
autoMatch <- function(match, auto, words){
  c <- which(names(match) == "matchWords");
  for(col in (c+1):length(names(match))){
    match[,col] <- as.character(match[,col]);
  }
  match[,"matchWords"] <- as.character(match[,"matchWords"]);
  max <- 0;
  for(i in 1:length(auto)){
    if(length(unique(auto[[i]][["match"]])) > max) { max <- length(unique(auto[[i]][["match"]])); }
  }
  end <- length(names(match));
  if(max > end) {
    for(i in (end+1):max){
      col <- rep(NA, length(row.names(match)));
      cbind(match, col);
    }
    end <- max;
  }
  for(row in row.names(match)){
    wordO <- as.character(match[row, "oWords"]);
    replace <- auto[[wordO]][["ok"]];
    if(length(replace) != 0) {
      match[row, "matchWords"] <- replace;
    }
    i <- 1;
    sinon <- unique(auto[[wordO]][["match"]]);
    if(length(sinon) == 0){
      sinon <- as.character(words[words$term_id == as.character(match[row,"term_id"]) & words$lang == "ro", "term"]);
    }
    for(col in (c+1):end){
      match[row,col] <- sinon[i];
      i <- i + 1;
    }
  }
  return(match);
}
autoReduce <- function(terms, sourc = "autoOk", so = "autoRed", newLang = "ro"){
  term_id <- unique(as.character(terms[,"term_id"]));
  term <- c();
  for(id in term_id){
    tt <- as.character(terms[terms$term_id == id & terms$source == sourc, "term"]);
    if(length(tt) > 1){
      max <- 0; ind <- c();
      for(t in tt){
        if(length(unlist(strsplit(t, " "))) > max) { max <- length(unlist(strsplit(t, " "))); ok <- t;}
        if(t %in% as.character(terms[terms$term_id == id, "term"])){ ind[t] <- 1; }
      }
      tt <- ok;
    }
    term <- c(term, tt);
  }
  pages <- rep(NA, length(term_id));
  id <- c();
  for(i in (length(row.names(terms)) + 1) : (length(row.names(terms)) + length(term_id))) { id <- c(id, i); }
  upd <- rep(NA, length(term_id));
  lang <- rep(newLang, length(term_id));
  part_speech <- rep(NA, length(term_id));
  gender <- rep(NA, length(term_id));
  description <- rep(NA, length(term_id));
  wiki <- rep(NA, length(term_id));
  email <- rep(NA, length(term_id));
  source <- rep(so, length(term_id));
  df2 <- data.frame(id, upd, term_id, lang, part_speech, gender, term, source, description, wiki, email, pages);
  df <- rbind(terms, df2);
  return(df);
}
compareRatioWT <- function(oTerms, terms, words, offLang = "la", newLang = "ro", sources){
  noWords <- c();
  noWordsQ <- c();
  noTerms <- c();
  noTermsQ <- c();
  ratioWT <- c();
  ratioWTQ <- c();
  for(lg in c("la", "en")) {
    noWords[lg] <- length(as.character(words[words$lang == lg, "term"]));
    noWordsQ[lg] <- length(unique(as.character(words[words$lang == lg, "term"])));
    noTerms[lg] <- length(as.character(oTerms[oTerms$lang == lg, "term"]));
    noTermsQ[lg] <- length(unique(as.character(oTerms[oTerms$lang == lg, "term"])));
    ratioWT[lg] <- noWords[lg]/noTerms[lg];
  }
  for(so in sources){
    if(so == "reference"){
      noWords[so] <- length(as.character(words[words$lang == newLang & words$source == so, "term"]));
      noWordsQ[so] <- length(unique(as.character(words[words$lang == newLang & words$source == so, "term"])));
      noTerms[so] <- length(as.character(terms[terms$lang == newLang & terms$source == so, "term"]));
      noTermsQ[so] <- length(unique(as.character(terms[terms$lang == newLang & terms$source == so, "term"])));
    }
    if(so == "referenceWordsLa") {
      noWords[so] <- length(as.character(words[words$lang == newLang & words$source == "reference", "term"]));
      noWordsQ[so] <- length(unique(as.character(words[words$lang == newLang & words$source == "reference", "term"])));
      noTerms[so] <- length(as.character(terms[terms$lang == newLang & terms$source == so, "term"]));
      noTermsQ[so] <- length(unique(as.character(terms[terms$lang == newLang & terms$source == so, "term"])));
    }
    if(length(grep("^gt", so) != 0)) {
      noWords[so] <- length(as.character(words[words$lang == newLang & words$source == sub("Terms", "Words", so, fixed=TRUE), "term"]));
      noWordsQ[so] <- length(unique(as.character(words[words$lang == newLang & words$source == sub("Terms", "Words", so, fixed=TRUE), "term"])));
      noTerms[so] <- length(as.character(terms[terms$lang == newLang & terms$source == so, "term"]));
      noTermsQ[so] <- length(unique(as.character(terms[terms$lang == newLang & terms$source == so, "term"])));
    }
    ratioWT[so] <- noWords[so]/noTerms[so];
  }
  df<-data.frame(noTerms, noTermsQ, noWords, noWordsQ, ratioWT);
  return(df);
}
compareRatioWT2 <- function(oTerms, terms, words, offLang = "la", newLang = "ro", sourcesT, sourcesW){
  noWords <- c();
  noWordsQ <- c();
  noTerms <- c();
  noTermsQ <- c();
  ratioWT <- c();
  ratioWTQ <- c();
  for(lg in offLang) {
    noWords[lg] <- length(as.character(words[words$lang == lg, "term"]));
    noWordsQ[lg] <- length(unique(as.character(words[words$lang == lg, "term"])));
    noTerms[lg] <- length(as.character(oTerms[oTerms$lang == lg, "term"]));
    noTermsQ[lg] <- length(unique(as.character(oTerms[oTerms$lang == lg, "term"])));
    ratioWT[lg] <- noWords[lg]/noTerms[lg];
  }
  for(so in 1:length(sourcesT)){
    noWords[sourcesT[so]] <- length(as.character(words[words$lang == newLang & words$source == sourcesW[so], "term"]));
    noWordsQ[sourcesT[so]] <- length(unique(as.character(words[words$lang == newLang & words$source == sourcesW[so], "term"])));
    noTerms[sourcesT[so]] <- length(as.character(terms[terms$lang == newLang & terms$source == sourcesW[so], "term"]));
    noTermsQ[sourcesT[so]] <- length(unique(as.character(terms[terms$lang == newLang & terms$source == sourcesW[so], "term"])));
    ratioWT[sourcesT[so]] <- noWords[sourcesT[so]]/noTerms[sourcesT[so]];
  }
  df<-data.frame(noTerms, noTermsQ, noWords, noWordsQ, ratioWT);
  return(df);
}
compareSynoT <- function(oTerms, terms, offLang = "la", newLang = "ro", sources){
  noConcepts <- c();
  noTerms <- c();
  noConceptsWithSyno <- c();
  RatioSynoT <- c();
  SynoList <- list();
  minSynoT <- c();
  maxSynoT <- c();
  maxSynoTerm <- c();
  for(lg in offLang) {
    noConcepts[lg] <- length(unique(oTerms[oTerms$lang == lg, "term_id"]));
    noTerms[lg] <- length(oTerms[oTerms$lang == lg, "term_id"]);
    RatioSynoT[lg] <- noTerms[lg]/noConcepts[lg];
    SynoList[[lg]] <- c();
    for(id in unique(oTerms[oTerms$lang == lg, "term_id"])){
      SynoList[[lg]] <- c(SynoList[[lg]], length(oTerms[oTerms$lang == lg & oTerms$term_id == id,"term"]));
    }
    noConceptsWithSyno[lg] <- length(as.character(unique(oTerms[oTerms$lang == lg, "term_id"])[which(SynoList[[lg]] > 1)]));
    minSynoT[lg] <- min(SynoList[[lg]]);
    maxSynoT[lg] <- max(SynoList[[lg]]);
    syno <- as.character(unique(oTerms[oTerms$lang == lg, "term_id"])[which(SynoList[[lg]] == maxSynoT[lg])]);
    sy <- c();
    for(syid in syno) {
      sy <- c(sy, paste(as.character(oTerms[oTerms$term_id == syid & oTerms$lang == lg, "term"]), collapse=", "));
    }
    maxSynoTerm[lg] <- paste(unique(sy), collapse = " ; ", sep = " ; ");
  }
  for(so in sources){
    noConcepts[so] <- length(unique(terms[terms$lang == newLang & terms$source == so & terms$term != "", "term_id"]));
    noTerms[so] <- length(terms[terms$lang == newLang & terms$source == so & terms$term != "", "term_id"]);
    RatioSynoT[so] <- noTerms[so]/noConcepts[so];
    SynoList[[so]] <- c();
    if(noConcepts[so] != noTerms[so]) {
      for(id in unique(terms[terms$lang == newLang & terms$source == so, "term_id"])){
        SynoList[[so]] <- c(SynoList[[so]], length(terms[terms$term_id == id & terms$source == so,"term"]));
      }
      noConceptsWithSyno[so] <- length(as.character(unique(terms[terms$lang == newLang & terms$source == so, "term_id"])[which(SynoList[[so]] > 1)]));
      minSynoT[so] <- min(SynoList[[so]]);
      maxSynoT[so] <- max(SynoList[[so]]);
      syno <- as.character(unique(terms[terms$lang == newLang & terms$source == so, "term_id"])[which(SynoList[[so]] == maxSynoT[so])]);
      sy <- c();
      for(syid in syno) {
        sy <- c(sy, paste(as.character(terms[terms$lang == newLang & terms$source == so & terms$term_id == syid , "term"]), collapse=", "));
      }
      maxSynoTerm[[so]] <- paste(unique(sy), collapse = " ; ", sep = " ; ");
    }
    else{
      noConceptsWithSyno[so] <- 0;
      minSynoT[so] <- 1;
      maxSynoT[so] <- 1;
      maxSynoTerm[so] <- "";
    }
  }
  df <- data.frame(noConcepts, noTerms, noConceptsWithSyno, RatioSynoT, minSynoT, maxSynoT, maxSynoTerm);
  return(df);
}
synoW <- function(match){
  uniqWo <- unique(as.character(match[,"oWords"]));
  uniq <- list();
  for(uniqW in uniqWo) { uniq[uniqW] <- c(); }
  for(row in row.names(match)){
    wo <- as.character(match[row,"oWords"]);
    if(wo != "" && match[row, "matchWords"] != ""){ uniq[[wo]] <- c(uniq[[wo]], as.character(match[row, "matchWords"])); }
  }
  return(uniq);
}
synDif <- function(List){
  dif <- c();
  for(wo in names(List)){ if(length(unique(List[[wo]])) != 1){ dif <- c(dif, wo); } }
  return(dif);
}
correctTerms <- function(terms, this, sources, lang){
  ids <- as.character(terms[terms$source == this & terms$lang == lang, "term_id"]);
  concept <- 0;
  sino <- 0;
  for(id in unique(ids)){
    tms <- as.character(terms[terms$term_id == id & terms$source == this & terms$lang == lang, "term"]);
    con <- 0;
    for(tm in tms) { 
      interm <- 0;
      for(so in sources){
        if(tm %in% as.character(terms[terms$term_id == id & terms$source == so & terms$lang == lang, "term"])){
          interm <- interm + 1;
        }
      }
      if(interm != 0) { sino <- sino + 1; con <- con +1;}   
    }
    if(con != 0) { concept <- concept + 1; }
  }
  return(matrix(c(c(length(unique(ids)), concept, concept/length(unique(ids))), c(length(ids), sino, sino/length(ids))), ncol=2));
}
correctWords <- function(terms, this, sources, lang){
  ids <- as.character(terms[terms$source == this & terms$lang == lang, "term_id"]);
  concept <- 0;
  sino <- 0;
  for(id in unique(ids)){
    tms <- as.character(terms[terms$term_id == id & terms$source == this & terms$lang == lang, "term"]);
    con <- 0;
    for(tm in tms) { 
      interm <- 0;
      for(so in sources){
        if(tm %in% as.character(terms[terms$term_id == id & terms$source == so & terms$lang == lang, "term"])){
          interm <- interm + 1;
        }
      }
      if(interm != 0) { sino <- sino + 1; con <- con +1;}   
    }
    if(con != 0) { concept <- concept + 1; }
  }
  return(matrix(c(c(length(unique(ids)), concept, concept/length(unique(ids))), c(length(ids), sino, sino/length(ids))), ncol=2));
}
ratioOffNewWords <- function(terms, offLang, newLang, sourc){
  ids <- as.character(terms[terms$source == sourc & terms$lang == newLang, "term_id"]);
  sameRatioWordsC <- 0;
  sameRatioWordsT <- 0;
  for(id in unique(ids)){
    tms <- as.character(terms[terms$term_id == id & terms$source == sourc & terms$lang == newLang, "term"]);
    interm <- 0;
    for(tm in tms){
      t <- 0;
      so <- as.character(terms[terms$term_id == id & terms$lang == offLang, "term"]);
      for(s in so) {
        if(length(unlist(strsplit(tm, " "))) == length(unlist(strsplit(s, " ")))){
          interm <- interm + 1;
        }
      }
      if(interm != 0) { t <- t + 1; sameRatioWordsT <- sameRatioWordsT +1; }
    }
    if(t != 0) { sameRatioWordsC <- sameRatioWordsC +1; }
  }
  return(matrix(c(c(length(unique(ids)), sameRatioWordsC, sameRatioWordsC/length(unique(ids))), c(length(ids), sameRatioWordsT, sameRatioWordsT/length(ids))), ncol=2));
}
findInText <- function(text, terms, sources, langs){
  find <- as.character(terms[terms$lang %in% langs & terms$source %in% sources, "term"]);
  for(term in find){
    text <- sub(paste(" ",term, " ", sep=""), paste("<", term, ">", sep=""),text);
  }
  return(text);
}
#global data frames: terms and relations
#cat("Connecting to database ...");

# x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv");
# allTerms <- read.csv(text = x);
# y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv");
# allRel <- read.csv(text = y);
# z <- getURL("https://raw.githubusercontent.com/loredanacirstea/thRoTrans/master/Data/500terms.csv");
# sample500 <- read.csv(text = z);
# tm <- allTerms;
# rel <- allRel;
# orig <- 10001;
# displayLg <-"la";
# finalLa <- cleardf(csvtodf(sample500[,2:15], "ro", 3));
# final <- referenceTrans(finalLa);
# finalLaEn <- csvtodf(sample500, off = TRUE);
# finalLaEn[,"term"] <- tolower(finalLaEn[,"term"]);
# finalLaEn[,"term"] <- sub("(", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub(")", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{ see page 73}", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{see page 3}", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{see also epidermis page 119}", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("41", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{ vide paginam  }", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{vide paginam }", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[,"term"] <- sub("{vide etiam epidermis paginam }", "", finalLaEn[,"term"], fixed=TRUE);
# finalLaEn[finalLaEn$term_id == 12965, "term"] <- "glandula parathyroidea"; #glandula parathyroideaglandula parathyroidea
# finalLaEn[finalLaEn$term_id == 12965 & finalLaEn$lang == "en", "term"] <- "parathyroid gland";
# finalLaEn2 <- separateTerms(finalLaEn, sources = FALSE);
# finalLaEn2 <- cleardf(finalLaEn2);
# finalLaWords <- sourceWordsList2(finalLaEn2,"la");
# finalEnWords <- sourceWordsList2(finalLaEn2,"en");
# wordsReference <- sourceWordsList2(final,"ro","reference");
# finalWords <- data.frame(t(rep(NA,length(names(final)))));
# names(finalWords)<-names(final);
# finalWords<-finalWords[-1,];
# finalWords <- listtodf(finalWords, finalLaWords, "la", termdf = finalLaEn2);
# finalWords <- listtodf(finalWords, finalEnWords, "en", termdf = finalLaEn2);
# finalWords <- listtodf(finalWords, wordsReference, "ro", sourc = "reference", termdf = final);
# termidsLaT <- as.character(finalLaEn2[finalLaEn2$lang == "la", "term_id"]);
# termidsEnT <- as.character(finalLaEn2[finalLaEn2$lang == "en", "term_id"]);
# termidsLaW <- as.character(finalWords[finalWords$lang == "la", "term_id"]);
# termidsEnW <- as.character(finalWords[finalWords$lang == "en", "term_id"]);

# gtLaRoTerms <- read.csv("../Data/gtLaRoTerms.csv", header = FALSE, sep = "\n");
# gtEnRoTerms <- read.csv("../Data/gtEnRoTerms.csv", header = FALSE, sep = "\n");
# gtLaRoWords <- read.csv("../Data/gtLaRoWords.csv", header = FALSE, sep = "\n");
# gtEnRoWords <- read.csv("../Data/gtEnRoWords.csv", header = FALSE, sep = "\n");
# gtLaRoTerms[,"V1"] <- sub(",", "", gtLaRoTerms[,"V1"], fixed=TRUE);
# gtLaRoTerms <- cleardf(gtLaRoTerms);
# gtLaRoTerms[,"V1"] <- tolower(gtLaRoTerms[,"V1"]);
# gtLaRoTerms[407,"V1"] <- "m.";
# gtEnRoTerms[,"V1"] <- sub(",", "", gtEnRoTerms[,"V1"], fixed=TRUE);
# gtEnRoTerms <- cleardf(gtEnRoTerms);
# gtEnRoTerms[,"V1"] <- tolower(gtEnRoTerms[,"V1"]);
# gtLaRoWords[,"V1"] <- sub(",", "", gtLaRoWords[,"V1"], fixed=TRUE);
# gtLaRoWords <- cleardf(gtLaRoWords);
# gtLaRoWords[,"V1"] <- tolower(gtLaRoWords[,"V1"]);
# gtEnRoWords[,"V1"] <- sub(",", "", gtEnRoWords[,"V1"], fixed=TRUE);
# gtEnRoWords <- cleardf(gtEnRoWords);
# gtEnRoWords[,"V1"] <- tolower(gtEnRoWords[,"V1"]);

# final <- appenddf(gtLaRoTerms, final, "ro", "gtLaRoTerms", termidsLaT); # a iesit in la
# final <- appenddf(gtEnRoTerms, final, "ro", "gtEnRoTerms", termidsEnT);
# finalWords <- appenddf(gtLaRoWords, finalWords, "ro", "gtLaRoWords", termidsLaW);
# finalWords <- appenddf(gtEnRoWords, finalWords, "ro", "gtEnRoWords", termidsEnW);

# LaRoRel <- wordMatchCsv(finalLaEn2, final, finalWords, "la", "ro", "reference", "gtLaRoWords");
# EnRoRel <- wordMatchCsv(finalLaEn2, final, finalWords, "en", "ro", "reference", "gtEnRoWords");
# LaRoRelOk <- read.csv("../Data/LaRoRelOk.csv");
# LaRoRelOk <- LaRoRelOk[,2:length(names(LaRoRelOk))];
# LaRoRelOk[,"matchWords"] <- tolower(LaRoRelOk[,"matchWords"]);
#final <- woToTermsBase(LaRoRelOk, final, "la", "ro", "gtLaRoWords");
#final <- woToTermsBase(EnRoRel, final, "la", "ro", "gtEnRoWords");
#final <- woToTermsRef(LaRoRelOk, final, "la", "ro", "referenceWordsLa");

# wordsjunqueira <- sourceWordsList2(final,"ro","junqueira");
# wordsjunqueira <- wordsjunqueira[which(wordsjunqueira != "")];
# wordsbuc1987 <- sourceWordsList2(final,"ro","buc1987");
# wordsbuc1987 <- wordsbuc1987[which(wordsbuc1987 != "")];
# wordstm2009 <- sourceWordsList2(final,"ro","tm2009");
# wordstm2009 <- wordstm2009[which(wordstm2009 != "")];
# wordstm2004 <- sourceWordsList2(final,"ro","tm2004");
# wordstm2004 <- wordstm2004[which(wordstm2004 != "")];
# wordscraiova2006 <- sourceWordsList2(final,"ro","craiova2006");
# wordscraiova2006 <- wordscraiova2006[which(wordscraiova2006 != "")];

# finalWords <- listtodf(finalWords, wordsjunqueira, "ro", sourc = "junqueira");
# finalWords <- listtodf(finalWords, wordsbuc1987, "ro", sourc = "buc1987");
# finalWords <- listtodf(finalWords, wordstm2009, "ro", sourc = "tm2009");
# finalWords <- listtodf(finalWords, wordstm2004, "ro", sourc = "tm2004");
# finalWords <- listtodf(finalWords, wordscraiova2006, "ro", sourc = "craiova2006");

#auto <- autoTranslWordList(final2, finalWords2, LaRoRelOk, "la", "ro", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"));
#LaRoRelAuto <- autoMatch(LaRoRelOk, auto, finalWords2);
#LaRoRelAutoOk <- read.csv("../Data/LaRoRelAutoOk.csv");
#finalWords3 <- appenddf(df=data.frame(V1 = LaRoRelAutoOk[, "matchWords"]) , finalWords2, "ro", "autoOk", as.character(LaRoRelAutoOk[, "term_id"]));
#final3 <- woToTermsBase(LaRoRelAutoOk, final2, "la", "ro", "autoOk", "matchWords");
#final3 <- autoReduce(final3, "autoOk", "autoRed", "ro");

#compareRatioWT2(finalLaEn2, final, finalWords, c("la","en"), "ro", c("reference","referenceWordsLa", "gtLaRoTerms", "gtEnRoTerms", "gtLaRoWords", "gtEnRoWords"), c("reference", "reference", "gtLaRoWords", "gtEnRoWords", "gtLaRoWords", "gtEnRoWords"));
#                  T    TQ     W      WQ     W/T
#la:              566 | 518 | 990  | 582 | 1.749117
#en:              581 | 535 | 1056 | 556 | 1.817556
#reference:       500 | 455 | 909  | 537 | 1.818000
#referenceWordsLa:500 | 455 | 909  | 537 | 1.818000
#gtLaRoTerms:     566 | 517 | 990  | 539 | 1.749117
#gtEnRoTerms:     581 | 534 | 1056 | 549 | 1.817556
#gtLaRoWords:     566 | 517 | 990  | 539 | 1.749117
#gtEnRoWords:     581 | 534 | 1056 | 549 | 1.817556

#compareSynoT(finalLaEn2, final, c("la","en"), "ro", c("reference", "referenceWordsLa", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"));

#                     noConcepts noTerms noConceptsWithSyno RatioSynoT minSynoT maxSynoT
# la                      500     566                 57   1.132000        1        4
# en                      500     581                 70   1.162000        1        3
# reference               500     500                  0   1.000000        1        1
# referenceWordsLa        500     500                  0   1.000000        1        1
# junqueira               496     583                 69   1.175403        1        4
# tm2009                  483     547                 55   1.132505        1        4
# tm2004                  193     230                 32   1.191710        1        3
# buc1987                  31      54                 12   1.741935        1        5
# craiova2006              68     125                 35   1.838235        1        5

#la         nexus, macula communicans, synapsis non vesicularis, synapsis electrica ; perikaryon, neurosoma, soma, corpus neuronis
#en         gap junction, nonvesicular synapse, electrical synapse ; neutrophilic granulocyte, neutrophil, segmented neutrophilic granulocyte ; plasmalemma, cell membrane, plasma membrane ; desmosome, macula adherens, spot desmosome ; secretory vacuole vesicle, secretory granule, secretory ; plasmocyte, plasma cell, plasmacyte ; stratum basale, basal cell layer, stratum germinativum ; microvillous border, brush border, striated border ; perisinusoidal cell, fat storing cell, hepatic stellate cell [hsc] ; portal area, portal canal, portal zone
#reference  
#referenceWordsLa
#junqueira  epiteliu simplu pavimentos, epiteliu unistratificat pavimentos, epiteliu simplu scuamos, epiteliu unistratificat scuamos
#tm2009     incluziune de glicogen, granulă de glicogen, depozit de glicogen, vacuolă de glicogen
#tm2004     celulă epitelială endocrină principală, celulă principală, celulă cromofobă ; celulă epitelială endocrină oxifilă, celulă oxifilă, celulă acidofilă ; celule nevroglice, celule gliale, nevroglii ; ganglion cerebro-spinal, ganglion senzitivo-senzorial, ganglion aferent ; neuron de asociație, interneuron, neuron intermediar
#buc1987    celulă stem, celulă sușă, precursor, progenitor, tulpină a altor tipuri de celule
#craiova2006 leucocit granulocitar neutrofil, granulocit neutrofil, granulocit neutrofil segmentat, neutrofil, polimorfonuclear neutrofil

# syn <- synoW(LaRoRelOk);
# length(names(syn));   #530 unique terms with transl != 0 (582 uniq la terms)
# noUniqueWords<-noElemList(syn, byElem = TRUE);   #vector of frequency
# max(noUniqueWords)     #20 = cellula
# syn[which(noUniqueWords >10)]
# woDif <- synDif(syn)   #40 words with diff synon
# syn[woDif]            #the diff words

# LaRoRelOk2 <- autoTranslRef(LaRoRelOk2) # sino reduced
# length(which(final[final$source == "gtLaRoTerms", "term"] %in% final[final$source == "reference", "term"])) # 55
# length(which(final[final$source == "gtLaRoTerms", "term"] %in% c(final[final$source == "reference", "term"], final[final$source == "junqueira", "term"], final[final$source == "tm2009", "term"], final[final$source == "tm2004", "term"], final[final$source == "buc1987", "term"], final[final$source == "craiova2006", "term"] ))) #65

#correctTerms(final, "reference", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro");
# 500c/500t
#correctTerms(final, "gtLaRoTerms", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro"); 
#64 concepts 64terms
#correctTerms(final, "gtEnRoTerms", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro"); 
# [,1]       [,2]
# [1,] 500.000 581.000000
# [2,] 128.000 129.000000
# [3,]   0.256   0.222031
#correctTerms(final, "gtLaRoWords", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro"); 
#        [,1]        [,2]
#[1,] 500.000 566.0000000
#[2,]  62.000  63.0000000
# [3,]   0.124   0.1113074
#correctTerms(final, "gtEnRoWords", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro"); 
#78 78
#correctTerms(final, "referenceWordsLa", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro"); 
# [,1]   [,2]
# [1,] 500.00 500.00
# [2,] 480.00 480.00
# [3,]   0.96   0.96


#correctWords(finalWords, "reference", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro");
# [,1] [,2]
# [1,]  500  909
# [2,]  500  909
# [3,]    1    1

#correctWords(finalWords, "gtLaRoWords", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro")
# [,1]        [,2]
# [1,] 500.000 990.0000000
# [2,] 204.000 258.0000000
# [3,]   0.408   0.2606061

#correctWords(finalWords, "gtEnRoWords", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro");
# [,1]         [,2]
# [1,] 500.00 1056.0000000
# [2,] 335.00  482.0000000
# [3,]   0.67    0.4564394

#correctWords(finalWords2, "referenceOk", c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro")
# [,1]        [,2]
# [1,] 500.000 990.0000000
# [2,] 474.000 771.0000000
# [3,]   0.948   0.7787879

#ratioOffNewWords(final2, "la","ro","reference")
# [,1]    [,2]
# [1,] 500.000 500.000
# [2,] 409.000 409.000
# [3,]   0.818   0.818

#ratioOffNewWords(final2, "la","ro","junqueira")
# [,1]        [,2]
# [1,] 500.00 587.0000000
# [2,] 415.00 485.0000000
# [3,]   0.83   0.8262351

# ratioOffNewWords(final2, "la","ro","gtLaRoTerms")
# [,1]        [,2]
# [1,] 500.000 566.0000000
# [2,] 434.000 495.0000000
# [3,]   0.868   0.8745583

#ratioOffNewWords(final2, "la","ro","gtLaRoWords")
# [,1]        [,2]
# [1,] 500.000 566.0000000
# [2,] 477.000 536.0000000
# [3,]   0.954   0.9469965



# write.csv(finalLaEn2, "../Data/finalLaEn.csv");
# writeLines(final[final$source == "reference", "term"], "../Data/referenceTerms.csv");
# writeLines(finalLaEn2[finalLaEn2$lang == "la", "term"], "../Data/laTerms.csv");
# writeLines(finalLaEn2[finalLaEn2$lang == "en", "term"], "../Data/enTerms.csv");
# writeLines(as.character(finalWords[finalWords$lang == "la", "term"]), "../Data/laWords.csv");
# writeLines(as.character(finalWords[finalWords$lang == "en", "term"]), "../Data/enWords.csv");
# write.csv(LaRoRel, "../Data/LaRoRel.csv");
# write.csv(EnRoRel, "../Data/EnRoRel.csv");
# write.csv(LaRoRelAuto, "../Data/LaRoRelAuto.csv", row.names=FALSE);

#write.csv(finalWords, "../Data/finalWords.csv");
#write.csv(final, "../Data/final.csv");

#length(LaRoRel[LaRoRel$matchWords != "", "matchWords"]) #722 from 990
#length(EnRoRel[EnRoRel$matchWords != "", "matchWords"]) #895 from 1056
#length(LaRoRelOk[LaRoRelOk$matchWords != "", "matchWords"]) #856 from 990
#length(which(LaRoRel[, "matchWords"] == LaRoRelOk[, "matchWords"])); #654
#length(which(LaRoRel[, "matchWords"] == "" & LaRoRelOk[, "matchWords"] == "")); #87 - "" -> 567 usable from 856
#length(as.character(finalWords[finalWords$source=="junqueira" & finalWords$lang == "ro","term"])); #1088






# unused
# termidsjunqueiraW <- rep(names(wordsjunqueira), noElemList(wordsjunqueira, byElem = TRUE)); # 1088 words - unused
# termidsbuc1987W <- rep(names(wordsbuc1987), noElemList(wordsbuc1987, byElem = TRUE)); # 123 words - unused
# termidstm2009W <- rep(names(wordstm2009), noElemList(wordstm2009, byElem = TRUE)); # 1019 words - unused
# termidstm2004W <- rep(names(wordstm2004), noElemList(wordstm2004, byElem = TRUE)); # 420 words - unused
# termidscraiova2006W <- rep(names(wordscraiova2006), noElemList(wordscraiova2006, byElem = TRUE)); # 293 words - unused
#finalLaEn3 <- finalLaEn2;
#finalLaEn3[,"source"] <- "fipat";
#final2 <- rbind(finalLaEn3, final);
#final2[,"id"]<- seq(1,length(row.names(final2))); #this is the shit now!
#finalWords2 <- appenddf(df=data.frame(V1 = LaRoRelOk[, "matchWords"]) , finalWords2, "ro", "referenceOk", as.character(LaRoRelOk[, "term_id"]));
#LaRoRelJ <- wordMatchCsv2(finalLaEn2, final, finalWords2, "la", "ro", "junqueira", c("gtLaRoWords", "referenceOk"));
# write.csv(LaRoRelJ, "../Data/LaRoRelJ.csv", row.names = TRUE);

#uniqueWo <- makeUniqueWords(final,"ro");
#g <- horizNoTerms(final);
#write.csv(final, "../Data/final.csv");
#write.csv(finalLaEn2, "finalLaEn.csv")
#w <- getURL("https://raw.githubusercontent.com/loredanacirstea/thRoTrans/ee4583b9c5c490e1732375acc4124ba248c763f2/Data/500terms.csv");
#sample <- read.csv(text = w);
#sample00 <- matchid(sample500);
#demog <- graphdemo();
#newlang <- csvtodf(sample00, "ro", 3);
#dd <- newlang[1:25,];
#finalLa <- separateTerms(newlang);
#sample002 <- sample002[,2:15];
#finalLa2 <- csvtodf(sample002, "ro", 3);
#finalLa2clear <- cleardf(finalLa2);
#fin <- referenceTrans(finalLa2clear);
#g <- graphcompare(finalLa2clear);
#g <- graphcompare(finalLa2);
#comp(finalLa2, finalLa);
#newd <- getURL("https://raw.githubusercontent.com/loredanacirstea/thRoTrans/master/Data/500terms.csv");
#newdf <- read.csv(text = newd);
#cat("Done.");
#ontobrowse();
#wo<-wordFrame("la","en");
#wo<-transWords(wo, "la", "ro");
#wo<-transWord(wo, 47032, "la", "ro");
#finalLaEn <- tm[tm$term_id %in% sample500[,"id"] & tm$lang %in% c("la","en"), ]; !!nu e bun!!!
#final <- woToTermsBase(LaRoRelOk, final, "la", "ro", "referenceWordsLa"); - nu
# final[final$source == "gtLaRoTerms","lang"] <- rep("ro", length(final[final$source == "gtLaRoTerms","lang"])); -nush de ce a fost nevoie
#length(sample500[sample500$junqueira != "", "junqueira"]);
#gsub("(^[[:space:]]+|[[:space:]]+$)", "", unlist(strsplit(as.character(sample002[,"junqueira"]),
# c(";", " ;", " ; ", "; "), fixed = TRUE)))