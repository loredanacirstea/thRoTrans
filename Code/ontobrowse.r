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
#makes a word vector from initialized terminology - +origin? (! [],no )
makeWords <- function(terms = tm,lang) {
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
csvtodf <- function(df, lang, langNo) {
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
separateTerms <- function(df) {
  for(row in row.names(df)) {
    if(as.character(df[row, "term"]) != "") {
      nosep <- length(grep(";", as.character(df[row, "term"]), fixed = TRUE));
      if(nosep > 0) {
        synon <- unlist(strsplit(as.character(df[row,"term"]), c(";", " ;", " ; ", "; "), fixed=TRUE));
        pgs <- unlist(strsplit(as.character(df[row,"pages"]), c(";", " ;", " ; ", "; "), fixed=TRUE));
        if(length(pgs) == 1) {
          pgs <- rep(pgs, nosep+1);
        }
        #str(synon);
        #str(pgs);
        for(i in 2:length(synon)) {
          end <- length(row.names(df));
          df[end + 1, ] <- df[row,];
          df[end + 1, "term"] <- synon[i];
          df[end + 1, "pages"] <- pgs[i];
        }
        df[row, "term"] <- synon[1];
        df[row, "pages"] <- pgs[1];
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
compare <- function(df, sources){
  #per <- c();
  #for(row in row.names(df)) {
    #per[""]
  #}
  #g <- barplot(c(100, 90, 80));
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
graphcompare <- function(df){
  add <- c();
  sources <- factor(df[, "source"]);
  for(so in levels(sources)) { add[so] <- 0;}
  add["junqueira"] <- length(df[df$source == "junqueira", "term"])
  for(row in row.names(df)) {
    if(length(as.character(df[row, "term"])) != 0 && length(as.character(df[df$term_id == df[row, "term_id"] & df$sources == "junqueira", "term"])) != 0) {
      if(as.character(df[row, "term"]) == as.character(df[df$term_id == df[row, "term_id"] & df$sources == "junqueira", "term"])) {
        add[df[row,"source"]] <- add[df[row,"source"]] +1;
      }
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
#global data frames: terms and relations
#cat("Connecting to database ...");
x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv");
allTerms <- read.csv(text = x);
y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv");
allRel <- read.csv(text = y);
z <- getURL("https://raw.githubusercontent.com/loredanacirstea/thRoTrans/master/Data/500terms.csv");
sample500 <- read.csv(text = z);
finalLa2 <- cleardf(csvtodf(sample002, "ro", 3));
final <- referenceTrans(finalLa2);



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
#tm <- allTerms;
#rel <- allRel;
#orig <- 10001;
#displayLg <-"la";
#cat("Done.");
#ontobrowse();
#wo<-wordFrame("la","en");
#wo<-transWords(wo, "la", "ro");
#wo<-transWord(wo, 47032, "la", "ro");

#fin[fin$source == "reference" & fin$term_id == 11222,"term"] %in% gsub("(^[[:space:]]+|[[:space:]]+$)", "", unlist(strsplit(as.character(sample002[,"junqueira"]), c(";", " ;", " ; ", "; "), fixed = TRUE)))
