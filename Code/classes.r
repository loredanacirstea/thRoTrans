#link with the database connection file
source("db_connect.r");

#create dataframe with all/selected terminologies (now:all)!
allQuery = dbSendQuery(mydb, "select * from `term`");
tm = fetch(allQuery, n = -1);
#create dataframe with all/selected relations (now:all)!
relQuery = dbSendQuery(mydb, "select * from `term_relation`");
rel = fetch(relQuery, n = -1);

#classes words, terms
#class words with init method, translate, usedIn(terms in which it is used)
setClass("words", representation = (Data = "data.frame", origin = "numeric"));
setMethod("translate", "words", function(word, lang) {
    })
setMethod("usedIn", "words", function(word, lang) {
    })

#class terms with init method, translate, ancestry(start point given by the terminology in use)
setClass("terms", representation = (Data = "data.frame", origin = "numeric"));
#should inherit from words
setMethod("translate", "terms", function(term, lang) {
    })
setMethod("ancestry", "terms", function(term, lang) {
    })