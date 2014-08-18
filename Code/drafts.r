#dataframe with all the relations term-term, word-term
#lower case for all terms except t language
#tm[,"term"] <- c(tolower(tm[tm$lang != "t","term"]), tm[tm$lang == "t","term"]);

#length(which(final3[final3$source == "reference","term"] %in% final3[final3$source == "junqueira","term"])) # same as fromSource

# compareRatioWT2(finalLaEn2, final3, finalWords3, c("la","en"), "ro", c("autoOk", "autoRed", "reference","referenceWordsLa", "gtLaRoTerms", "gtEnRoTerms", "gtLaRoWords", "gtEnRoWords", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), c("autoOk", "autoRed", "reference", "reference", "gtLaRoWords", "gtEnRoWords", "gtLaRoWords", "gtEnRoWords", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"));
#                   noTerms noTermsQ noWords noWordsQ  ratioWT
# la                   566      518     990      582 1.749117
# en                   581      535    1056      556 1.817556
# autoOk               566      513     984      570 1.738516
# autoRed              500      461     857      521 1.714000
# reference            500      455     909      537 1.818000
# referenceWordsLa     500      455     909      537 1.818000
# gtLaRoTerms          566      518     990      539 1.749117
# gtEnRoTerms          581      530    1056      549 1.817556
# gtLaRoWords          566      517     990      539 1.749117
# gtEnRoWords          581      534    1056      549 1.817556
# junqueira            583      533    1088      597 1.866209
# tm2009               547      495    1019      564 1.862888
# tm2004               230      200     420      255 1.826087
# buc1987               54       39     123       64 2.277778
# craiova2006          125      104     293      135 2.344000

# compareSynoT(finalLaEn2, final3, c("la","en"), "ro", c("autoOk", "autoRed", "reference","referenceWordsLa", "gtLaRoTerms", "gtEnRoTerms", "gtLaRoWords", "gtEnRoWords", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"));
#                     noConcepts noTerms noConceptsWithSyno RatioSynoT minSynoT maxSynoT
# la                      500     566                 57   1.132000        1        4
# en                      500     581                 70   1.162000        1        3
# autoOk                  500     566                 57   1.132000        1        4
# autoRed                 500     500                  0   1.000000        1        1
# reference               500     500                  0   1.000000        1        1
# referenceWordsLa        500     500                  0   1.000000        1        1
# gtLaRoTerms             500     566                 57   1.132000        1        4
# gtEnRoTerms             500     581                 70   1.162000        1        3
# gtLaRoWords             500     566                 57   1.132000        1        4
# gtEnRoWords             500     581                 70   1.162000        1        3
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
#autoOk     nexus, macula comunicantă, sinapsă  , sinapsă  ; pericarion, neurozom, soma, corp neuronal
#autoRed    -
#gtLaRoTerms legătură, partaja un loc, sinapsele nu veziculoase, sinapse electrice ; perikaryon, neurosoma, soma, neuroni corp
#gtEnRoTerms joncțiune gap, synapse nonvesicular, sinapsă electrică ; granulocite neutrofile, neutrofile, granulocite neutrofile segmentate ; plasmalema, membranei celulare, membranei plasmatice ; desmosome, adherens macula, la fața locului desmosome ; secretorie veziculă vacuole, granule secretorii, secretorie ; plasmocyte, celulelor plasmatice, plasmacyte ; stratul bazal, strat de celule bazale, strat germinativum ; frontieră microvillous, perie de frontieră, frontieră striat ; celulă perisinusoidală, celulă de stocare de grasime, celulelor hepatice stelat [hsc] ; zona de portal, canal portal, zona de portal
#gtLaRoWords legătură, punct comunicarea, sinapselor nu veziculoase, sinapselor electric ; perikaryon, neurosoma, soma, corp neuron
#gtEnRoWords plasmalema, celulă membrană, plasma membrană ; microvillous frontieră, perie frontieră, striat frontieră ; desmosome, macula adherens, punct desmosome ; decalaj joncțiune, nonvesicular synapse, electric synapse ; secretorie vacuole veziculă, secretorie granulă, secretorie ; plasmocyte, plasma celulă, plasmacyte ; neutrofilica granulocite, neutrofile, segmentat neutrofilica granulocite ; perisinusoidală celulă, grăsime stocarea celulă, hepatic stelat celulă hsc ; portal domeniu, portal canal, portal zonă ; strat bazal, bazale celulă strat, strat germinativum



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

# correctTerms(final3, "autoOk", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro")
# [,1]        [,2]
# [1,] 500.00 566.0000000
# [2,] 385.00 405.0000000
# [3,]   0.77   0.7155477

# correctTerms(final3, "autoRed", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro")
# [,1]    [,2]
# [1,] 500.000 500.000
# [2,] 371.000 371.000
# [3,]   0.742   0.742

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

# correctWords(finalWords3, "autoOk", c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro")
# [,1]        [,2]
# [1,] 500.000 990.0000000
# [2,] 457.000 801.0000000
# [3,]   0.914   0.8090909

# ratioOffNewWords(final3, "la","ro","autoOk")
# [,1] [,2]
# [1,]  500  566
# [2,]  500  566
# [3,]    1    1

# ratioOffNewWords(final3, "la","ro","autoRed")
# [,1] [,2]
# [1,]  500  500
# [2,]  500  500
# [3,]    1    1

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

# ratioOffNewWords(final3, "la","ro","autoOk")
# [,1] [,2]
# [1,]  500  566
# [2,]  500  566
# [3,]    1    1

# ratioOffNewWords(final3, "la","ro","autoRed")
# [,1] [,2]
# [1,]  500  500
# [2,]  500  500
# [3,]    1    1


#final3[row.names(final3[final3$source == "autoOk", ])[which(nchar(as.character(final3[final3$source == "autoOk", "term"])) == 6)], "term"];
#"celulă" "nucleu" "flagel" "mitoză" "papilă"
#final3[row.names(final3[final3$source == "autoOk", ])[which(nchar(as.character(final3[final3$source == "autoOk", "term"])) == 15)], "term"];
#"glande salivare" "glandă parotidă" "coroană dentară" "alveolă dentară" "folicul limfoid"
#final3[row.names(final3[final3$source == "autoOk", ])[which(nchar(as.character(final3[final3$source == "autoOk", "term"])) == 20)], "term"];
#"celulă multinucleată" "țesut conjunctiv lax" "granulocit neutrofil" "neurofibră senzitivă" "proces odontoblastic"

#final3[row.names(final3[final3$source == "autoRed", ])[which(nchar(as.character(final3[final3$source == "autoRed", "term"])) == 6)], "term"];
#"celulă" "esofag" "neuron" "nucleu" "plasmă"
#final3[row.names(final3[final3$source == "autoRed", ])[which(nchar(as.character(final3[final3$source == "autoRed", "term"])) == 15)], "term"];
#"arahnoidă mater" "astrocit fibros" "neuron purkinje" "cortex cerebral" "zonula ocludens"
#final3[row.names(final3[final3$source == "autoRed", ])[which(nchar(as.character(final3[final3$source == "autoRed", "term"])) == 20)], "term"];
#"țesut conjunctiv lax" "țesut muscular neted" "ligament periodontal" "neurofibră senzitivă" "celulă multinucleată"

#final3[row.names(final3[final3$source == "reference", ])[which(nchar(as.character(final3[final3$source == "reference", "term"])) == 6)], "term"];
#"celulă" "esofag" "neuron" "nucleu" "plasmă"
#final3[row.names(final3[final3$source == "reference", ])[which(nchar(as.character(final3[final3$source == "reference", "term"])) == 15)], "term"];
#"astrocit fibros" "cavitate bucală" "celulă satelită"  "cortex cerebral" "măduva spinării"
#final3[row.names(final3[final3$source == "reference", ])[which(nchar(as.character(final3[final3$source == "reference", "term"])) == 20)], "term"];
#"țesut conjunctiv lax" "țesut muscular neted" "ligament periodontal" "celulă mioepitelială" "celulă centroacinară"

#final3[row.names(final3[final3$source == "referenceWordsLa", ])[which(nchar(as.character(final3[final3$source == "referenceWordsLa", "term"])) == 6)], "term"];
#"celulă" "nucleu" "flagel" "mitoză" "stromă"
#final3[row.names(final3[final3$source == "referenceWordsLa", ])[which(nchar(as.character(final3[final3$source == "referenceWordsLa", "term"])) == 15)], "term"];
#"margine striată" "nucleu picnotic" "heterocromatină" "membrană bazală" "țesut epitelial"
#final3[row.names(final3[final3$source == "referenceWordsLa", ])[which(nchar(as.character(final3[final3$source == "referenceWordsLa", "term"])) == 20)], "term"];
#"celulă multinucleată" "celulă mioepitelială" "proces odontoblastic" "ligament periodontal" "celulă centroacinară"

#final3[row.names(final3[final3$source == "gtLaRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtLaRoTerms", "term"])) == 6)], "term"];
#"celulă" "creier" "esofag" "neuron" "nucleu"
#final3[row.names(final3[final3$source == "gtLaRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtLaRoTerms", "term"])) == 15)], "term"];
#"canal alimentar" "țesut epitelial" "glanda exocrina" "tropocollagenum" "myofibroblastus"
#final3[row.names(final3[final3$source == "gtLaRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtLaRoTerms", "term"])) == 20)], "term"];
#"precursori de celule" "epiteliul respirator" "strat de accidentale" "cavitatea unui dinte" "capsule transparente"

#final3[row.names(final3[final3$source == "gtEnRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtEnRoTerms", "term"])) == 6)], "term"];
#"celulă" "esofag" "neuron" "nucleu" "plasma"
#final3[row.names(final3[final3$source == "gtEnRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtEnRoTerms", "term"])) == 15)], "term"];
#"mater arahnoida" "cavitatea orală" "celule purkinje" "citoscheletului" "adherens macula"
#final3[row.names(final3[final3$source == "gtEnRoTerms", ])[which(nchar(as.character(final3[final3$source == "gtEnRoTerms", "term"])) == 20)], "term"];
#"cortexul cerebeloasa" "synapse axodendritic" "țesut muscular neted" "veziculă pinocytotic" "microbutule centrală"

#final3[row.names(final3[final3$source == "gtLaRoWords", ])[which(nchar(as.character(final3[final3$source == "gtLaRoWords", "term"])) == 6)], "term"];
#"celulă" "nucleu" "organe" "flagel" "nucleu"
#final3[row.names(final3[final3$source == "gtLaRoWords", ])[which(nchar(as.character(final3[final3$source == "gtLaRoWords", "term"])) == 15)], "term"];
#"celulă gigantic" "filament actina" "neurofilamentum" "citoscheletului" "microfilamentum"
#final3[row.names(final3[final3$source == "gtLaRoWords", ])[which(nchar(as.character(final3[final3$source == "gtLaRoWords", "term"])) == 20)], "term"];
#"matrice cytoplasmica" "celulă multinucleata" "microtubulus central" "matrice extracelular" "neurofibra senzorial"

#final3[row.names(final3[final3$source == "gtEnRoWords", ])[which(nchar(as.character(final3[final3$source == "gtEnRoWords", "term"])) == 6)], "term"];
#"celulă" "nucleu" "cilium" "dynein" "flagel"
#final3[row.names(final3[final3$source == "gtEnRoWords", ])[which(nchar(as.character(final3[final3$source == "gtEnRoWords", "term"])) == 15)], "term"];
#"celulă membrană" "plasma membrană" "perie frontieră" "macula adherens" "punct desmosome"
#final3[row.names(final3[final3$source == "gtEnRoWords", ])[which(nchar(as.character(final3[final3$source == "gtEnRoWords", "term"])) == 20)], "term"];
#"multinucleate celulă" "nonvesicular synapse" "endoplasmatic stomac" "pinocytotic veziculă" "nonvesicular synapse"

#file <- "../Test Files/txt.txt";
#txt <- readChar(file, file.info(file)$size);
#txt2 <- findInText2(txt, final3, "autoRed", "ro");

#system.time( replicate(100, findInText2(txt, final3, "autoRed", "ro") ) )
# user  system elapsed 
# 9.126   0.290   9.461 

#system.time( replicate(100, findInText2(txt, final3, "autoOk", "ro") ) )
# user  system elapsed 
# 10.248   0.314  10.835 

#system.time( replicate(100, findInText2(txt, final3, "reference", "ro") ) )
# user  system elapsed 
# 9.351   0.281   9.959 

#system.time( replicate(100, findInText2(txt, final3, "referenceWordsLa", "ro") ) )
# user  system elapsed 
# 9.184   0.263   9.677

#system.time( replicate(100, findInText2(txt, final3, "gtLaRoTerms", "ro") ) )
# user  system elapsed 
# 10.062   0.304  10.521 

#system.time( replicate(100, findInText2(txt, final3, "gtEnRoTerms", "ro") ) )
# user  system elapsed 
# 10.374   0.294  11.164 

#system.time( replicate(100, findInText2(txt, final3, "gtLaRoWords", "ro") ) )
# user  system elapsed 
# 10.148   0.260  10.611 

#system.time( replicate(100, findInText2(txt, final3, "gtEnRoWords", "ro") ) )
# user  system elapsed 
# 10.331   0.308  10.844 


#txt2 <- findInText(txt, final3, c("junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro");
#system.time( replicate(10, myfunction(with,arguments) ) )
# user  system elapsed 
# 30.676   0.374  31.160 


#length(LaRoRel[LaRoRel$matchWords != "", "matchWords"]) #722 from 990
#length(EnRoRel[EnRoRel$matchWords != "", "matchWords"]) #895 from 1056
#length(LaRoRelOk[LaRoRelOk$matchWords != "", "matchWords"]) #856 from 990
#length(which(LaRoRel[, "matchWords"] == LaRoRelOk[, "matchWords"] & LaRoRel[, "matchWords"] != "")) #567
#length(as.character(finalWords[finalWords$source=="junqueira" & finalWords$lang == "ro","term"])); #1088

#length(LaRoRelAuto[LaRoRelAuto$matchWords != "", "matchWords"]) #961
#length(LaRoRelAutoOk[LaRoRelAutoOk$matchWords != "", "matchWords"]) #984
#length(which(LaRoRelAuto[, "matchWords"] == LaRoRelAutoOk[, "matchWords"] & LaRoRelAuto[, "matchWords"] != "")) #772


# unused
# termidsjunqueiraW <- rep(names(wordsjunqueira), noElemList(wordsjunqueira, byElem = TRUE)); # 1088 words - unused
# termidsbuc1987W <- rep(names(wordsbuc1987), noElemList(wordsbuc1987, byElem = TRUE)); # 123 words - unused
# termidstm2009W <- rep(names(wordstm2009), noElemList(wordstm2009, byElem = TRUE)); # 1019 words - unused
# termidstm2004W <- rep(names(wordstm2004), noElemList(wordstm2004, byElem = TRUE)); # 420 words - unused
# termidscraiova2006W <- rep(names(wordscraiova2006), noElemList(wordscraiova2006, byElem = TRUE)); # 293 words - unused

#LaRoRelJ <- wordMatchCsv2(finalLaEn2, final, finalWords2, "la", "ro", "junqueira", c("gtLaRoWords", "referenceOk"));
# write.csv(LaRoRelJ, "../Data/LaRoRelJ.csv", row.names = TRUE);

#text <- "O celulă are multe organite și este de mai multe feluri. Avem o celulă scuamoasă cu margine striată și platou microvilos cu microvil și stereocil. Epiteliu pseudostratificat există."
#text <- findInText(text,final3, c("autoRed"), "ro");
#txt <- readLines("../Test Files/textGT.txt");
#txt2 <- findInText(txt, final3, c("reference", "junqueira", "tm2009", "tm2004", "buc1987", "craiova2006"), "ro");
#writeLines(txt2, "../Test Files/textGT2.txt")
#system.time( replicate(10000, myfunction(with,arguments) ) )

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