#install.packages("RCurl");
library(RCurl);
x <- getURL("https://raw.githubusercontent.com/loredanacirstea/thRoTrans/master/Data/500terms.csv");
tm <- read.csv(text = x);
colNames<-names(tm);