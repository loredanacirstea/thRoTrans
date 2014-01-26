install.packages("RMySQL");
library(RMySQL);
#connect mysql
#mydb = dbConnect(MySQL(), user='root', password='', dbname='thesis', host='127.0.0.1');
mydb = dbConnect(MySQL(), user='root', password='', dbname='chr', host='127.0.0.1');