#https://drupal.org/comment/7710247#comment-7710247

#key = AIzaSyCMYvuc5Qi42roHIOPpAxRjR_bSSgYOHpU
#GET https://www.googleapis.com/language/translate/v2?key = AIzaSyCMYvuc5Qi42roHIOPpAxRjR_bSSgYOHpU&target=ro&q=blue%20balls

#AIzaSyC52zBpNSi-udSEDQhZK_WqU5TMOr84XN0
#AIzaSyDlE05FCFugKklVOfGytGZjuNpsJ6e0Ivo
AIzaSyC5nT8bwUjdNXJxRbiloQhy6qhybDsdPNo

#install.packages("RCurl");
#library(RCurl);
#curl = getCurlHandle();
#yey = getURL("http://en.wikipedia.org/wiki/Gaia_(spacecraft)");
#gt = getURL("https://www.googleapis.com/language/translate/v2?key=AIzaSyCMYvuc5Qi42roHIOPpAxRjR_bSSgYOHpU&target=ro&q=blue%20balls");


install.packages("translate");
library(translate);
set.key("AIzaSyC5nT8bwUjdNXJxRbiloQhy6qhybDsdPNo")
translate("Hello, world!", "en", "de")
