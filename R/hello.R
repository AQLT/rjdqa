rm(list = ls())
library(RJDemetra)
library(plotrix)
library(zoo)
load("./Data/ABS.rda")
# s <- ts(window(ABS$X0.2.09.10.M,end = 1990),frequency = 4,start= c(1980,2))
# user_defined_variables()
# mysa <- x13_def(s,
#                 spec=c("RSA5c"),
#                 userdefined = c("decomposition.c17"))
# myspec1<-x13_spec_def(spec=c("RSA5c"),usrdef.outliersEnabled = TRUE,
#                       usrdef.outliersType = c("AO","AO","AO","AO"),
#                       usrdef.outliersDate=c("2000-01-01","2000-04-01","2000-07-01","2000-10-01"),
#                       tradingdays.option = "WorkingDays",tradingdays.test = "None")
# mysa <- x13(s,myspec1,
#                 userdefined = list(decomposition = c("decomposition.c17")))
# mysa$regarima
userdefined <- c("decomposition.c17","preprocessing.model.tde_f",
                 "preprocessing.model.mhe_f")
myspec1<-x13_spec_def(spec=c("RSA5c"),transform.function = "None")
myspec1<-x13_spec_def(spec=c("RSA5c"),transform.function = "Log")
mysa <- x13(window(ABS$X0.2.09.10.M, end = c(2016,3)), myspec1,
                userdefined = userdefined)
seasonal_dashboard(mysa)
x <- mysa
liste_f <- list.files(path = "R/",full.names = TRUE)[-1]
for (f in liste_f){
    source(f,encoding = "UTF-8")
}
sa_d <- sa_dashboard(x)
plot(sa_d)


?barp
?barp
col_td <- grep("^td...$", colnames(demetra_m),value = FALSE)
col_ly <- grep("^lp$", colnames(demetra_m),value = FALSE)

if(all(is.na(demetra_m[,col_ly])))
    col_ly <- NULL
if(all(is.na(demetra_m[,col_td[1]])))
    col_td <- NULL

cols <- c(col_td, col_ly)



plot.new()
textbox(c(0,0.2), 1, c("many words","more words","why not?",
                       "keep going",rep("and going",10)))
textbox(c(0.3,0.5), 1, c("keep going",rep("and going",10)), cex=0.45,
        col="blue", border="red", fill="#00FFEE80", density=25, angle=60)
textbox(c(0.6,0.8), 1, c("keep going",rep("and going",10)), justify='c', cex=0.6,
        leading=1, font=4, border="gold", lty=2, lwd=4, margin=0.025)
textbox(c(0.6,0.8), 0.5, c("keep going",rep("and going",10)), justify='r', cex=0.7,
        col="purple", font=3, border="green", margin=-0.025)
lines(c(0,1), c(1,1), col="red", lty=2)
lines(c(0,1), c(0.5,0.5), col="red", lty=2)