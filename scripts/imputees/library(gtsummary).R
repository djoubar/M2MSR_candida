library(gtsummary)
imp_tot <- complete(imp, action = "long")

tbl_uvregression(resultat_candida_def ~ , data = imp_tot)
