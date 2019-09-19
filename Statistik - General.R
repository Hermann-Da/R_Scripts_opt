#############################################
# Vorbehandlung auswählen
#############################################

# Welche Vorbehandlungen gibt es
Type <- c("Spectra","Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n" )
# Welche werden ausgewählt
Stats <- 5

# Wenn die abgeleiteten Spektren verwendet werden
# Sample <- Sample_d

#########################
# PCA 
#########################
# Scale = TRUE, damit wird die Korrelationsmatrix verwendet
# Scale = FALSE, damit wird die Kovarianzmatrix verwendet


PCA <- prcomp(Data[[Type[Stats]]][!is.na(Data$Groups),Sample],
                  center = TRUE,rank.=5, scale. = TRUE )

# Damit "NAs" entfernen, um die Farben richtig darzustellen
PCA$Groups <- Data$Groups[!is.na(Data$Groups)]

summary(PCA)

######################
# Erklärte Varianz pro Principal Component
#####################

# Varianz ausgeben
Data$Variance <- PCA$sdev^2/sum(PCA$sdev^2)


#My.export_start(paste("Variance",Type[Stats], sep = " "))

plot(x= 1:length(Data$Variance), y = cumsum(Data$Variance[1:length(Data$Variance)]*100),
     type = "b", xlab = "Number of Principal Components",
     ylab = "Explained Variance [%]",
     col = "dimgrey", pch = 21, bg = "darkgrey",
     font = 2, font.lab = 2,lab = c(10,20,20))
grid(lwd = 0.8)
#My.export_end()


########################
# Loadings ausgeben
#######################

# My.export_start(paste("Loadings",Type[Stats], sep = " "))

plot.load()

#My.export_end()

#####################
# Scorewerte plotten, überblicksmäßig
#####################


#My.export_start(paste("Scores_ges",Type[Stats], sep = " "))

pairs(PCA$x[,1:3],col=Data$Groups, pch = 19)
legend("bottomright", legend=levels(Data$Groups), pch=19, col=unique(Data$Groups), inset = 0.05,
       horiz = FALSE, bty = "n")

#My.export_end()

#####################
# Plotten der ScoreWerte einzelner PCA's
####################

X_Ax = 1 #PC auswählen (X-Achse)
Y_Ax = 2 #PC auswählen (Y-Achse)

# My.export_start("paste("Scores_1_2",Type[Stats], sep = " ")")

plot(x = PCA$x[,X_Ax], y = PCA$x[,Y_Ax],
     xlab = paste("Scores of","PC" ,X_Ax, "[",round(Data$Variance[X_Ax]*100,1),"%]", sep = " "),
     ylab = paste("Scores of","PC" ,Y_Ax, "[",round(Data$Variance[Y_Ax]*100,1),"%]", sep = " "),
     col = Data$Groups[!is.na(Data$Groups)], 
     pch = 19, font.lab = 2)
grid(lwd = 0.8)
abline(h = 0, v = 0)
legend("bottomleft", legend=levels(Data$Groups[!is.na(Data$Groups)]), 
       pch=16, col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.01, bty = "n")
# My.export_end()

###################################
# Ausreißer identifizieren mit identify
###################################
# Entkommentieren und ausführen, gibt Indexzes der gewählten Punkte aus

#Ausreißer = identify( x = PCA_n$x[,X_Ax], y = PCA_n$x[,Y_Ax])



###################################
# Hierarchical clustering Analysis
###################################

#My.export_start("paste("HCA",Type[Stats], sep = " ")")


HCA(Data[[Type[Stats]]][!is.na(Data$Groups),Sample],Data$Groups[!is.na(Data$Groups)])
legend("topright", legend=levels(Data$Groups[!is.na(Data$Groups)]), 
       pch=16, col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.01, bty = "n")

#My.export_end()

###################################
# PCA und Hierarchical clustering Analysis
###################################

#My.export_start(paste("PCA_HCA",Type[Stats], sep = " "))

HCA(Data = PCA$x[,1:2],Groups = Data$Groups[!is.na(Data$Groups)])
legend("topright", legend=levels(Data$Groups[!is.na(Data$Groups)]), 
       pch=16, col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.01, bty = "n")
#My.export_end()
