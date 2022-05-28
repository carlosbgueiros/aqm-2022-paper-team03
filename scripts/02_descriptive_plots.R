

par(mfrow = c(2,1))

barplot(table(data_ineq$stmigrant),
        border = F,
        main = "Immigration as cause for Inequality",
        names.arg = c("Disapprove,\nstrongly", "", "", "", "Approve,\nstrongly"),
        cex.names = 0.8,
        cex.main = 0.8,
        las = 1)

barplot(table(data_ineq$lrscale),
        main = "Left-Right Scale (Self Assesment)",
        names.arg = c("left", rep("", 9), "right"),
        cex.names = 0.8,
        border = F,
        cex.main = 0.8,
        las = 1)


par(mar = c(5, 6, 4, 2) + 0.1)
plot(jitter(data_ineq$lrscale),
     jitter(as.numeric(data_ineq$stmigrant)),
     pch = 19,
     col = viridis(1, 0.03),
     main = "Immigration as cause for Inequality",
     font.main = 1,
     xlab = "Left-Right Scale",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1,
     at = 1:9,
     labels = c("left", rep("", 9),
                "right"),
     cex.axis = 0.7)
axis(2,
     at = 1:5,
     labels = c("Disapprove,\nstrongly",
                "Disapprove,\nnot so strongly",
                "Nor agree \nnor disagree",
                "Approve,\nnot so strongly",
                "Approve,\nstrongly"),
     las = 1,
     cex.axis = 0.7)


df <- data.frame(data_ineq$downward_mob,as.numeric(data_ineq$stmigrant)) 
colnames(df) <- c("downward_mob", "stmigrant")
## Use densCols() output to get density at each point
#x <- densCols(data_ineq$downward_mob,as.numeric(data_ineq$stmigrant), colramp=colorRampPalette(c("black", "white")))

#df$dens <- col2rgb(x)[1,] + 1L

## Map densities to colors
cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)

#df$col <- cols[df$dens]

df <- df |> 
  group_by(downward_mob, stmigrant) |> 
  mutate(x = densCols(downward_mob,stmigrant, colramp=colorRampPalette(c("black", "white"))),
         dens = col2rgb(x)[1,] + 1)

#par(mar = c(5, 6, 4, 2) + 0.1)
plot(jitter(data_ineq$downward_mob),
     jitter(as.numeric(data_ineq$stmigrant)),
    # stmigrant ~ downward_mob,
     pch = 19,
    # data = df[order(df$dens),],
     col = cols,    #viridis(1, 0.05),
     main = "Immigration as cause for Inequality",
     font.main = 1,
     xlab = "Expect Downward Mobility",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")
axis(1,
     at = 0:1,
     labels = c("Yes", "No"),
     cex.axis = 0.7)
axis(2,
     at = 1:5,
     labels = c("Disapprove,\nstrongly",
                "Disapprove,\nnot so strongly",
                "Nor agree \nnor disagree",
                "Approve,\nnot so strongly",
                "Approve,\nstrongly"),
     las = 1,
     cex.axis = 0.7)