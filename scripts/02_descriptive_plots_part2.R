

## Immigration and Social Mobility

# #par(mar = c(5, 6, 4, 2) + 0.1)
# plot(jitter(data_ineq$mobility3),
#      jitter(as.numeric(data_ineq$stmigrant)),
#      pch = 19,
#      col = viridis(1, 0.03),
#      main = "Immigration as cause for Inequality",
#      font.main = 1,
#      xlab = "Expected Mobility Scale",
#      ylab = "",
#      xaxt = "n",
#      yaxt = "n",
#      bty = "n")
# axis(1,
#      at = -1:1,
#      labels = c("Expect Downward Mobility", "No mobility",
#                 "Expect Upward Mobility"),
#      cex.axis = 0.7)
# axis(2,
#      at = 1:5,
#      labels = c("Disapprove,\nstrongly",
#                 "Disapprove,\nnot so strongly",
#                 "Nor agree \nnor disagree",
#                 "Approve,\nnot so strongly",
#                 "Approve,\nstrongly"),
#      las = 1,
#      cex.axis = 0.7)

## Immigration and Political Ideology ####

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
     at = 1:11,
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
