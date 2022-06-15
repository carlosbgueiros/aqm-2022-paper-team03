
#par(mfrow = c(2,1))

par(mfrow = c(2, 2))

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

barplot(table(data_ineq$mobility3),
        main = "Expect Some Mobility (Self Assesment)",
        names.arg = c("Downward", "No Mobility" ,"Upward"),
        cex.names = 0.8,
        border = F,
        cex.main = 0.8,
        las = 1)

barplot(table(data_ineq$downward_mob),
        main = "Expect Downward Mobility (Self Assesment)",
        names.arg = c("No", "Yes"),
        cex.names = 0.8,
        border = F,
        cex.main = 0.8,
        las = 1)

par(mfrow = c(1, 1))




