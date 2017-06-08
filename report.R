results = read.csv("results.csv")

yd = read.csv("yougov.csv")
yd$Constituency.Name = gsub(" and ", " & ", yd$Constituency.Name)
names(results)[2:4] = c("Constituency.Name","Party.Name","share")
results$Party.Name = gsub("Lib Dem", "LD", results$Party.Name)
# TODO: do clever matching elsewhere
results[results$Constituency.Name == "Swindon North","Constituency.Name"] = "North Swindon"
m = merge(yd, results,by=c("Constituency.Name","Party.Name"))
m$diff = m$share - m$est
aggregate(diff ~ Party.Name, m, median)
aggregate(diff ~ Constituency.Name, m, median)
setdiff(unique(results$Constituency.Name), unique(yd$Constituency.Name))
write.csv(file="m.csv", m)
