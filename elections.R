setwd("~/ge2017")
# https://github.com/six50/pipeline
ge15 = read.csv("https://s3-eu-west-1.amazonaws.com/sixfifty/ge_2010_results.csv")
ge10 = read.csv("https://s3-eu-west-1.amazonaws.com/sixfifty/ge_2015_results.csv")
data = cbind(ge10[,c("Constituency.Name","Electorate","Votes","Lab","Con","LD","Grn","UKIP","SNP")], data.frame(year=2010))
data = rbind(data, cbind(ge15[,c("Constituency.Name","Electorate","Lab","LD","UKIP","SNP")], data.frame(Con=ge15$C, Grn=ge15$Green,Votes=ge15$Valid.Votes, year=2015)))
data$Constituency.Name = gsub("&", "and", data$Constituency.Name)
data$Lab = data$Lab+ge15$Lab.Co.op
# https://docs.google.com/spreadsheets/d/1wTK5dV2_YjCMsUYlwg0l48uWWf44sKgG8uFVMv5OWlA/edit#gid=893960794
euref = read.csv("Revised estimates of leave vote in Westminster constituencies - demog_based_estimates.csv")
euref=data.frame(Constituency.Name=euref$Constituency, leave=euref$Figure.to.use, year=2016)
m = merge(data, euref[,c("Constituency.Name","leave")],by="Constituency.Name",all=T)
write.csv(x = m, file = "combined.csv")

library(rjson)
# https://yougov.co.uk/uk-general-election-2017/
ygov = rjson::fromJSON(file="constituency_detailed_results.json")
yd = data.frame()
for (i in 2:length(ygov)) {
  jitems = ygov[[i]]$d
  for (p in 1:length(jitems)) {
    if (length(jitems[[p]]) == 3) {
      item = data.frame(i=i, party=p)
      item[,c("est","lo","hi")] = jitems[[p]]
      yd = rbind(yd, item)
    }
  }
}

cnames = data.frame(Constituency.Name=readLines(file("names.txt")))
cnames$i = 2:(nrow(cnames)+1)
yd = merge(yd,cnames)
parties = data.frame()
parties = rbind(parties, data.frame(p=1,Party.Name="Con"))
parties = rbind(parties, data.frame(p=2,Party.Name="Lab"))
parties = rbind(parties, data.frame(p=3,Party.Name="LD"))
parties = rbind(parties, data.frame(p=4,Party.Name="UKIP"))
parties = rbind(parties, data.frame(p=5,Party.Name="SNP"))
parties = rbind(parties, data.frame(p=6,Party.Name="Plaid"))
parties = rbind(parties, data.frame(p=7,Party.Name="Grn"))
parties = rbind(parties, data.frame(p=8,Party.Name="Other"))
parties$party = parties$p
yd = merge(yd, parties)

write.csv(x = yd, file = "yougov.csv")


guardian = rjson::fromJSON(file="full.json")
ygest = reshape(yd[,c("Constituency.Name","Party.Name","est")], timevar="Party.Name", idvar="Constituency.Name", direction="wide")
names(ygest) = gsub("est.","pct.", names(ygest))
ygest$year = 2017

for (party in intersect(names(m), parties$Party.Name)) {
  m[,paste0("pct.", party)] = m[,party] / m$Votes * 100
}

library(plyr)

mm = rbind.fill(m, ygest[,intersect(names(m), names(ygest))])
mm = merge(mm[,grep("leave",invert = T, names(mm))], aggregate(leave ~ Constituency.Name, mm, max))
mm = arrange(mm, Constituency.Name, year)
mm[mm$year==2017,"leave"] = mm[mm$year==2015,"leave"] 

dm = within(merge(mm[mm$year==2015,],mm[mm$year==2017,],by="Constituency.Name"), { 
  diff.Lab = pct.Lab.y - pct.Lab.x
  diff.Con = pct.Con.y - pct.Con.x
  diff.LD = pct.LD.y - pct.LD.x
  diff.UKIP=pct.UKIP.y-pct.UKIP.x
  diff.SNP=pct.SNP.y-pct.SNP.x
  diff.GRN=pct.Grn.y-pct.Grn.x
})
dm$leave = dm$leave.x
names(dm) = gsub("pct.([^\\.]+).x", "pct15.\\1", names(dm))
names(dm) = gsub("pct.([^\\.]+).y", "pct17.\\1", names(dm))
names(dm) = gsub("diff.([^\\.]+)", "diff17.\\1", names(dm))
dm = dm[!is.na(dm$Constituency.Name),]
dm = dm[,grep("Constituency|leave$|diff17|pct10|pct15|pct17", names(dm),value=T)]

dm = within(merge(dm,mm[mm$year==2010,],by="Constituency.Name"), { 
  diff15.Lab = pct15.Lab - pct.Lab
  diff15.LD = pct15.LD - pct.LD
  diff15.Con = pct15.Con - pct.Con
  diff15.SNP = pct15.SNP - pct.SNP
  diff15.Grn = pct15.Grn - pct.Grn
  diff15.UKIP = pct15.UKIP - pct.UKIP
})
dm$leave = dm$leave.x
names(dm) = gsub("pct\\.([^\\.]+)", "pct10.\\1", names(dm))
dm = dm[,grep("Constituency|leave$|diff1[57]|pct10|pct15|pct17", names(dm),value=T)]
  colmapping = data.frame(party=c("UKIP","Grn","SNP","Con","LD","Lab"), colour=c("purple","green", "yellow","blue", "orange","red"))
dm$leave_pos=order(dm$leave)
  
prepare = function(name,title,vname="leave") {
  
  x = dm[,grep(paste0("Constituency.Name|",vname,"$|",name), names(dm), value=T)]
  names(x) = gsub(paste0(name,"."), "",names(x))
  melted = melt(x, id.vars = c(vname,"Constituency.Name"))
  ggplot(melted, aes_string(x=vname,y="value",colour="variable"))+stat_smooth()+scale_colour_manual(values=c("GRN" = "green","UKIP"="purple","Con"="blue","Lab"="red","SNP"="yellow","LD"="orange","Grn" = "green"))+ggtitle(title)
}
library(gridExtra)
grid.arrange(prepare("diff17","%points difference between YG2017 and GE2015"),prepare("diff15", "%points difference between GE2015 and GE2010"))
grid.arrange(prepare("diff17","%points difference between YG2017 and GE2015","leave_pos"),prepare("diff15", "%points difference between GE2015 and GE2010","leave_pos"))
write.csv(x=dm, file = "dm.csv")

lm(leave*100 ~ pct.Lab + pct.LD + pct.Con + pct.UKIP, mm[mm$year == 2017,])

results = read.csv("results.csv")
yd = read.csv("yougov.csv")
names(results)[2:4] = c("Constituency.Name","Party.Name","share")
m = merge(yd, results,by=c("Constituency.Name","Party.Name"))
m$diff = m$share - m$est
aggregate(diff ~ Party.Name, m, median)
setdiff(unique(results$Constituency.Name), unique(m$Constituency.Name))
