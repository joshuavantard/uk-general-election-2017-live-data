
# Forename,Surname,Description on ballot paper,Constituency Name,PANO,Votes,Share (%),Change,,Incumbent?,,Constituency ID ,Region ID,County ,Region ,Country ,Constituency type,Party name identifier,Party abbreviation

library("plyr")


e2015=read.csv("/Users/plagchk/vote17/RESULTS.csv", as.is=TRUE)
# e2015=read.csv("/Users/plagchk/vote17/ge_2015_results.csv", as.is=TRUE)

eng_2015=subset(e2015, Country == "England")

constit_swing=function(df)
{
con=subset(df, Party.abbreviation == "Con")
lab=subset(df, Party.abbreviation == "Lab")
ldem=subset(df, Party.abbreviation == "LD")
others_swing_amount=sum(subset(eng_2015, Party.abbreviation != "Con" &
					Party.abbreviation != "Lab" &
					Party.abbreviation != "LD")$swing_amount)

# print(con)
# print(lab)
# print(ldem)
# blah()

t=data.frame(ldem=ldem$Share....
		+others_swing_amount
		+con$swing_amount*con_swing+lab$swing_amount*lab_swing,
		con=con$Share....-con$swing_amount*con_swing,
		lab=lab$Share....-lab$swing_amount*lab_swing)
if (nrow(t) == 1)
   t$winner=which(t == max(t))[1]

# print(t)
# blah()

return(t)
}


swing_impact=function(df, swing)
{
df$swing_amount=df$Share....*swing

t=ddply(df, .(Constituency.Name), constit_swing)

return(t)
}

swing_results=function(swing)
{
sw=swing_impact(eng_2015, swing)
win_tab=table(sw$winner)

points(swing, win_tab[1], col="orange")
points(swing, win_tab[2], col="blue")
points(swing, win_tab[3], col="red")
}


con_swing=0.5
lab_swing=1.3

plot(0, 0,
	xlab="Swing", ylab="MPs",
	xlim=c(0.2, 0.4), ylim=c(10, 350))


dummy=sapply(seq(0.2, 0.4, by=0.01), swing_results)

