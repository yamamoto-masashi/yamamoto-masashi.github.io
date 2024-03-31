# ディレクトリの変更
# ただしウィンドウズの場合
setwd("C:/Rdata")

# パッケージの読み込み
library(ggplot2)
library(tidyverse)

############################################################
######### １標本の平均の検定の例
#############################################################

# 10人の患者にある睡眠薬を飲ませたところ，睡眠時間がそれぞれ
# 次の時間だけ増えました（Arthur R. Cushny and A. Roy Peebles, 
# The Journal of Physiology 32, 501-510 (1905)）：

x = c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4,5.5,1.6,4.6,3.4)

#boxplot(x)
#stripchart(x, pch=16, at=0, method="stack")
#hist(x, right=FALSE, col="gray")


# 帰無仮説： 睡眠薬投与後の睡眠時間の増加はゼロ

t.test(x)

# p<0.05なので、そもそも睡眠薬投与による睡眠時間の増加が
# ゼロという前提が間違えていると結論づける。
# この結論が間違えている確率は5%以下


############################################################
######### 2標本の平均の差の検定の例
#############################################################

# 2組の患者たちに痛みのレベルを報告してもらったところ，
# 次のような結果を得ました (T. Lumley, Biometrics 52, 354-361 (1996))。

treated = c(1,2,1,1,1,1,1,1,1,1,2,4,1,1)
control = c(3,3,4,3,1,2,3,1,1,5,4)

boxplot(treated, control)

t.test(treated, control)

DB<-cbind.data.frame(treated,control)

par(family = "HiraMinProN-W6")
cix = t.test(treated)$conf.int
ciy = t.test(control)$conf.int
dotchart(c(mean(treated),mean(control)), pch=16, xlim=range(cix,ciy),
         xlab="痛みのレベル")
arrows(cix[1], 1, cix[2], 1, length=0.05, angle=90, code=3)
arrows(ciy[1], 2, ciy[2], 2, length=0.05, angle=90, code=3)
mtext(c("対照群","実験群"), side=2, at=1:2, line=0.5, las=1)

