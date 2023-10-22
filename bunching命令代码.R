install.packages("bunching")

help(bunching)

#You can install the development version from GitHub with:
# install.packages("devtools")
devtools::install_github("mavpanos/bunching")

library(bunching)

install.packages("haven")
library(haven)
data <- read_dta("C:/Users/密大宝饲养员/Desktop/聚束分析法/R语言中bunchr命令/15190高新技术企业20141.dta")
data <- read_dta("C:/Users/密大宝饲养员/Desktop/聚束分析法/R语言中bunchr命令/高新技术企业5000万以下的RD1.dta")


print("Top 6 Entries of data frame:")
head(data,n=10)
print("summary:")
summary(data)

class(data)

plot_hist( z_vector = data$RD1, zstar = 0.09, binwidth = 0.001,
bins_l = 60, bins_r = 90,
p_title = "Notch", p_title_size = 11)$plot

# bin_data(data, binv = "median", 0.06, 0.001, 0.25, 0.15)


notch1 <- bunchit(z_vector = data $ RD1, zstar = 0.09, binwidth = 0.001,
bins_l = 90, bins_r = 60, poly = 5, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE, p_e = TRUE ,p_b_e_xpos = 0.04, seed = 1,
p_title = "Notch without correction")
notch1$plot

#下面为返回输出结果
# zD
notch1$zD
#> [1] 10934
# zD_bin
notch1$zD_bin
#> [1] 19
# alpha
notch1$alpha
#> [1] 0.794406


#第二例，考虑约整数效应
notch2 <- bunchit(z_vector = data $ RD1, zstar = 0.09, binwidth = 0.001,
bins_l = 90, bins_r = 60, poly = 4, t0=0.15, t1=.25, correct = TRUE,
notch = TRUE, p_b = TRUE, p_b_e_xpos = 0.04, seed = 1,
p_title = "Notch with correction from zstar")
notch2$plot

# zD
notch1$zD
#> [1] 10934
# zD_bin
notch1$zD_bin
#> [1] 19
# alpha
notch1$alpha
#> [1] 0.794406


# Now let's see what happens if we instead set correct_above_zu = TRUE:

notch3 <- bunchit(z_vector = data $ RD1, zstar = 0.09, binwidth = 0.001, 
                  bins_l = 90, bins_r = 60, poly = 5, t0=0.15, t1=.25,  correct = TRUE, 
                  notch = TRUE, correct_above_zu = TRUE, p_b = TRUE, p_b_e_xpos = 0.04,
                  seed = 1, p_title = "Notch with correction from zU")
notch3$plot

# zD
notch1$zD
#> [1] 10934
# zD_bin
notch1$zD_bin
#> [1] 19
# alpha
notch1$alpha
#> [1] 0.794406


#图形美化
notch_p <- bunchit(z_vector = bunching_data$notch_vector, zstar = 10000, binwidth = 50, 
                   bins_l = 40, bins_r = 40, poly = 5, t0=0.18, t1=.25, correct = FALSE, 
                   notch = TRUE, p_title = "Notch without correction",
                   p_xtitle = "Earnings", p_ytitle = "Bin Count",
                   p_title_size = 15, p_axis_title_size = 13, p_axis_val_size = 11, 
                   p_grid_major_y_color = "white",  p_cf_color =  "red", 
                   p_freq_color = "#1A476F", p_freq_size = .8, p_zstar_color = "black",
                   p_freq_msize = 1.5, p_maxy = 2500, p_ybreaks = c(1000,2000), 
                   p_b = TRUE, p_b_e_size = 5, p_b_e_xpos = 8700, p_b_e_ypos = 1500, 
                   seed = 1, p_domregion_color = "black", p_domregion_ltype = "dotted")
notch_p$plot


#尝试RD2
data <- read_dta("C:/Users/密大宝饲养员/Desktop/聚束分析法/R语言中bunchr命令/高新技术企业5000万以下的RD1.dta")
head(data,n=10)

plot_hist( z_vector = data$RD2, zstar = 9, binwidth = 0.1,
bins_l = 60, bins_r = 90,
p_title = "Notch", p_title_size = 11)$plot

#省略
notch1 <- bunchit(z_vector = data $ RD1, zstar = 0.09, binwidth = 0.001,
bins_l = 90, bins_r = 60, poly = 5, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE, p_e = TRUE ,p_b_e_xpos = 0.04, seed = 1,
p_title = "Notch without correction")
notch1$plot

#测试高项次数为9
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 9, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
#划分占优区域的数值
notch1$zD
#z星上方的箱子划分了主导区域
notch1$zD_bin
#通过设置force_notch = FALSE，从缺口设置中估计的zU（排除区域的上范围）的位置。
notch1$zU_bin
notch1$marginal_buncher
notch1$marginal_buncher_vector
notch1$marginal_buncher_sd
notch1$alpha
notch1$alpha_vector
notch1$alpha_sd
notch1$cf
notch1$B
notch1$B_vector
notch1$B_sd
notch1$b
notch1$b_vector
notch1$b_sd
notch1$e
notch1$e_vector
notch1$e_sd


#能够看到弹性数值的估计产生了较大的变化，返回RD1查看是否可以给出弹性的准确估计
notch1 <- bunchit(z_vector = data $ RD1, zstar = 0.09, binwidth = 0.001,
bins_l = 90, bins_r = 60, poly = 9, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 0.04, seed = 1,
p_title = "Notch without correction")
notch1$plot
#划分占优区域的数值
notch1$zD
#z星上方的箱子划分了主导区域
notch1$zD_bin
#通过设置force_notch = FALSE，从缺口设置中估计的zU（排除区域的上范围）的位置。
notch1$zU_bin
notch1$marginal_buncher
notch1$marginal_buncher_vector
notch1$marginal_buncher_sd
notch1$alpha
notch1$alpha_vector
notch1$alpha_sd
notch1$cf
notch1$B
notch1$B_vector
notch1$B_sd
notch1$b
notch1$b_vector
notch1$b_sd
notch1$e
notch1$e_vector
notch1$e_sd



#测试高阶项次数为8
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 8, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha

#测试高阶项次数为7
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 7, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha

#测试高阶项次数为6
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 6, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha

#测试高阶项次数为5
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 5, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha

#测试高阶项次数为4
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 4, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha


#测试高阶项次数为3
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 3, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$zD
notch1$zD_bin
notch1$alpha
notch1$zU_bin


#测试高阶项次数为2
notch1 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 2, t0=0.15, t1=.25, correct = FALSE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch without correction")
notch1$plot
notch1$b
notch1$b_sd
notch1$zD
notch1$zD_bin
notch1$alpha
notch1$zU_bin


#RD2情况下，考虑约整数效应
#积分约束校正可以通过两种不同的方式应用。主要区别在于，我们是从z*上方的bin开始向上移动反事实，还是从zU上方的bin向上移动。
#先看第一种情况的结果，其中我们将correct设置为TRUE，并依赖于默认值correct_above_zu=FALSE：

notch2 <- bunchit(z_vector = data $ RD2, zstar = 9, binwidth = 0.1,
bins_l = 90, bins_r = 60, poly = 4, t0=0.15, t1=.25, correct = TRUE,
notch = TRUE, p_b = TRUE ,p_b_e_xpos = 4, seed = 1,
p_title = "Notch with correction from zstar")
notch2$plot
notch2$zD
notch2$zD_bin
notch2$alpha
notch2$zU_bin

# #解释：在这种情况下，我们发现反事实分布发生了更多的变化。这对b的估计没有太大影响，因为z＊处的反事实保持稳定。然而，它可能会使自举估计变得不稳定，从而导致标准误差。此外，它可以降低α，因为它将反事实符号向上倾斜并增加缺失质量。在zU远高于z的情况下，预计会产生这种影响
# 
# *，因为它接近带宽的极限，并迫使相同的质量由更少的仓来计算，从而将反事实转移到zU右侧，使其达到比其他情况高得多的水平，这也将其拉高到z*和zU之间的仓。在这种情况下，最好考虑转移
# 
# 从z*（默认值）向上，或根据目视检查将zU强制设置为某个值。


#其他命令学习
#估计占优区域上界
domregion(zstar, t0, t1, binwidth)


#Estimate bunching on bootstrapped samples, using residual-based bootstrapping with replacement.
#使用带替换的基于残差的自举法估计自举样本上的积聚
do_bootstrap(
zstar,
binwidth,
firstpass_prep,
residuals,
n_boot = 100,
correct = TRUE,
correct_iter_max = 200,
notch = FALSE,
zD_bin = NA,
seed = NA
)

#从单个归一化的聚束中估计弹性
elasticity(beta=2,
binwidth=0.1,
zstar=9,
t0=0.15,
t1=0.25,
notch = TRUE)


#估计边缘聚束者的位置
marginal_buncher(beta=2, binwidth=0.1, zstar=9, notch = TRUE)


#基于缺口设置中的参数效用函数定义无差异条件。用于参数化求解弹性。
notch_equation(e = .04, t0 = 0.15, t1 = .25, zstar = 9, dzstar=12)


#作图，plot_bunching中可以包括聚束质量和弹性估计
plot_bunching(z_vector = data$RD2, zstar = 9,
binwidth = 0.1, bins_excl_l = 0 , bins_excl_r = 0,
b = 0.439, b_sd = 0.227, p_b = TRUE)

#发现缺少参数binned_data等，寻找


#首先可以通过bin_data命令对原始数据进行分组,bin_data返回一个带有bins和相应频率（计数）的数据帧
bin_data

binned_data <- bin_data(z_vector = data$RD2, zstar =9,
binwidth = 0.1, bins_l = 90, bins_r = 60)
head(binned_data)

#将聚束模型拟合到（分类）数据中，并估计多余的质量
fit_bunching(thedata, themodelformula, binwidth, notch = FALSE, zD_bin = NA)

#积分约束校正，do_correction实现对积分约束的修正
corrected <- do_correction(zstar = 9, binwidth = 0.1,
data_prepped = prepped_data$data_binned,
firstpass_results = firstpass,notch= TRUE)

paste0("Without correction, b = ", firstpass$b_estimate)
paste0("With correction, b = ", round(corrected$b_corrected,3))

#准备分箱数据和模型
prep_data_for_fit

prepped_data <- prep_data_for_fit(binned_data, zstar = 10000, binwidth = 50,
bins_l = 20, bins_r = 20, poly = 4,
bins_excl_l = 2, bins_excl_r = 3,
rn = c(250,500), extra_fe = 10200)
head(prepped_data$data_binned)
prepped_data$model_formula




#图片美化


#R语言将更改后的数据导出到excel中
# 首先需要安装并加载 "writexl" 包
install.packages("writexl")
library(writexl)

# 假设你的数据表名为 modified_data
write_xlsx(modified_data, path = "modified_data.xlsx")



#查看命令（函数）背后的源代码
例如我们写下下面的函数

matchar  <- function(name) {
  
  name = unlist(name)
  
  name = as.character(name)
  
  return(name)
  
}

将它保存为matchar.r

如果他的保存路径为C:pa

那么下次调用这个函数时，不必重新定义

只要

path="C:/pa" #声明matchar.r所在位置

setwd(path)  #把工作路径设置到path

source('matchar.r')  #“预装“函数

这时候如果你输入matchar

R工作台就成显示matchar函数的代码，说明预装成功，可以使用了。

