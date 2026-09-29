############################################################
# R Statistical Analysis Example
# 包含：
# 1. 安装和加载R包
# 2. 创建模拟医学数据
# 3. 数据查看与清洗
# 4. 缺失值处理
# 5. 描述性统计
# 6. 正态性检验
# 7. t检验
# 8. 卡方检验
# 9. ANOVA
# 10. 相关分析
# 11. 线性回归
# 12. Logistic回归
# 13. 生存分析
# 14. 数据可视化
############################################################


############################
# 1. 清空当前工作环境
############################

rm(list = ls())

# 查看当前工作目录
getwd()

# 如有需要，可以修改工作目录
# setwd("D:/R_project")


############################
# 2. 安装和加载需要的R包
############################

packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "survival",
  "survminer"
)

# 检查哪些包尚未安装
new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

# 如果存在未安装的包，则自动安装
if(length(new_packages) > 0){
  install.packages(new_packages)
}

# 加载包
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(survival)
library(survminer)


############################
# 3. 设置随机种子
############################

set.seed(2026)


############################
# 4. 创建模拟医学研究数据
############################

n <- 300

medical_data <- data.frame(

  # 患者编号
  patient_id = 1:n,

  # 年龄
  age = round(
    rnorm(
      n,
      mean = 60,
      sd = 12
    )
  ),

  # 性别
  sex = sample(
    c("Male", "Female"),
    n,
    replace = TRUE,
    prob = c(0.58, 0.42)
  ),

  # BMI
  bmi = round(
    rnorm(
      n,
      mean = 24,
      sd = 3.5
    ),
    1
  ),

  # 吸烟状态
  smoking = sample(
    c("Never", "Former", "Current"),
    n,
    replace = TRUE,
    prob = c(0.50, 0.30, 0.20)
  ),

  # 是否患高血压
  hypertension = sample(
    c(0, 1),
    n,
    replace = TRUE,
    prob = c(0.60, 0.40)
  ),

  # 是否患糖尿病
  diabetes = sample(
    c(0, 1),
    n,
    replace = TRUE,
    prob = c(0.80, 0.20)
  ),

  # 分组
  treatment_group = sample(
    c("Control", "Treatment"),
    n,
    replace = TRUE
  ),

  # 肿瘤分期
  stage = sample(
    c("I", "II", "III", "IV"),
    n,
    replace = TRUE,
    prob = c(0.20, 0.30, 0.30, 0.20)
  ),

  # 实验室指标
  biomarker = round(
    rnorm(
      n,
      mean = 50,
      sd = 15
    ),
    2
  )
)


############################
# 5. 根据变量生成结局
############################

# 连续型结局
medical_data$clinical_score <-
  60 +
  0.15 * medical_data$age +
  3 * medical_data$diabetes +
  4 * (medical_data$treatment_group == "Treatment") +
  rnorm(n, 0, 8)


# Logistic模型中的线性预测值
linear_predictor <-
  -4 +
  0.04 * medical_data$age +
  0.7 * medical_data$diabetes +
  0.6 * medical_data$hypertension +
  0.8 * (medical_data$stage == "IV")


# 转换为概率
event_probability <-
  exp(linear_predictor) /
  (1 + exp(linear_predictor))


# 二分类疾病结局
medical_data$disease_event <-
  rbinom(
    n,
    size = 1,
    prob = event_probability
  )


############################
# 6. 创建生存时间数据
############################

medical_data$survival_time <-
  round(
    rexp(
      n,
      rate = 0.03
    ),
    1
  )

medical_data$death <-
  rbinom(
    n,
    size = 1,
    prob = 0.40
  )


############################
# 7. 人为加入部分缺失值
############################

missing_index_bmi <-
  sample(
    1:n,
    15
  )

medical_data$bmi[
  missing_index_bmi
] <- NA


missing_index_biomarker <-
  sample(
    1:n,
    20
  )

medical_data$biomarker[
  missing_index_biomarker
] <- NA


############################
# 8. 查看数据结构
############################

head(medical_data)

tail(medical_data)

dim(medical_data)

names(medical_data)

str(medical_data)

summary(medical_data)


############################
# 9. 将变量转换为因子
############################

medical_data$sex <-
  factor(
    medical_data$sex
  )

medical_data$smoking <-
  factor(
    medical_data$smoking
  )

medical_data$treatment_group <-
  factor(
    medical_data$treatment_group
  )

medical_data$stage <-
  factor(
    medical_data$stage,
    levels = c(
      "I",
      "II",
      "III",
      "IV"
    )
  )

medical_data$hypertension <-
  factor(
    medical_data$hypertension,
    levels = c(0, 1),
    labels = c(
      "No",
      "Yes"
    )
  )

medical_data$diabetes <-
  factor(
    medical_data$diabetes,
    levels = c(0, 1),
    labels = c(
      "No",
      "Yes"
    )
  )


############################
# 10. 检查缺失值
############################

# 每列缺失值数量
colSums(
  is.na(
    medical_data
  )
)


# 每列缺失比例
missing_rate <-
  colMeans(
    is.na(
      medical_data
    )
  )

missing_rate


############################
# 11. 删除完全缺失的行（如存在）
############################

medical_complete <-
  medical_data[
    complete.cases(
      medical_data
    ),
  ]

dim(medical_complete)


############################
# 12. 使用中位数填补BMI
############################

median_bmi <-
  median(
    medical_data$bmi,
    na.rm = TRUE
  )

medical_data$bmi_imputed <-
  ifelse(
    is.na(
      medical_data$bmi
    ),
    median_bmi,
    medical_data$bmi
  )


############################
# 13. 使用均值填补biomarker
############################

mean_biomarker <-
  mean(
    medical_data$biomarker,
    na.rm = TRUE
  )

medical_data$biomarker_imputed <-
  ifelse(
    is.na(
      medical_data$biomarker
    ),
    mean_biomarker,
    medical_data$biomarker
  )


############################
# 14. 描述性统计
############################

# 年龄均值
mean(
  medical_data$age
)

# 年龄标准差
sd(
  medical_data$age
)

# 年龄中位数
median(
  medical_data$age
)

# 四分位数
quantile(
  medical_data$age
)

# BMI均值
mean(
  medical_data$bmi,
  na.rm = TRUE
)

# BMI标准差
sd(
  medical_data$bmi,
  na.rm = TRUE
)


############################
# 15. 分类变量频数统计
############################

table(
  medical_data$sex
)

prop.table(
  table(
    medical_data$sex
  )
)

table(
  medical_data$stage
)

prop.table(
  table(
    medical_data$stage
  )
)


############################
# 16. 按治疗组计算年龄
############################

medical_data %>%
  group_by(
    treatment_group
  ) %>%
  summarise(

    n = n(),

    mean_age =
      mean(
        age
      ),

    sd_age =
      sd(
        age
      ),

    median_age =
      median(
        age
      ),

    min_age =
      min(
        age
      ),

    max_age =
      max(
        age
      )
  )


############################
# 17. 按性别统计BMI
############################

medical_data %>%
  group_by(
    sex
  ) %>%
  summarise(

    n = n(),

    mean_bmi =
      mean(
        bmi,
        na.rm = TRUE
      ),

    sd_bmi =
      sd(
        bmi,
        na.rm = TRUE
      )
  )


############################
# 18. 正态性检验
############################

shapiro.test(
  medical_data$clinical_score
)


# 注意：
# 大样本情况下Shapiro-Wilk检验很敏感
# 因此通常同时结合直方图和Q-Q图判断


############################
# 19. 绘制直方图
############################

ggplot(
  medical_data,
  aes(
    x = clinical_score
  )
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Distribution of Clinical Score",
    x = "Clinical Score",
    y = "Frequency"
  ) +
  theme_classic()


############################
# 20. Q-Q图
############################

ggplot(
  medical_data,
  aes(
    sample = clinical_score
  )
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Clinical Score"
  ) +
  theme_classic()


############################
# 21. 两独立样本t检验
############################

t_test_result <-
  t.test(
    clinical_score ~ treatment_group,
    data = medical_data
  )

t_test_result


############################
# 22. 查看两组均值
############################

medical_data %>%
  group_by(
    treatment_group
  ) %>%
  summarise(

    mean_score =
      mean(
        clinical_score
      ),

    sd_score =
      sd(
        clinical_score
      ),

    n =
      n()
  )


############################
# 23. 非参数检验
############################

wilcox.test(
  clinical_score ~ treatment_group,
  data = medical_data
)


############################
# 24. 卡方检验
############################

sex_group_table <-
  table(
    medical_data$sex,
    medical_data$treatment_group
  )

sex_group_table

chisq.test(
  sex_group_table
)


############################
# 25. 糖尿病与结局的卡方检验
############################

diabetes_event_table <-
  table(
    medical_data$diabetes,
    medical_data$disease_event
  )

diabetes_event_table

chisq.test(
  diabetes_event_table
)


############################
# 26. Fisher精确检验
############################

fisher.test(
  diabetes_event_table
)


############################
# 27. 单因素ANOVA
############################

anova_model <-
  aov(
    clinical_score ~ stage,
    data = medical_data
  )

summary(
  anova_model
)


############################
# 28. ANOVA后的多重比较
############################

TukeyHSD(
  anova_model
)


############################
# 29. 相关性分析
############################

cor(
  medical_data$age,
  medical_data$clinical_score,
  method = "pearson"
)


############################
# 30. Pearson相关检验
############################

cor.test(
  medical_data$age,
  medical_data$clinical_score,
  method = "pearson"
)


############################
# 31. Spearman相关分析
############################

cor.test(
  medical_data$age,
  medical_data$clinical_score,
  method = "spearman"
)


############################
# 32. 简单线性回归
############################

linear_model_1 <-
  lm(
    clinical_score ~ age,
    data = medical_data
  )

summary(
  linear_model_1
)


############################
# 33. 多元线性回归
############################

linear_model_2 <-
  lm(
    clinical_score ~
      age +
      sex +
      bmi_imputed +
      hypertension +
      diabetes +
      treatment_group,
    data = medical_data
  )

summary(
  linear_model_2
)


############################
# 34. 线性回归95%置信区间
############################

confint(
  linear_model_2
)


############################
# 35. 查看线性回归残差
############################

par(
  mfrow = c(
    2,
    2
  )
)

plot(
  linear_model_2
)

par(
  mfrow = c(
    1,
    1
  )
)


############################
# 36. Logistic回归
############################

logistic_model <-
  glm(
    disease_event ~
      age +
      sex +
      bmi_imputed +
      hypertension +
      diabetes +
      stage,
    family = binomial(
      link = "logit"
    ),
    data = medical_data
  )

summary(
  logistic_model
)


############################
# 37. 提取OR值
############################

OR <-
  exp(
    coef(
      logistic_model
    )
  )

OR


############################
# 38. 提取OR及95%CI
############################

OR_CI <-
  exp(
    confint(
      logistic_model
    )
  )

OR_CI


############################
# 39. 整理Logistic结果
############################

logistic_result <-
  data.frame(

    Variable =
      names(
        coef(
          logistic_model
        )
      ),

    OR =
      exp(
        coef(
          logistic_model
        )
      ),

    Lower_CI =
      exp(
        confint(
          logistic_model
        )[, 1]
      ),

    Upper_CI =
      exp(
        confint(
          logistic_model
        )[, 2]
      )
  )

logistic_result


############################
# 40. 生存分析
############################

survival_object <-
  Surv(
    time =
      medical_data$survival_time,

    event =
      medical_data$death
  )


############################
# 41. Kaplan-Meier分析
############################

km_model <-
  survfit(
    survival_object ~ treatment_group,
    data = medical_data
  )

summary(
  km_model
)


############################
# 42. Kaplan-Meier曲线
############################

ggsurvplot(
  km_model,
  data = medical_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Follow-up Time",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curve"
)


############################
# 43. Log-rank检验
############################

survdiff(
  survival_object ~ treatment_group,
  data = medical_data
)


############################
# 44. Cox比例风险模型
############################

cox_model <-
  coxph(
    survival_object ~
      age +
      sex +
      treatment_group +
      diabetes +
      hypertension +
      stage,
    data = medical_data
  )

summary(
  cox_model
)


############################
# 45. Cox模型HR
############################

cox_HR <-
  exp(
    coef(
      cox_model
    )
  )

cox_HR


############################
# 46. Cox模型95%CI
############################

cox_CI <-
  exp(
    confint(
      cox_model
    )
  )

cox_CI


############################
# 47. 检查比例风险假设
############################

cox_ph_test <-
  cox.zph(
    cox_model
  )

cox_ph_test

plot(
  cox_ph_test
)


############################
# 48. 年龄分布图
############################

ggplot(
  medical_data,
  aes(
    x = age
  )
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Frequency"
  ) +
  theme_classic()


############################
# 49. BMI箱线图
############################

ggplot(
  medical_data,
  aes(
    x = treatment_group,
    y = bmi
  )
) +
  geom_boxplot() +
  labs(
    title = "BMI by Treatment Group",
    x = "Treatment Group",
    y = "BMI"
  ) +
  theme_classic()


############################
# 50. 临床评分箱线图
############################

ggplot(
  medical_data,
  aes(
    x = treatment_group,
    y = clinical_score
  )
) +
  geom_boxplot() +
  geom_jitter(
    width = 0.15,
    alpha = 0.4
  ) +
  labs(
    title = "Clinical Score by Treatment Group",
    x = "Treatment Group",
    y = "Clinical Score"
  ) +
  theme_classic()


############################
# 51. 年龄与临床评分散点图
############################

ggplot(
  medical_data,
  aes(
    x = age,
    y = clinical_score
  )
) +
  geom_point(
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Association Between Age and Clinical Score",
    x = "Age",
    y = "Clinical Score"
  ) +
  theme_classic()


############################
# 52. 不同肿瘤分期的评分
############################

ggplot(
  medical_data,
  aes(
    x = stage,
    y = clinical_score
  )
) +
  geom_boxplot() +
  labs(
    title = "Clinical Score Across Tumor Stages",
    x = "Tumor Stage",
    y = "Clinical Score"
  ) +
  theme_classic()


############################
# 53. 绘制性别频数柱状图
############################

ggplot(
  medical_data,
  aes(
    x = sex
  )
) +
  geom_bar() +
  labs(
    title = "Sex Distribution",
    x = "Sex",
    y = "Number of Patients"
  ) +
  theme_classic()


############################
# 54. 分组柱状图
############################

ggplot(
  medical_data,
  aes(
    x = stage,
    fill = treatment_group
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title = "Tumor Stage by Treatment Group",
    x = "Tumor Stage",
    y = "Number of Patients",
    fill = "Treatment Group"
  ) +
  theme_classic()


############################
# 55. 创建年龄分组
############################

medical_data <-
  medical_data %>%
  mutate(

    age_group =
      case_when(

        age < 50 ~
          "<50",

        age >= 50 &
          age < 60 ~
          "50-59",

        age >= 60 &
          age < 70 ~
          "60-69",

        age >= 70 ~
          "≥70"

      )
  )


############################
# 56. 查看年龄分组
############################

table(
  medical_data$age_group
)


############################
# 57. 创建BMI分类变量
############################

medical_data <-
  medical_data %>%
  mutate(

    bmi_group =
      case_when(

        bmi_imputed < 18.5 ~
          "Underweight",

        bmi_imputed >= 18.5 &
          bmi_imputed < 24 ~
          "Normal",

        bmi_imputed >= 24 &
          bmi_imputed < 28 ~
          "Overweight",

        bmi_imputed >= 28 ~
          "Obesity"

      )
  )


############################
# 58. BMI分类统计
############################

table(
  medical_data$bmi_group
)

prop.table(
  table(
    medical_data$bmi_group
  )
)


############################
# 59. 创建综合描述统计表
############################

summary_table <-
  medical_data %>%
  group_by(
    treatment_group
  ) %>%
  summarise(

    Number =
      n(),

    Mean_Age =
      round(
        mean(
          age
        ),
        2
      ),

    SD_Age =
      round(
        sd(
          age
        ),
        2
      ),

    Mean_BMI =
      round(
        mean(
          bmi_imputed
        ),
        2
      ),

    SD_BMI =
      round(
        sd(
          bmi_imputed
        ),
        2
      ),

    Mean_Clinical_Score =
      round(
        mean(
          clinical_score
        ),
        2
      ),

    SD_Clinical_Score =
      round(
        sd(
          clinical_score
        ),
        2
      )
  )

summary_table


############################
# 60. 输出数据为CSV
############################

write.csv(
  medical_data,
  "medical_data.csv",
  row.names = FALSE
)


############################
# 61. 输出统计结果
############################

write.csv(
  summary_table,
  "summary_table.csv",
  row.names = FALSE
)

write.csv(
  logistic_result,
  "logistic_regression_result.csv",
  row.names = FALSE
)


############################
# 62. 示例：读取CSV文件
############################

# 如果你有自己的数据，可以使用下面代码
#
# my_data <- read.csv(
#   "your_data.csv"
# )
#
# head(my_data)
#
# str(my_data)
#
# summary(my_data)


############################
# 63. 示例：读取Excel数据
############################

# 首先安装readxl包
#
# install.packages("readxl")
#
# library(readxl)
#
# excel_data <-
#   read_excel(
#     "your_data.xlsx",
#     sheet = 1
#   )
#
# head(excel_data)


############################
# 64. 示例：筛选患者
############################

older_patients <-
  medical_data %>%
  filter(
    age >= 65
  )

head(
  older_patients
)


############################
# 65. 示例：选择特定变量
############################

selected_data <-
  medical_data %>%
  select(
    patient_id,
    age,
    sex,
    bmi_imputed,
    stage,
    disease_event
  )

head(
  selected_data
)


############################
# 66. 示例：排序
############################

sorted_data <-
  medical_data %>%
  arrange(
    desc(
      age
    )
  )

head(
  sorted_data
)


############################
# 67. 找年龄最大的10名患者
############################

oldest_10 <-
  medical_data %>%
  arrange(
    desc(
      age
    )
  ) %>%
  slice_head(
    n = 10
  )

oldest_10


############################
# 68. 计算事件发生率
############################

event_rate <-
  mean(
    medical_data$disease_event
  )

event_rate


############################
# 69. 按治疗组计算事件率
############################

medical_data %>%
  group_by(
    treatment_group
  ) %>%
  summarise(

    Event_Number =
      sum(
        disease_event
      ),

    Total =
      n(),

    Event_Rate =
      mean(
        disease_event
      )
  )


############################
# 70. 按肿瘤分期计算事件率
############################

medical_data %>%
  group_by(
    stage
  ) %>%
  summarise(

    Event_Number =
      sum(
        disease_event
      ),

    Total =
      n(),

    Event_Rate =
      mean(
        disease_event
      )
  )


############################
# 71. 计算不同分期的平均生存时间
############################

medical_data %>%
  group_by(
    stage
  ) %>%
  summarise(

    Mean_Survival =
      mean(
        survival_time
      ),

    Median_Survival =
      median(
        survival_time
      ),

    SD_Survival =
      sd(
        survival_time
      )
  )


############################
# 72. 简单循环示例
############################

variables <-
  c(
    "age",
    "bmi_imputed",
    "clinical_score",
    "biomarker_imputed"
  )

for(variable in variables){

  cat(
    "\n---------------------------------\n"
  )

  cat(
    "Variable:",
    variable,
    "\n"
  )

  x <-
    medical_data[
      [variable]
    ]

  cat(
    "Mean:",
    mean(
      x,
      na.rm = TRUE
    ),
    "\n"
  )

  cat(
    "SD:",
    sd(
      x,
      na.rm = TRUE
    ),
    "\n"
  )

  cat(
    "Median:",
    median(
      x,
      na.rm = TRUE
    ),
    "\n"
  )
}


############################
# 73. 自定义描述统计函数
############################

descriptive_statistics <-
  function(x){

    result <-
      c(

        N =
          sum(
            !is.na(
              x
            )
          ),

        Mean =
          mean(
            x,
            na.rm = TRUE
          ),

        SD =
          sd(
            x,
            na.rm = TRUE
          ),

        Median =
          median(
            x,
            na.rm = TRUE
          ),

        Minimum =
          min(
            x,
            na.rm = TRUE
          ),

        Maximum =
          max(
            x,
            na.rm = TRUE
          )

      )

    return(
      result
    )
  }


############################
# 74. 使用自定义函数
############################

descriptive_statistics(
  medical_data$age
)

descriptive_statistics(
  medical_data$bmi_imputed
)

descriptive_statistics(
  medical_data$clinical_score
)


############################
# 75. 创建一个简单函数计算OR
############################

calculate_or <-
  function(model){

    OR <-
      exp(
        coef(
          model
        )
      )

    CI <-
      exp(
        confint(
          model
        )
      )

    result <-
      data.frame(

        Variable =
          names(
            OR
          ),

        OR =
          OR,

        Lower95CI =
          CI[, 1],

        Upper95CI =
          CI[, 2]

      )

    return(
      result
    )
  }


############################
# 76. 使用OR函数
############################

calculate_or(
  logistic_model
)


############################
# 77. 保存图片
############################

plot_age <-
  ggplot(
    medical_data,
    aes(
      x = age
    )
  ) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Frequency"
  ) +
  theme_classic()


ggsave(
  filename = "age_distribution.png",
  plot = plot_age,
  width = 7,
  height = 5,
  dpi = 300
)


############################
# 78. 保存治疗组评分图
############################

plot_score <-
  ggplot(
    medical_data,
    aes(
      x = treatment_group,
      y = clinical_score
    )
  ) +
  geom_boxplot() +
  labs(
    title = "Clinical Score by Treatment Group",
    x = "Treatment Group",
    y = "Clinical Score"
  ) +
  theme_classic()


ggsave(
  filename = "clinical_score_boxplot.png",
  plot = plot_score,
  width = 7,
  height = 5,
  dpi = 300
)


############################
# 79. 显示当前R版本
############################

R.version.string


############################
# 80. 查看当前session信息
############################

sessionInfo()


############################################################
# Analysis Finished
############################################################
