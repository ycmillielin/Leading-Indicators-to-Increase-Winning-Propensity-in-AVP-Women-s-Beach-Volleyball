# import the dataset
bv = read.csv(file.choose())

# for reproduction
set.seed(1)

# split the dataset
sample = sample(c(TRUE, FALSE), nrow(bv), replace = TRUE, prob = c(0.7, 0.3))
train = bv[sample, ]
test = bv[!sample, ]

# train the model on training set
log_model = glm(win ~ mean_age + mean_hgt + sum_tot_attacks + sum_tot_kills + sum_tot_errors + sum_tot_aces + 
                  sum_tot_serve_errors + sum_tot_blocks + sum_tot_digs + mean_tot_hitpct, 
                    family = 'binomial', data = train)
summary(log_model)

# remove insignificant variables and rerun the model
log_model2 = glm(win ~ sum_tot_attacks + sum_tot_kills + sum_tot_errors + sum_tot_aces + sum_tot_serve_errors + 
                  sum_tot_blocks + sum_tot_digs + mean_tot_hitpct, family = 'binomial', data = train)
summary(log_model2)

# make predictions for test set
predict = predict(log_model2, test, type = 'response')
predict = ifelse(predict > 0.5, 1, 0)

confusion_matrix = table(test$win, predict)
confusion_matrix

# calculate the accuracy
errors = mean(predict != test$win)
print(paste('Accuracy =', 1 - errors))

# ROC-AUC Curve
library(ROCR)
ROCPred = prediction(predict, test$win) 
ROCPer = performance(ROCPred, measure = "tpr", x.measure = "fpr")

auc = performance(ROCPred, measure = "auc")
auc = auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc = round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

# final model
final_log_model = glm(win ~ sum_tot_attacks + sum_tot_kills + sum_tot_errors + sum_tot_aces + sum_tot_serve_errors + 
                        sum_tot_blocks + sum_tot_digs + mean_tot_hitpct, family = 'binomial', data = bv)
summary(final_log_model)
