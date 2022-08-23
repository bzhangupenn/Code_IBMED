setwd("C:/Users/bzhan/Desktop/JM")
dt = read.csv('CM_longitudinal_cox_new_comp.csv')

library(survminer)

ggsurvplot(
  fit = survfit(Surv(dt$Day1, dt$Day2, 
                     dt$Outcome_onlydeath) ~ 1, data = dt), 
  conf.int = TRUE,
  conf.int.alpha = 0.2,
  conf.int.fill = 'red',
  legend = 'none',
  ylim = c(0.7, 1),
  xlab = "Days", 
  ylab = "Survival probability",
  ggtheme = theme_bw(base_size = 25))

# Expand dt by further specifying measurements at baseline
foo <- function(MP) return(which(dt$MP == MP)[1])

dt_baseline = data.frame(dt$Right.MCA.Vs.SD[sapply(dt$MP, foo)], 
                         dt$Right.MCA.Vd.SD[sapply(dt$MP, foo)], 
                         dt$Right.MCA.Vm.SD[sapply(dt$MP, foo)],
                         dt$Left.MCA.Vs.SD[sapply(dt$MP, foo)], 
                         dt$Left..MCA.Vd.SD[sapply(dt$MP, foo)], 
                         dt$Left.MCA.Vm.SD[sapply(dt$MP, foo)],
                         dt$BA.Vs.SD[sapply(dt$MP, foo)], 
                         dt$BA.Vd.SD[sapply(dt$MP, foo)], 
                         dt$BA.Vm.SD[sapply(dt$MP, foo)])

colnames(dt_baseline) = c('RMCA_VS_Baseline', 'RMCA_VD_Baseline', 'RMCA_VM_Baseline',
                          'LMCA_VS_Baseline', 'LMCA_VD_Baseline', 'LMCA_VM_Baseline',
                          'BA_VS_Baseline', 'BA_VD_Baseline', 'BA_VM_Baseline')

dt_final = cbind(dt, dt_baseline)



library(survival)

################################################################################
# Compare null to the baseline
mdnull = coxph(Surv(dt_final$Day1, dt_final$Day2, 
                    dt_final$Outcome_onlydeath) ~ Age + Gender,
                    data = dt_final)


mdbaseline = coxph(Surv(dt_final$Day1, dt_final$Day2, 
                        dt_final$Outcome_onlydeath) ~ Age + Gender + 
                        RMCA_VS_Baseline + I(RMCA_VS_Baseline^2) + 
                        RMCA_VD_Baseline + I(RMCA_VD_Baseline^2) + 
                        RMCA_VM_Baseline + I(RMCA_VM_Baseline^2),
                        data = dt_final)


################################################################################
# Compare null to incorporating TCD

mdnull = coxph(Surv(dt$Day1, dt$Day2, 
                    dt$Outcome_onlydeath) ~ Age + Gender,
               data = dt)


mdR = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
              Right.MCA.Vs.SD + I(Right.MCA.Vs.SD^2) + 
              Right.MCA.Vd.SD + I(Right.MCA.Vd.SD^2) + 
              Right.MCA.Vm.SD + I(Right.MCA.Vm.SD^2),
            data = dt)

mdLR = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
              Right.MCA.Vs.SD + I(Right.MCA.Vs.SD^2) + 
              Right.MCA.Vd.SD + I(Right.MCA.Vd.SD^2) + 
              Right.MCA.Vm.SD + I(Right.MCA.Vm.SD^2) + 
              Left.MCA.Vs.SD + I(Left.MCA.Vs.SD^2) +
              Left..MCA.Vd.SD + I(Left..MCA.Vd.SD^2) +
              Left.MCA.Vm.SD + I(Left.MCA.Vm.SD^2),
              data = dt)

mdL = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
              Left.MCA.Vs.SD + I(Left.MCA.Vs.SD^2) +
              Left..MCA.Vd.SD + I(Left..MCA.Vd.SD^2) +
              Left.MCA.Vm.SD + I(Left.MCA.Vm.SD^2),
            data = dt)


mdB = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
              BA.Vs.SD + I(BA.Vs.SD^2) + 
              BA.Vd.SD + I(BA.Vd.SD^2) +
              BA.Vm.SD + I(BA.Vm.SD^2),
            data = dt)


mdLB = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
              BA.Vs.SD + I(BA.Vs.SD^2) + 
              BA.Vd.SD + I(BA.Vd.SD^2) +
              BA.Vm.SD + I(BA.Vm.SD^2) +
               Left.MCA.Vs.SD + I(Left.MCA.Vs.SD^2) +
               Left..MCA.Vd.SD + I(Left..MCA.Vd.SD^2) +
               Left.MCA.Vm.SD + I(Left.MCA.Vm.SD^2),
            data = dt)


mdall = coxph(Surv(dt$Day1, dt$Day2, dt$Outcome_onlydeath) ~ Age + Gender + 
             Right.MCA.Vs.SD + I(Right.MCA.Vs.SD^2) + 
             Right.MCA.Vd.SD + I(Right.MCA.Vd.SD^2) + 
             Right.MCA.Vm.SD + I(Right.MCA.Vm.SD^2) + 
             Left.MCA.Vs.SD + I(Left.MCA.Vs.SD^2) +
             Left..MCA.Vd.SD + I(Left..MCA.Vd.SD^2) +
             Left.MCA.Vm.SD + I(Left.MCA.Vm.SD^2) +
             BA.Vs.SD + I(BA.Vs.SD^2) + 
             BA.Vd.SD + I(BA.Vd.SD^2) +
             BA.Vm.SD + I(BA.Vm.SD^2),
           data = dt)


# ANOVA: compare two nested models
anova(mdnull, mdR, mdLR, mdall)


