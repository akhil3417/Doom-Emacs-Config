;;; startup/misc-shell-commands.el -*- lexical-binding: t; -*-

;; Run our GPU at max power.
(call-process-shell-command "nvidia-settings -a '[gpu:0]/gpupowermizermode=1'" nil 0)

;; Run our CPU at max power.
(call-process-shell-command "echo 'performance' | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor")

;; Allow commands running under Sudo to access the diplay.
(call-process-shell-command "xhost +" nil 0)
