# -*- coding: utf-8 -*-
"""
@author: moramaldonado
"""
import os
import matplotlib.pyplot as plt
import time
import math
import numpy as npy
import pygame
from organization_functions import *
from analyses_MT import *
from speed_acceleration import *
from normalization import *
from integrate import *
from local_maxima import *
from plot_mouse_paths import *
from scipy import stats


path = "/Users/moramaldonado/WebstormProjects/negationMT"

pathData = path + '/0617_raw_data_calibration'
print path


#import the data
all_trials,names = joining_data(pathData)
print ">>>> data charged"
print len(all_trials)
points_per_trial = points_per_trial(all_trials) #Mean mouse-tracking points per subject

info, total_time = information(all_trials,names,points_per_trial) #subject information + total time to perform the task
print ">>>> information per subject taken"

all_trials = organization_trials(all_trials)
print ">>>> trial information organized"

#minutes taken to complete the task (in minutes)
print ">>>> total_time per subject:"+ str(total_time)

all_trials = convert_time(all_trials) #time in milliseconds
print ">>>> raw time added to mouse_log"


all_trials = delay(all_trials) #delay to start moving the mouse after clicking start


all_trials = normalisation_in_time(all_trials)
print ">>>> normalization in time"

all_trials = velocity_normalized_d(all_trials)
print ">>>> velocity normalized taken"

all_trials = acceleration_normalized(all_trials)
print ">>>> acceleration done"

all_trials = vel_acc_moving(all_trials,6)
print ">>>> acceleration smooth with D and D"

##False == A, True==B
all_trials = euclidean_distance(all_trials,'false')
all_trials = euclidean_distance(all_trials,'true')
print ">>>> euclidean distance done"

all_trials = total_length(all_trials)
print ">>>> mouse path length taken"

all_trials = normalization_in_space(all_trials,101)
print ">>>> normalization in space"

all_trials = smooth(all_trials,'acceleration')
all_trials = local_maxima_acc(all_trials)
print '>>>>>>> smoothing acceleration and local maxima taken'

all_trials = ratio(all_trials)
all_trials = log_ratio(all_trials)
print '>>>> ratio distance_target/ distance_alternative for each point'

all_trials = difference(all_trials)
print '>>>> difference distance target- distance alternative for each point'

all_trials,more_R = maximum_value(all_trials, 'ratio')
all_trials,more_RL = maximum_value(all_trials, 'ratio_log')
all_trials,more_SA = maximum_value(all_trials, 'acceleration')  #max_smooth_acceleration
all_trials = median_value(all_trials,'ratio_log')
all_trials = median_value(all_trials,'difference')
print ">>>> maximum values: ratio, difference, velocity, acceleration"


all_trials = distance_ideal_trajectory(all_trials)
all_trials = maximum_deviation(all_trials)
print ">>>> maximum derivation taken"


all_trials = integrate('ratio_log','fin',all_trials)
all_trials = integrate_x('fin',all_trials)
all_trials = local_maxima_x(all_trials)
all_trials = x_flips(all_trials)
all_trials = find_point_change(all_trials)
#all_trials = filter_acceleration(all_trials)
#all_trials = smooth(all_trials, 'acceleration_filter')
all_trials = acc_flips(all_trials)


# Exporting the data for R
pathR = path+'/data_R/calibration_pilot'
exporting_data(pathR,all_trials, info)
print ">>>> data exported in csv for R"
