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
from normalization import *
from integrate import *
from local_maxima import *
from plot_mouse_paths import *
from scipy import stats


path = "/Users/moramaldonado/WebstormProjects/negationMT"

pathData = path + '/raw_data'
print path

#import the data
all_trials,names = joining_data(pathData)
print ">>>> data charged"
print len(all_trials)
points_per_trial = points_per_trial(all_trials) #Mean mouse-tracking points per subject

info, total_time = information(all_trials,names,points_per_trial) #subject information + total time to perform the task
print ">>>> information per subject taken"

print info
all_trials = organization_trials(all_trials)
print ">>>> trial information organized"

#minutes taken to complete the task (in minutes)
print ">>>> total_time per subject:"+ str(total_time)

all_trials = convert_time(all_trials) #time in milliseconds
print ">>>> raw time added to mouse_log"


all_trials = delay(all_trials) #delay to start moving the mouse after clicking start


all_trials = normalisation_in_time(all_trials)
print ">>>> normalization in time"

all_trials = velocity_normalized(all_trials)
print ">>>> velocity normalized taken"

all_trials = acceleration_normalized(all_trials)
print ">>>> acceleration done"

##False == A, True==B
all_trials = euclidean_distance(all_trials,'false')
all_trials = euclidean_distance(all_trials,'true')
print ">>>> euclidean distance done"

all_trials = total_length(all_trials)
print ">>>> mouse path length taken"

all_trials = normalization_in_space(all_trials,101)
print ">>>> normalization in space"

all_trials = smooth(all_trials,'acceleration')
all_trials = local_maxima(all_trials)
print '>>>>>>> smoothing acceleration and local maxima taken'

all_trials = ratio(all_trials)
all_trials = log_ratio(all_trials)
print '>>>> ratio distance_target/ distance_alternative for each point'

all_trials = difference(all_trials)
print '>>>> difference distance target- distance alternative for each point'


all_trials,more_V = maximum_value(all_trials, 'velocity_normalized')
all_trials,more_R = maximum_value(all_trials, 'ratio')
all_trials,more_RL = maximum_value(all_trials, 'ratio_log')
all_trials,more_D = maximum_value(all_trials, 'difference')
all_trials,more_SA = maximum_value(all_trials, 'smooth_acceleration')  #max_smooth_acceleration
all_trials = median_value(all_trials,'ratio_log')
all_trials = median_value(all_trials,'difference')

print ">>>> maximum values: ratio, difference, velocity, acceleration"

print all_trials[1][10]['median_ratio_log']

all_trials = find_value_in(all_trials, 'ratio_log','max_smooth_acceleration')

all_trials = distance_ideal_trajectory(all_trials)
all_trials = maximum_deviation(all_trials)
print ">>>> maximum derivation taken"

all_trials = find_value_in(all_trials, 'ratio_log','maxDeviation')

all_trials = integrate('ratio','max_smooth_acceleration',all_trials)
all_trials = integrate('ratio_log','max_smooth_acceleration',all_trials)
all_trials = integrate('ratio_log','fin',all_trials)
all_trials = integrate('difference','max_smooth_acceleration',all_trials)
all_trials = integrate('difference','maxDeviation',all_trials)
all_trials = integrate_x('max_smooth_acceleration',all_trials)
all_trials = integrate_x('fin',all_trials)


#exporting the data for R
pathR = path+'/data_R'
exporting_data(pathR,all_trials, info)
print ">>>> data exported in csv for R"

#plotting
os.chdir(path+'/figures')
plot_calibration(all_trials,info,True,'normalized_positions_space')
#plot_per_subject(0,all_trials,info,'true','normalized_positions_space','red')
#plot_per_subject(0,all_trials,info,'false','normalized_positions_space','green')

#plot a couple of subject

startX, startY = 0, 0
x = info[0]['normalized_button_size']['x']
y = info[0]['normalized_button_size']['y']
currentAxis = plt.gca()
# currentAxis.add_patch(Ellipsis((startX -(x/2), startY - y), x, y, ec='black', fill=False))
start(currentAxis)

# plt.axis([-1-x-.25, 1+x+.25, 0-y , 1+y+.1])
plt.ylabel('y coordenate')
plt.xlabel('x coordenate')
title = 'Uncertain class as deviated'
plt.title(title)
px = []
py = []
for i in range(len(all_trials[1][4]['normalized_positions_space'])):
    px.append(all_trials[1][4]['normalized_positions_space'][i][0])
    py.append(all_trials[1][4]['normalized_positions_space'][i][1])
    plt.plot(px, py, '-', c='blue', alpha=0.7)
plt.savefig('S1-bad.pdf', format='pdf', bbox_inches='tight')
plt.close()

startX, startY = 0, 0
x = info[0]['normalized_button_size']['x']
y = info[0]['normalized_button_size']['y']
currentAxis = plt.gca()
# currentAxis.add_patch(Ellipsis((startX -(x/2), startY - y), x, y, ec='black', fill=False))
start(currentAxis)

plt.ylabel('y coordenate')
plt.xlabel('x coordenate')
title = 'Deviated class as uncertain'
plt.title(title)
px = []
py = []
for i in range(len(all_trials[3][2]['normalized_positions_space'])):
    px.append(all_trials[3][2]['normalized_positions_space'][i][0])
    py.append(all_trials[3][2]['normalized_positions_space'][i][1])
    plt.plot(px, py, '-', c='red', alpha=0.7)
plt.savefig('S3-bad.pdf', format='pdf', bbox_inches='tight')
plt.close()


startX, startY = 0, 0
x = info[0]['normalized_button_size']['x']
y = info[0]['normalized_button_size']['y']
currentAxis = plt.gca()
# currentAxis.add_patch(Ellipsis((startX -(x/2), startY - y), x, y, ec='black', fill=False))
start(currentAxis)

plt.ylabel('y coordenate')
plt.xlabel('x coordenate')
title = 'uncertain class as deviated'
plt.title(title)
px = []
py = []
for i in range(len(all_trials[0][5]['normalized_positions_space'])):
    px.append(all_trials[0][5]['normalized_positions_space'][i][0])
    py.append(all_trials[0][5]['normalized_positions_space'][i][1])
    plt.plot(px, py, '-', c='blue', alpha=0.7)
plt.savefig('S0-bad.pdf', format='pdf', bbox_inches='tight')
plt.close()
