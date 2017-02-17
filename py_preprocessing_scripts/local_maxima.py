# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 18:01:41 2015

@author: moramaldonado
"""

def local_maxima(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                
                m = max(all_trials[s][t]['smooth_acceleration'])
                temp = [i for i, j in enumerate(all_trials[s][t]['smooth_acceleration']) if j == m]
                index = temp[0]
               
                der_acc = []
                local_maxima = []
                der_acc.append(all_trials[s][t]['smooth_acceleration'][0])
                for i in range(1,len(all_trials[s][t]['smooth_acceleration'])):
                    a1 = all_trials[s][t]['smooth_acceleration'][i-1]
                    a2 = all_trials[s][t]['smooth_acceleration'][i]
                    t1 = all_trials[s][t]['temp_velocity'][i-1]
                    t2 = all_trials[s][t]['temp_velocity'][i]
                    d = float((a2-a1)/(t2-t1))
                    der_acc.append(d)
                for j in range(1,len(der_acc)):
                    if der_acc[j-1] > 0 and der_acc[j] < 0 and all_trials[s][t]['smooth_acceleration'][j-1] >= m/2:
                        local_maxima.append([all_trials[s][t]['smooth_acceleration'][j-1],j-1])
                    
                    
                if local_maxima == []:
                    local_maxima.append([m, index])
                all_trials[s][t]['local_maxima'] = local_maxima
            else:
                all_trials[s][t]['local_maxima'] = 'NA'
    return all_trials


def local_maxima_x(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                    all_trials[s][t]['mouse_log']) > 1:

                der_acc = []
                local_maxima = []

                der_acc.append(all_trials[s][t]['normalized_positions'][0][0])

                for i in range(1, len(all_trials[s][t]['normalized_positions'])):
                    a1 = abs(all_trials[s][t]['normalized_positions'][i - 1][0])
                    a2 = abs(all_trials[s][t]['normalized_positions'][i][0])
                    t1 = i-1
                    t2 = i
                    d = float((a2 - a1) / (t2 - t1))
                    der_acc.append(d)
                for j in range(1, len(der_acc)):
                    if der_acc[j - 1] > 0 and der_acc[j] < 0:
                        local_maxima.append([all_trials[s][t]['normalized_positions'][j - 1][0], j - 1])
                all_trials[s][t]['local_maxima_x'] = local_maxima
            else:
                all_trials[s][t]['local_maxima_x'] = 'NA'
    return all_trials

def x_flips(all_trials):

    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            x_flips = 0
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                all_trials[s][t]['mouse_log']) > 1:
                for i in range(1, 100):
                    x1 = abs(all_trials[s][t]['normalized_positions'][i][0]) #x-1
                    x0 = abs(all_trials[s][t]['normalized_positions'][i+1][0]) #x
                    x2 = abs(all_trials[s][t]['normalized_positions'][i-1][0]) #x-2
                    flip = -1*(x0-x1)*(x1-x2)
                    if flip < 0:
                        flip = 0
                    elif flip > 0:
                        flip = 1

                    x_flips = x_flips + flip

                all_trials[s][t]['x_flips'] = x_flips
    return all_trials

def x_flips2(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            pass_zero = 0
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                all_trials[s][t]['mouse_log']) > 1:
                for i in range(1, 100):
                    x1 = all_trials[s][t]['normalized_positions'][i][0] #x-1
                    x0 = all_trials[s][t]['normalized_positions'][i+1][0] #x
                    if (x0 < 0 and x1 > 0) or (x0 > 0 and x1 < 0):
                        pass_zero = pass_zero + 1


                all_trials[s][t]['x_flips2'] = pass_zero




    return all_trials


                #x values are in mouse_log[i][0]

