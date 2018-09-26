# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 18:01:41 2015

@author: moramaldonado
"""




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
            xflips = []
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
                    xflips.append(flip)

                flips = sum(xflips)
                all_trials[s][t]['x_flips'] = flips
    return all_trials



                #x values are in mouse_log[i][0]


def x_flips_test(all_trials, threshold):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            differences = []
            differencesnocero = []
            for x in range(0, 99):
                DIFF1 = all_trials[s][t]['normalized_positions_x'][x] - all_trials[s][t]['normalized_positions_x'][x+1]
                DIFF2 = all_trials[s][t]['normalized_positions_x'][x+1] - all_trials[s][t]['normalized_positions_x'][x+2]

                if  DIFF1 != 0:
                    DIFF1 = DIFF1/abs(DIFF1)
                if DIFF2 != 0:
                    DIFF2 = DIFF2 / abs(DIFF2)

                DIFFD = DIFF2 - DIFF1
                differences.append(DIFFD)
                differencesnocero.append(DIFFD)

            print differences

            differencesnocero = [x for x in differencesnocero if x != 0]
            print differencesnocero
            if threshold == 0:
                flips = sum(differencesnocero)

            else:
                if sum(differencesnocero) == 1:
                    flips = 1
                else:
                    diff_differences = []
                    for d in range(0, len(differencesnocero)-1):
                        diff = differencesnocero[d] - differencesnocero[d+1]
                        diff_differences.append(diff)
                    if sum(diff_differences) >= threshold:
                        flips = sum(diff_differences)
                    else:
                        flips = 1

            print flips






def acc_flips_test(all_trials, threshold):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            differences = []
            differencesnocero = []
            for x in range(0, len(all_trials[s][t]['acceleration_window'])-2):
                DIFF1 = all_trials[s][t]['acceleration_window'][x] - all_trials[s][t]['acceleration_window'][x+1]
                DIFF2 = all_trials[s][t]['acceleration_window'][x+1] - all_trials[s][t]['acceleration_window'][x+2]

                if  DIFF1 != 0:
                    DIFF1 = DIFF1/abs(DIFF1)
                if DIFF2 != 0:
                    DIFF2 = DIFF2 / abs(DIFF2)

                DIFFD = DIFF1 - DIFF2
                differences.append(DIFFD)
                differencesnocero.append(DIFFD)

            differencesnocero = [x for x in differencesnocero if x != 0]

            if threshold == 0:
                flips = sum(differencesnocero)

            else:
                if sum(differencesnocero) == 1:
                    flips = 1
                else:
                    diff_differences = []
                    for d in range(0, len(differencesnocero)-1):
                        diff = differencesnocero[d] - differencesnocero[d+1]
                        diff_differences.append(diff)
                    if sum(diff_differences) >= threshold:
                        flips = sum(diff_differences)
                    else:
                        flips = 1

            print flips


