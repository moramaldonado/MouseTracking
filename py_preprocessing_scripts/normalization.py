# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 11:47:48 2015

@author: moramaldonado
"""
import math

# FUNCTION: normalization_in_space(trials)
#   DESCRIPTION:
#   INPUT:
#   OUTPUT: trials[i]['normalized_position']




def normalisation_in_time(all_trials):
    points = 101
    for s in range(len(all_trials)):  # subject
        for t in range(len(all_trials[s])):  # trial
            if all_trials[s][t]['value'] != '--' and len(all_trials[s][t]['mouse_log']) > 1:
                normalized_points = []
                normalized_points_x = []
                normalized_points_y = []
                corresponding_time =[]
                rt = all_trials[s][t]['mouse_log'][-1][3] - all_trials[s][t]['mouse_log'][0][3]
                delta_t = float(rt) / (points - 1)
                time_offset = all_trials[s][t]['mouse_log'][0][3]
                t0 = time_offset
                x0 = all_trials[s][t]['mouse_log'][0][0]
                y0 = all_trials[s][t]['mouse_log'][0][1]
                normalized_points.append([x0, y0, t0])
                normalized_points_x.append(x0)
                normalized_points_y.append(y0)
                corresponding_time.append(t0)

                for i in range(1, (points - 1)):
                    temp = time_offset + (i * delta_t)
                    j = 0
                    while (temp) >= all_trials[s][t]['mouse_log'][j][3]:
                        j = j + 1
                    if (temp) != all_trials[s][t]['mouse_log'][j][3]:
                        var_time = float((temp - all_trials[s][t]['mouse_log'][j - 1][3]) / (
                        all_trials[s][t]['mouse_log'][j][3] - all_trials[s][t]['mouse_log'][j - 1][3]))
                        x = float(all_trials[s][t]['mouse_log'][j - 1][0]) + float(
                            var_time * (all_trials[s][t]['mouse_log'][j][0] - all_trials[s][t]['mouse_log'][j - 1][0]))
                        y = float(all_trials[s][t]['mouse_log'][j - 1][1]) + float(
                            var_time * (all_trials[s][t]['mouse_log'][j][1] - all_trials[s][t]['mouse_log'][j - 1][1]))
                    else:
                        x = all_trials[s][t]['mouse_log'][j][0]
                        y = all_trials[s][t]['mouse_log'][j][1]
                    normalized_points.append([x, y, temp])
                    normalized_points_x.append(x)
                    normalized_points_y.append(y)
                    corresponding_time.append(temp)

                tn = rt + time_offset
                xn = all_trials[s][t]['mouse_log'][-1][0]
                yn = all_trials[s][t]['mouse_log'][-1][1]

                normalized_points.append([xn, yn, tn])
                normalized_points_x.append(xn)
                normalized_points_y.append(yn)
                corresponding_time.append(tn)
                all_trials[s][t]['normalized_positions'] = normalized_points
                all_trials[s][t]['normalized_positions_x'] = normalized_points_x
                all_trials[s][t]['normalized_positions_y'] = normalized_points_y
                all_trials[s][t]['corresponding_time'] = corresponding_time
            else:
                # print 'here I do not have information', s,t, all_trials[s][t]['value'], all_trials[s][t]['accuracy']
                all_trials[s][t]['normalized_positions'] = 'NA'
                all_trials[s][t]['normalized_positions_x'] = 'NA'
                all_trials[s][t]['normalized_positions_y'] = 'NA'
                all_trials[s][t]['corresponding_time'] = 'NA'



    return all_trials


# FUNCTION: total_lenght(trials)
#   DESCRIPTION: Calculate mouse path curve total length
#   INPUT: trials -> Mouse path points
#   OUTPUT: trials[i]['totalLength'] -> Mouse path curve length
#           trials[i]['mouse_log'][j][4] -> Mouse path point [j] partial length 
def total_length(trials):
    for s in range(len(trials)): #for each subject
        for i in range(len(trials[s])): #for each trial
            curveLength = 0
            sectionLength = 0
            if trials[s][i]['mouse_log'] != [] and len(trials[s][i]['mouse_log']) > 1:
                trials[s][i]['mouse_log'][0].append(0) #first position has a length of 0 (I am considering this one the first position)
                for j in range(len(trials[s][i]['mouse_log']) - 1): # for each position except the first one that is zero
                    px1 = trials[s][i]['mouse_log'][j][0]
                    py1 = trials[s][i]['mouse_log'][j][1]
                    px2 = trials[s][i]['mouse_log'][j+1][0]
                    py2 = trials[s][i]['mouse_log'][j+1][1] 
                    sectionLength = math.sqrt( math.fabs(px2-px1) + math.fabs(py2-py1) )
                    curveLength = curveLength + sectionLength
                    trials[s][i]['mouse_log'][j+1].append(curveLength)
            trials[s][i]['totalLength'] = curveLength
    return trials

# FUNCTION: normalization_in_space(trials, totalPoints)
#   DESCRIPTION: total_length(trials) has to be called before
#   INPUT: trials[i]['positions'] -> RAW time-dependent mouse path points (unequally spaced)
#          totalPoints -> number of interpolated points
#   OUTPUT: trials[i]['normalized_position_space'] -> Time normalized mouse path points (equally spaced)

def normalization_in_space(trials, totalPoints):
    for s in range(len(trials)):
        for i in range(len(trials[s])):
            if trials[s][i]['value'] != '--' and trials[s][i]['mouse_log'] != [] and len(trials[s][i]['mouse_log']) > 1:
            # set first interpolated point = original first
                px = trials[s][i]['mouse_log'][0][0] 
                py = trials[s][i]['mouse_log'][0][1]
                interpolatedPoints = []
                interpolatedPoints.append([px,py]) 
                # set next (totalPoint-2) interpolated points
                curveLength = trials[s][i]['totalLength']
                sectionLength = curveLength / (totalPoints-1)
                k = 1
                currentPointCurveLength = trials[s][i]['mouse_log'][k][4]
                for j in range(1, totalPoints-1):
                    curveLengthAtNextInterPoint = sectionLength * j
                    while (curveLengthAtNextInterPoint >= currentPointCurveLength):
                        k = k + 1
                        currentPointCurveLength = trials[s][i]['mouse_log'][k][4]
                    px1 = trials[s][i]['mouse_log'][k-1][0]
                    py1 = trials[s][i]['mouse_log'][k-1][1]
                    px2 = trials[s][i]['mouse_log'][k][0]
                    py2 = trials[s][i]['mouse_log'][k][1]
                    previousPointCurveLength = trials[s][i]['mouse_log'][k-1][4]
                    d1 = curveLengthAtNextInterPoint - previousPointCurveLength
                    d = currentPointCurveLength - previousPointCurveLength
                    interPx = px1 + (d1 / d) * (px2 - px1)
                    interPy = py1 + (d1 / d) * (py2 - py1)
                    interpolatedPoints.append([interPx, interPy])
                    
                # set last interpolated point = original last
                lastPointIndex = len(trials[s][i]['mouse_log'])-1
                px = trials[s][i]['mouse_log'][lastPointIndex][0]
                py = trials[s][i]['mouse_log'][lastPointIndex][1]
                interpolatedPoints.append([px,py])
                # add all interpolated points to trilas
                trials[s][i]['normalized_positions_space'] = interpolatedPoints
            else:
                trials[s][i]['normalized_positions_space'] = 'NA'
                
    return trials