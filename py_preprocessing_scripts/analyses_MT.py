# -*- coding: utf-8 -*-
"""
Created on Sat Apr 18 10:23:28 2015

@author: moramaldonado

"""




import math
import numpy as np

# FUNCTION: euclidean_distance(all_trials, value)
#   DESCRIPTION: Calc eucl.distance from the position to each of the response buttons ('value'==response)
#   OUTPUT: list of distances: all_trials[euclidean_distance_response1],all_trials[euclidean_distance_response2]
def euclidean_distance(all_trials, value):
    name = 'euclidean_distance_' + value
    if value == 'true':
        x2 = 1
        y2 = 1
    else:
        x2 = -1
        y2 = 1
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            euclidean_distance = []
            if all_trials[s][t]['normalized_positions'] != 'NA' and len(all_trials[s][t]['mouse_log']) > 1:
                for i in range(len(all_trials[s][t]['normalized_positions'])):
                    x1 = float(all_trials[s][t]['normalized_positions'][i][0])
                    y1 = float(all_trials[s][t]['normalized_positions'][i][1])
                    if value == 'true' or value=='B':
                        if x1 > x2 and y1 < y2:
                            x1 = x2
                        if x1 < x2 and y1 > y2:
                            y1 = y2
                        if x1 > x2 and y1 > y2:
                            y1 = y2
                            x1 = x2
                    else:
                        if x1 < x2 and y1 < y2:
                            x1 = x2
                        if x1 > x2 and y1 > y2:
                            y1 = y2
                        if x1 < x2 and y1 > y2:
                            y1 = y2
                            x1 = x2
                    ed = lineMagnitude(x1, y1, x2, y2)
                    euclidean_distance.append(ed)

                # mx = max(euclidean_distance)
                #                if mx == 0:
                #                    #print s,t
                #                    all_trials[s][t][name] = euclidean_distance
                #                else:
                ##
                #                for j in range(len(euclidean_distance)):
                #                    euclidean_distance[j] = float(euclidean_distance[j]/1.414)

                all_trials[s][t][name] = euclidean_distance
            else:
                all_trials[s][t][name] = []
    return all_trials

# FUNCTION: difference(all_trials)
#   DESCRIPTION: Calc difference btw eucl.distance to target and to alternative for each normalized position
#   OUTPUT: list of ratios - all_trials['difference']
def difference(all_trials):  # distance to expected response - distance to alternative
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            difference = []
            if all_trials[s][t]['value'] != '--':
                if all_trials[s][t]['value'] == 'true':
                    target = 'euclidean_distance_true'
                    alternative = 'euclidean_distance_false'
                else:
                    target = 'euclidean_distance_false'
                    alternative = 'euclidean_distance_true'

                for i in range(len(all_trials[s][t][target])):
                    a = all_trials[s][t][target][i]
                    b = all_trials[s][t][alternative][i]
                    d = float(a - b)
                    difference.append(d)

                all_trials[s][t]['difference'] = difference
            else:
                all_trials[s][t]['difference'] = []

    return all_trials


# FUNCTION: ratio(all_trials)
#   DESCRIPTION: Calc ratio btw eucl.distance to target and to alternative for each normalized position
#   OUTPUT: list of ratios - all_trials['ratio']
def ratio(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            ratio =[]
            if all_trials[s][t]['value'] != '--':
                if all_trials[s][t]['value'] == 'true':
                    target = 'euclidean_distance_true'
                    alternative = 'euclidean_distance_false'


                elif all_trials[s][t]['value'] == 'false':
                    target = 'euclidean_distance_false'
                    alternative = 'euclidean_distance_true'

                for i in range(len(all_trials[s][t][target])):
                    a = all_trials[s][t][target][i]
                    if a < 0.001:
                        a = 0.001
                    b = all_trials[s][t][alternative][i]
                    if b < 0.001:
                        b = 0.001
                    r = float(a/b)
                    ratio.append(r)
            all_trials[s][t]['ratio'] = ratio
    return all_trials

#log-transform ratio values for each position
def log_ratio(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            ratio_log =[]
            for i in range(len(all_trials[s][t]['ratio'])):
                r = math.log(all_trials[s][t]['ratio'][i])
                ratio_log.append(r)
            all_trials[s][t]['ratio_log'] = ratio_log
    return all_trials


def lineMagnitude(x1, y1, x2, y2):
    lineMagnitude = math.sqrt(math.pow((x2 - x1), 2) + math.pow((y2 - y1), 2))
    return lineMagnitude


def median_value(all_trials, value):
    name = 'median_' + value
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                m = np.median(all_trials[s][t][value])
                # temp = [i for i, j in enumerate(all_trials[s][t][value]) if j == m]
                # index = temp[-1]
                # if len(temp) > 1:
                #     more.append([s, t])
                #
                # temp = all_trials[s][t]['temp_velocity'][index]
                #
                # all_trials[s][t][name] = [m, temp, index]
                all_trials[s][t][name] = m
            else:
                all_trials[s][t][name] = ['NA', 'NA', 'NA']

    return all_trials


def maximum_value(all_trials, value):
    name = 'max_' + value
    more = []
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                    all_trials[s][t]['mouse_log']) > 1:

                if value == 'acceleration' and all_trials[s][t]['local_maxima'] != []:
                    m = all_trials[s][t]['local_maxima'][-1][0]
                else:
                    m = max(all_trials[s][t][value])
                temp = [i for i, j in enumerate(all_trials[s][t][value]) if j == m]
                index = temp[-1]
                if len(temp) > 1:
                    more.append([s, t])

                temp = all_trials[s][t]['velocity.time'][index]

                all_trials[s][t][name] = [m, temp, index]
            else:
                all_trials[s][t][name] = ['NA', 'NA', 'NA']

    return all_trials, more


def find_value_in(all_trials, value, where):
    name = value + '_in_' + where
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t][where][2] != 'NA':
                index = all_trials[s][t][where][2]
                all_trials[s][t][name] = all_trials[s][t][value][index]
            else:
                all_trials[s][t][name] = 'NA'

    return all_trials


# FUNCTION: DistancePointLine (px, py, x1, y1, x2, y2)
#   DESCRIPTION: Calc minimum distance from a point and a line segment (i.e. consecutive vertices in a polyline).
#   REQUIREMENTS:
#   INPUT: point (px, py) and segment line limits (x1, y1, x2, y2)
#   OUTPUT: trials -> calculated distance

def DistancePointLine(px, py, x1, y1, x2, y2):
    # http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/source.vba
    LineMag = lineMagnitude(x1, y1, x2, y2)
    intersection = 'False'
    if LineMag < 0.000000000000000000001:
        DistancePointLine = 9999
        return DistancePointLine

    u1 = (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
    u = u1 / (LineMag * LineMag)

    if (u < 0.00000000000000001) or (u > 1):
        # // closest point does not fall within the line segment, take the shorter distance
        # // to an endpoint
        ix = lineMagnitude(px, py, x1, y1)
        iy = lineMagnitude(px, py, x2, y2)
        if ix > iy:
            DistancePointLine = iy
        else:
            DistancePointLine = ix
    else:
        # Intersecting point is on the line, use the formula
        intersection = 'True'
        ix = x1 + u * (x2 - x1)
        iy = y1 + u * (y2 - y1)
        DistancePointLine = lineMagnitude(px, py, ix, iy)

    return DistancePointLine, ix, iy, intersection


# FUNCTION: MousePathPositionsDistances(trials)
#   DESCRIPTION: calculates distances from mouse path points to ideal path
#   REQUIREMENTS:
#   INPUT: trials -> mouse path points
#   OUTPUT: trials -> calculated distances

def distance_ideal_trajectory(trials):
    # Extender el segmento de recta del camino ideal hasta el borde de la pantalla para evitar un calculo erroneo de la distancia entre los puntos y el segmento cuando el punto cae fuera del segmento.
    for s in range(len(trials)):
        for t in range(len(trials[s])):

            if trials[s][t]['value'] != '--' and len(trials[s][t]['mouse_log']) > 1:
                # x1 = trials[i][3][0][2]
                # y1 = trials[i][3][0][3]
                x1 = 0
                y1 = 0
                if trials[s][t]['value'] == 'true' or trials[s][t]['value'] == 'B':
                    x2 = 1.1
                    y2 = 1.1
                elif trials[s][t]['value'] == 'false' or trials[s][t]['value'] == 'A':
                    x2 = -1.1
                    y2 = 1.1

                for j in range(len(trials[s][t]['normalized_positions'])):
                    px = trials[s][t]['normalized_positions'][j][0]
                    py = trials[s][t]['normalized_positions'][j][1]
                    dist, ix, iy, intersection = DistancePointLine(px, py, x1, y1, x2, y2)
                    trials[s][t]['normalized_positions'][j].append(dist)
                    trials[s][t]['normalized_positions'][j].append([ix, iy])

    return trials


def maximum_deviation(trials):
    for s in range(len(trials)):
        for t in range(len(trials[s])):
            if trials[s][t]['value'] != '--' and trials[s][t]['normalized_positions'] != 'NA':
                # area = 0
                maxDeviation = 0
                maxDeviation_border = 0

                jMaxDeviation = 0  # nuevo
                jMaxDeviation_border = 0

                for j in range(len(trials[s][t]['normalized_positions'])):
                    if trials[s][t]['value'] == 'true':
                        maxTemp = 1 - trials[s][t]['normalized_positions'][j][0]
                    else:
                        maxTemp = abs(-1 - trials[s][t]['normalized_positions'][j][0])

                    if maxTemp >= maxDeviation_border:
                        maxDeviation_border = maxTemp
                        jMaxDeviation_border = j
                        MD_border_time = trials[s][t]['normalized_positions'][j][2]  # nuevo

                    if trials[s][t]['normalized_positions'][j][3] >= maxDeviation:
                        maxDeviation = trials[s][t]['normalized_positions'][j][3]
                        jMaxDeviation = j  # nuevo
                        MD_time = trials[s][t]['normalized_positions'][j][2]  # nuevo

                # area = area / len(trials[i]['positions'])
                trials[s][t]['maxDeviation'] = [maxDeviation, MD_time, jMaxDeviation]
                trials[s][t]['maxDeviationBorder'] = [maxDeviation_border, MD_border_time, jMaxDeviation_border]
            else:
                trials[s][t]['maxDeviation'] = ['NA', 'NA', 'NA']
                trials[s][t]['maxDeviationBorder'] = ['NA', 'NA', 'NA']

    return trials


def smooth(all_trials, data_type):
    name = 'smooth_' + data_type
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t][data_type] != [] and all_trials[s][t][data_type] != 'NA':
                smooth = []
                for i in range(len(all_trials[s][t][data_type])):
                    if i == 0:
                        new = all_trials[s][t][data_type][i]
                    else:
                        new = float((all_trials[s][t][data_type][i] + all_trials[s][t][data_type][i - 1]) / 2)
                    smooth.append(new)
                all_trials[s][t][name] = smooth
            else:
                all_trials[s][t][name] = 'NA'

    return all_trials

def exclude_subjects(all_trials, exclude):
    all_trials_raw = all_trials
    for i in range(len(exclude)):
        ex = exclude[i]
        del all_trials[ex]
    return all_trials, all_trials_raw


def mean_acceleration(all_trials,block,expected_response,experiment):
    meanCurve = []
    for i in range(101):
        numTrials = 0
        meanCurve.append([0,0])
        for s in range(len(all_trials)):
            for t in range(len(all_trials[s])):
                if all_trials[s][t]['accuracy'] == 1 and all_trials[s][t]['expected_response'] == expected_response and len(all_trials[s][t]['mouse_log']) > 1 and all_trials[s][t]['experiment']==experiment and all_trials[s][t]['block'] == block:
                    meanCurve[i][0] = meanCurve[i][0] + all_trials[s][t]['acceleration'][i]
                    meanCurve[i][1] = meanCurve[i][1] + i
                    numTrials = numTrials + 1
        if numTrials == 0:
            meanCurve = []

        else:
            meanCurve[i][0] = float(meanCurve[i][0] / numTrials)
            meanCurve[i][1] = float(meanCurve[i][1] / numTrials)
    return meanCurve

def mean_trajectory_subject(all_trials,block,expected_response,experiment,timing):
    meanCurve = []
    s = subject
    for i in range(101):
        numTrials = 0
        meanCurve.append([0,0])
        for t in range(len(all_trials[subject])):
            if all_trials[s][t]['accuracy'] == 1 and all_trials[s][t]['expected_response'] == expected_response and len(all_trials[s][t]['mouse_log']) > 1 and \
                            all_trials[s][t]['experiment']==experiment and all_trials[s][t]['block'] == block and all_trials[s][t]['timing'] == timing :
                meanCurve[i][0] = meanCurve[i][0] + all_trials[s][t][data_type][i][0]
                meanCurve[i][1] = meanCurve[i][1] + all_trials[s][t][data_type][i][1]
                numTrials = numTrials + 1

        if numTrials == 0:
            meanCurve =[]
            
        else:
            meanCurve[i][0] = float(meanCurve[i][0] / numTrials)
            meanCurve[i][1] = float(meanCurve[i][1] / numTrials)

    return meanCurve


def mean_x(all_trials, block, info, data_type, expected_response, experiment):
    meanCurve = []
    for i in range(101):
        numTrials = 0
        meanCurve.append([0, 0])
        for s in range(len(all_trials)):
            if info[s]['experiment'] == experiment:
                for t in range(len(all_trials[s])):
                    # print s,t
                    if all_trials[s][t]['accuracy'] == 1 and all_trials[s][t][
                        'expected_response'] == expected_response and len(all_trials[s][t]['mouse_log']) > 1 and \
                                    all_trials[s][t]['experiment'] == experiment and all_trials[s][t][
                        'block'] == block:
                        meanCurve[i][0] = meanCurve[i][0] + all_trials[s][t][data_type][i][0]
                        meanCurve[i][1] = meanCurve[i][1] + i
                        numTrials = numTrials + 1

    meanCurve[i][0] = float(meanCurve[i][0] / numTrials)
    meanCurve[i][1] = float(meanCurve[i][1] / numTrials)

    return meanCurve


def mean_trajectory_calibration(all_trials, data_type, type):

    meanCurveL = []
    meanCurveR = []
    for i in range(101):
        numTrialsL = 0
        meanCurveL.append([0, 0])
        numTrialsR = 0
        meanCurveR.append([0, 0])
        for s in range(len(all_trials)):
            for t in range(len(all_trials[s])):
                if all_trials[s][t]['data']['item']['type'] == 'calibration' and all_trials[s][t]['polarity'] == type:
                    if all_trials[s][t]['expected_response'] =='false':
                        meanCurveL[i][0] = meanCurveL[i][0] + all_trials[s][t][data_type][i][0]
                        meanCurveL[i][1] = meanCurveL[i][1] + all_trials[s][t][data_type][i][1]
                        numTrialsL = numTrialsL + 1
                    elif all_trials[s][t]['expected_response'] == 'true':
                            meanCurveR[i][0] = meanCurveR[i][0] + all_trials[s][t][data_type][i][0]
                            meanCurveR[i][1] = meanCurveR[i][1] + all_trials[s][t][data_type][i][1]
                            numTrialsR = numTrialsR + 1

        meanCurveR[i][0] = float(meanCurveR[i][0] / numTrialsR)
        meanCurveR[i][1] = float(meanCurveR[i][1] / numTrialsR)

        meanCurveL[i][0] = float(meanCurveL[i][0] / numTrialsL)
        meanCurveL[i][1] = float(meanCurveL[i][1] / numTrialsL)

    meanCurveL = np.array(meanCurveL)
    meanCurveR = np.array(meanCurveR)
    return meanCurveL, meanCurveR



def mean_trajectory(all_trials,block,info, data_type, expected_response,experiment):
    meanCurve = []
    for i in range(101):
        numTrials = 0
        meanCurve.append([0,0])
        for s in range(len(all_trials)):
            if info[s]['experiment'] == experiment:
                for t in range(len(all_trials[s])):
                    #print s,t
                    if all_trials[s][t]['accuracy'] == 1 and all_trials[s][t][
                        'expected_response'] == expected_response and len(all_trials[s][t]['mouse_log']) > 1 and \
                                    all_trials[s][t]['experiment'] == experiment and all_trials[s][t][
                        'block'] == block:
                        meanCurve[i][0] = meanCurve[i][0] + all_trials[s][t][data_type][i][0]
                        meanCurve[i][1] = meanCurve[i][1] + all_trials[s][t][data_type][i][1]
                        numTrials = numTrials + 1

        meanCurve[i][0] = float(meanCurve[i][0] / numTrials)
        meanCurve[i][1] = float(meanCurve[i][1] / numTrials)
        
    return meanCurve

def find_point_change(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            myList = all_trials[s][t]['normalized_positions_y']
            myNumber = float(all_trials[s][t]['change_point'])
            if myNumber != 0:
                closest_y = min(myList, key=lambda x: abs(x - myNumber))
                time_point_change = myList.index(closest_y)
                all_trials[s][t]['PointChange.Y'] = closest_y
                all_trials[s][t]['PointChange.X'] = all_trials[s][t]['normalized_positions_x'][time_point_change]
                all_trials[s][t]['PointChange.Time'] = time_point_change
                all_trials[s][t]['PointChange.Time.Raw'] = all_trials[s][t]['corresponding_time'][time_point_change]

            else:
                all_trials[s][t]['PointChange.Y'] = 0
                all_trials[s][t]['PointChange.X'] = 0
                all_trials[s][t]['PointChange.Time'] = 0
                all_trials[s][t]['PointChange.Time.Raw'] = 0

    return all_trials




# def new_mean_trajectories(all_trials,info,condition,value,expected_response,experiment,quantifier):
#     mean_y_coor = []
#     mean_x_coor = []
#     sd_x_coor = []
#     se_x_coor=[]
#
#     for i in range(101):
#         x = []
#         y = []
#         for s in range(len(all_trials)):
#             if info[s]['experiment'] == experiment:
#                 for t in range(len(all_trials[s])):
#                     #print s,t
#                     if all_trials[s][t]['value'] == value and all_trials[s][t]['expected_response'] == expected_response and len(all_trials[s][t]['mouse_log']) > 1:
#                         if condition == 'controlp' or condition == 'controln':
#                             if all_trials[s][t]['type'] == condition:
#                                 x.append(all_trials[s][t]['normalized_positions'][i][0])
#                                 y.append(all_trials[s][t]['normalized_positions'][i][1])
#
#                         else:
#                             if quantifier != 'False':
#                                 if all_trials[s][t]['type'] == 'starget' and all_trials[s][t]['condition'] == condition and all_trials[s][t]['quantifier.combination']== quantifier:
#                                     x.append(all_trials[s][t]['normalized_positions'][i][0])
#                                     y.append(all_trials[s][t]['normalized_positions'][i][1])
#                             else:
#                                 if all_trials[s][t]['type'] == 'starget' and all_trials[s][t]['condition'] == condition:
#                                     x.append(all_trials[s][t]['normalized_positions'][i][0])
#                                     y.append(all_trials[s][t]['normalized_positions'][i][1])
#         mean_x = numpy.mean(x)
#         mean_y =numpy.mean(y)
#         sd_x = numpy.std(x)
#         se_x = stats.sem(x)
#
#         mean_x_coor.append(mean_x)
#         mean_y_coor.append(mean_y)
#         sd_x_coor.append(sd_x)
#         se_x_coor.append(se_x)
#
#
#     return mean_x_coor, sd_x_coor, se_x_coor, mean_y_coor




