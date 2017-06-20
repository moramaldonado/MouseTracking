import math
import numpy as np

# SPEED FUNCTION (based on distance instead of x-coordenate)
def velocity_normalized_d(all_trials):
    for s in range(len(all_trials)):
        overlap = []
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                    all_trials[s][t]['mouse_log']) > 1:
                bad_points = []
                velocity = []
                temp = []
                temp.append(all_trials[s][t]['normalized_positions'][0][2])
                velocity.append(0)
                for p in range(1, len(all_trials[s][t]['normalized_positions'])):
                    if p > 0:
                        t1 = float(all_trials[s][t]['normalized_positions'][p - 1][2])
                        t2 = float(all_trials[s][t]['normalized_positions'][p][2])
                        resta = t2 - t1
                        if resta == 0:
                            bad_points.append(p)
                            print p

                        else:
                            x1 = float(all_trials[s][t]['normalized_positions'][p - 1][0])
                            x2 = float(all_trials[s][t]['normalized_positions'][p][0])
                            y1 = float(all_trials[s][t]['normalized_positions'][p - 1][1])
                            y2 = float(all_trials[s][t]['normalized_positions'][p][1])
                            v = math.sqrt(math.pow((x2 - x1), 2) + math.pow((y2 - y1), 2)) / resta
                            v = abs(v)*1000

                    temp.append((t2+t1)/2)
                    velocity.append(v)

                overlap.append(len(bad_points))
                all_trials[s][t]['velocity'] = velocity
                all_trials[s][t]['velocity.time'] = temp
            else:
                all_trials[s][t]['velocity'] = 'NA'
                all_trials[s][t]['velocity.time'] = 'NA'


    return all_trials



# FUNCTION: acceleration_normalized(all_trials, value)
#   DESCRIPTION: requires velocity_normalized first
#   OUTPUT:
def acceleration_normalized(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                acceleration = []
                acceleration_time=[]
                bad_points = []
                for p in range(len(all_trials[s][t]['velocity'])):
                    if p > 0:
                        t1 = float(all_trials[s][t]['velocity.time'][p - 1])
                        t2 = float(all_trials[s][t]['velocity.time'][p])
                        resta = t2 - t1
                        acc_time = (t2 + t1)/2
                        if resta == 0:
                            bad_points.append(p)
                            print p

                        else:
                            x1 = float(all_trials[s][t]['velocity'][p - 1])
                            x2 = float(all_trials[s][t]['velocity'][p])
                            a = (x2 - x1) / resta

                    else:
                        a = 0
                        acc_time = 0

                    acceleration.append(a)
                    acceleration_time.append(acc_time)


                all_trials[s][t]['acceleration'] = acceleration
                all_trials[s][t]['acceleration.time'] = acceleration_time
            else:
                all_trials[s][t]['acceleration'] = 'NA'
                all_trials[s][t]['acceleration.time'] = 'NA'

    return all_trials


# Function speed and acceleration throuh simple moving-average smoothing over a window of a set size.
def vel_acc_moving(all_trials, windsize):
    for s in range(len(all_trials)):  # subject
        for t in range(len(all_trials[s])):  # trial
            # Take the two vectors of times
            ts1 = [all_trials[s][t]['corresponding_time'][i] for i in range(0,len(all_trials[s][t]['corresponding_time'])-(windsize-1))]
            ts2 = [all_trials[s][t]['corresponding_time'][i] for i in range(5,len(all_trials[s][t]['corresponding_time']))]
            distance = []

            # Calculate the distances for all the points in vector DISTANCE
            for i in range(1,len(all_trials[s][t]['normalized_positions_x'])-1):
                diffy = all_trials[s][t]['normalized_positions_x'][i] - all_trials[s][t]['normalized_positions_x'][i-1]
                diffx = all_trials[s][t]['normalized_positions_y'][i] - all_trials[s][t]['normalized_positions_y'][i-1]
                dist = math.sqrt(math.pow(diffx, 2) + math.pow(diffy, 2))
                distance.append(dist)

            # In loop 2:WS-1, sum the distances such that I end up with vector distances of 0:95

            distance2 = []
            velocity_window = []
            velocity_window_time = []

            for i in range(0, len(all_trials[s][t]['normalized_positions_x']) - (windsize-1)):
                dist = distance[i]
                for w in range(2, windsize-1):
                    dist = dist + distance[i]
                difftime = ts2[i] - ts1[i]
                vel_time = (ts2[i] + ts1[i])/2
                vel = (dist/difftime)*1000
                velocity_window.append(vel)
                velocity_window_time.append(vel_time)

            #include this in all_trials
            all_trials[s][t]['velocity_window'] = velocity_window
            all_trials[s][t]['velocity_window.time'] = velocity_window_time

            #Acceleration
            acceleration = []
            acceleration_time = []
            for i in range(1, len(all_trials[s][t]['velocity_window'])-1):
                diffvel = all_trials[s][t]['velocity_window'][i] - all_trials[s][t]['velocity_window'][i-1]
                difftime = all_trials[s][t]['velocity_window.time'][i] - all_trials[s][t]['velocity_window.time'][i - 1]
                acc = diffvel/difftime
                acc_time = (all_trials[s][t]['velocity_window.time'][i] + all_trials[s][t]['velocity_window.time'][i - 1])/2
                acceleration.append(acc)
                acceleration_time.append(acc_time)

            # include this in all_trials
            all_trials[s][t]['acceleration_window'] = acceleration
            all_trials[s][t]['acceleration_window.time'] = acceleration_time

    return all_trials



# FUNCTION: velocity normalized(all_trials)
#   DESCRIPTION: measures velocity between 2 points and time, done over normalized positions (in time)
#   OUTPUT:

def velocity_normalized_x(all_trials):
    for s in range(len(all_trials)):
        overlap = []
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                    all_trials[s][t]['mouse_log']) > 1:
                bad_points = []
                velocity = []
                temp = []
                temp.append(all_trials[s][t]['normalized_positions'][0][2])
                velocity.append(0)
                for p in range(1, len(all_trials[s][t]['normalized_positions'])):
                    if p > 0:
                        t1 = float(all_trials[s][t]['normalized_positions'][p - 1][2])
                        t2 = float(all_trials[s][t]['normalized_positions'][p][2])
                        resta = t2 - t1
                        if resta == 0:
                            bad_points.append(p)
                            print p

                        else:
                            x1 = float(all_trials[s][t]['normalized_positions'][p - 1][0])
                            x2 = float(all_trials[s][t]['normalized_positions'][p][0])
                            v = (x2 - x1) / resta
                            v = abs(v)

                    temp.append(t2)
                    velocity.append(v)

                overlap.append(len(bad_points))
                all_trials[s][t]['velocity_normalized_x'] = velocity
                all_trials[s][t]['temp_velocity_x'] = temp
            else:
                all_trials[s][t]['velocity_normalized_x'] = 'NA'
                all_trials[s][t]['temp_velocity_x'] = 'NA'

    return all_trials

def acc_flips(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            x_flips = 0
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                for i in range(1, len(all_trials[s][t]['acceleration_window'])-1):
                    x1 = all_trials[s][t]['acceleration_window'][i] #x-1
                    x0 = all_trials[s][t]['acceleration_window'][i+1] #x
                    x2 = all_trials[s][t]['acceleration_window'][i-1] #x-2

                    flip = -1*(x0-x1)*(x1-x2)
                    if flip < 0:
                        flip = 0
                    elif flip > 0:
                        flip = 1

                    x_flips = x_flips + flip

                all_trials[s][t]['acc_flips'] = x_flips - 1
    return all_trials

def local_maxima_acc(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(
                    all_trials[s][t]['mouse_log']) > 1:

                m = max(all_trials[s][t]['acceleration'])
                temp = [i for i, j in enumerate(all_trials[s][t]['acceleration']) if j == m]
                index = temp[0]

                der_acc = []
                local_maxima = []
                der_acc.append(all_trials[s][t]['acceleration'][0])
                for i in range(1, len(all_trials[s][t]['acceleration'])):
                    a1 = all_trials[s][t]['acceleration'][i - 1]
                    a2 = all_trials[s][t]['acceleration'][i]
                    t1 = all_trials[s][t]['acceleration.time'][i - 1]
                    t2 = all_trials[s][t]['acceleration.time'][i]
                    d = float((a2 - a1) / (t2 - t1))
                    der_acc.append(d)
                for j in range(1, len(der_acc)):
                    if der_acc[j - 1] > 0 and der_acc[j] < 0 and all_trials[s][t]['acceleration'][j - 1] >= m / 2:
                        local_maxima.append([all_trials[s][t]['acceleration'][j - 1], j - 1])

                if local_maxima == []:
                    local_maxima.append([m, index])
                all_trials[s][t]['local_maxima'] = local_maxima
            else:
                all_trials[s][t]['local_maxima'] = 'NA'
    return all_trials




##FILTER
#Take absolute acceleration max value and define threshold as 70%: everything that is below .7 of the maxima it's 0.
def filter_acceleration(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                threshold = all_trials[s][t]['max_smooth_acceleration'][0] * .7
                acceleration = []
                for p in range(len(all_trials[s][t]['smooth_acceleration'])):
                    if abs(all_trials[s][t]['smooth_acceleration'][p]) >= threshold:
                        a = all_trials[s][t]['smooth_acceleration'][p]
                    else:
                        a = 0
                    acceleration.append(a)
                all_trials[s][t]['acceleration_filter'] = acceleration
            else:
                all_trials[s][t]['acceleration_filter'] = 'NA'
    return all_trials

