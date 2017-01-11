# # -*- coding: utf-8 -*-
# """
# Created on Sat Apr 18 00:17:22 2015
#
# @author: moramaldonado
# """
# from analyses_MT import *
# import matplotlib.pyplot as plt
# from matplotlib.patches import Rectangle
# from mpl_toolkits.axes_grid1 import host_subplot
# import mpl_toolkits.axisartist as AA
# import os
#
#
#
#
# def plot_acc(a,b):
#
#     px=[]
#     py=[]
#     for i in range(len(a)):
#         px.append(a[i][1])
#         py.append(a[i][0])
#     plt.plot(px,py,'b-')
#
#     px=[]
#     py=[]
#     for i in range(len(b)):
#         px.append(b[i][1])
#         py.append(b[i][0])
#     plt.plot(px,py, 'r-')
#
#
# def plot_ratio(all_trials, subject, trial):
#
#     plt.plot(all_trials[subject][trial]['temp_velocity'],all_trials[subject][trial]['ratio'],'r.-')
#     plt.plot(all_trials[subject][trial]['max_ratio'][1],all_trials[subject][trial]['max_ratio'][0],'ko',label='maxRatio (a/b)')
#     plt.legend(loc='best')
#     plt.ylim(-1,110)
#
# def plot_difference(all_trials, subject, trial):
#
#     plt.plot(all_trials[subject][trial]['temp_velocity'],all_trials[subject][trial]['difference'],'m.-')
#     plt.plot(all_trials[subject][trial]['max_difference'][1],all_trials[subject][trial]['max_difference'][0],'ko',label='maxDifference(a-b)')
#     plt.legend(loc='best')
#     plt.ylim(-2,2)
#
#
# def simple_plot(data,color,name):
#     plt.axis([-1.5, 1.5, -0.3, 1.5])
#     #plt.title(title)
#     plt.ylabel('y coordenate')
#     plt.xlabel('x coordenate')
#     px  = []
#     py = []
#     for i in range(len(data)):
#         px.append(data[i][0])
#         py.append(data[i][1])
#
#     plt.plot(px,py,color,label=name)
#     plt.legend(loc='lower left',
#           nrows=2, fancybox=True, shadow=True, fontsize=9)
#     plt.show()
#
#
# def simple_plot_2(data,color,name,label):
#     #plt.axis([-2, 2, 0, 1.5])
#     #plt.title(title)
#     plt.ylabel(label)
#     plt.xlabel('time step')
#     px  = []
#     py = []
#     for i in range(len(data)):
#         px.append(data[i][1])
#         py.append(data[i][0])
#
#     plt.ylim(-2.5,1)
#     plt.plot(px,py,color,label=name)
#     plt.legend(loc='lower left',
#           ncol=2, fancybox=True, shadow=True, fontsize=9)
#
#     plt.show()
#
#
#
#
#
# def multiple_axes(all_trials, subject, trial):
#     title = 'Example_Subject:'+str(subject)+', trial:'+str(trial)
#     if 1:
#
#         host = host_subplot(111, axes_class=AA.Axes)
#         plt.subplots_adjust(right=0.75)
#         plt.title('Velocity vs. Trajectory')
#         par1 = host.twinx()
#         par2 = host.twinx()
#         offset = 60
#         new_fixed_axis = par2.get_grid_helper().new_fixed_axis
#         par2.axis["right"] = new_fixed_axis(loc="right",
#                                         axes=par2,
#                                         offset=(offset, 0))
#
#         par2.axis["right"].toggle(all=True)
#
#         host.set_xlabel("Raw Time")
#         host.set_ylabel("X-Coordenate")
#         par1.set_ylabel("X-Coordenate Velocity")
#         par2.set_ylabel("X-Coordenate Acceleration")
# #        data = all_trials[subject][trial]['mouse_log']
# #
# #        for i in range(len(data)):
# #        #plt.axis([-1.25, 1.25, 0, 1.5])
# #            p1,= host.plot(data[i][3], data[i][0],'r.')
#         data = all_trials[subject][trial]['normalized_positions']
#         for i in range(len(data)):
#         #plt.axis([-1.25, 1.25, 0, 1.5])
#             p1, = host.plot(data[i][2], data[i][0],'ko-')
#
#         dataY = all_trials[subject][trial]['velocity_normalized']
#         dataX = all_trials[subject][trial]['temp_velocity']
#         dataA = all_trials[subject][trial]['smooth_acceleration']
#
#         p2, = par1.plot(dataX,dataY, 'g.-')
#         p3, = par2.plot(dataX,dataA, 'm.-')
#
#         host.legend()
#         host.set_ylim(-2, 2)
#
#         host.axis["left"].label.set_color(p1.get_color())
#         par1.axis["right"].label.set_color(p2.get_color())
#         par2.axis["right"].label.set_color(p3.get_color())
#
#         par1.plot(all_trials[subject][trial]['max_velocity_normalized'][1],all_trials[subject][trial]['max_velocity_normalized'][0],'bo')
#         par2.plot(all_trials[subject][trial]['max_smooth_acceleration'][1],all_trials[subject][trial]['max_smooth_acceleration'][0],'bo')
#
#         plt.title(title)
#         plt.draw()
#         plt.show()
#
#     os.chdir('/Users/moramaldonado/Dropbox/Stage/M2/MT Online Experiment/Data')
#     plt.savefig("One example.png")
#
#
# def plot_trial_velocity(all_trials,subject,trial):
#
#     dataY = all_trials[subject][trial]['velocity_normalized']
#     dataX = all_trials[subject][trial]['temp_velocity']
#     plt.plot(dataX,dataY, 'b-', label= 'velocity')
#     plt.ylabel("X coordenate velocity")
#     plt.plot(all_trials[subject][trial]['max_velocity_normalized'][1],all_trials[subject][trial]['max_velocity_normalized'][0],'ko',label='maxVelocity')
#
#
# def plot_trial_time(all_trials,subject,trial):
#     title = 'Example_Subject:'+str(subject)+', trial:'+str(trial)
#     data = all_trials[subject][trial]['mouse_log']
#     for i in range(len(data)):
#         plt.plot(data[i][3], data[i][0],'r.', label='trajectory')
#     data = all_trials[subject][trial]['normalized_positions']
#     for i in range(len(data)):
#
#         plt.plot(data[i][2], data[i][0],'go', label='normalization')
#
#
# def plot_euclidean_distance(all_trials,subject,trial):
#     name = "Euclidean distance:"+'subject:'+str(subject)+',trial:'+str(trial)
#     plt.plot(all_trials[subject][trial]['temp_velocity'],all_trials[subject][trial]['euclidean_distance_True'],'r.-', label = 'towards True')
#     plt.plot(all_trials[subject][trial]['temp_velocity'],all_trials[subject][trial]['euclidean_distance_False'],'b.-', label='towards False')
#     #plt.title('Euclidean proximity to each button, subject: 0, trial: 15', fontsize=20)
#     plt.ylabel('Euclidean proximity')
#     plt.xlabel('raw_time')
#     plt.title(name)
#     plt.legend(loc='best', bbox_to_anchor=(0.5, 1),
#               ncol=2, fancybox=True, shadow=True, fontsize=9)
#     plt.savefig(name+'.png')
#
# def plot_trial_positions(all_trials,subject,trial):
#     title = 'Example_Subject:'+str(subject)+', trial:'+str(trial)
# #    data = all_trials[subject][trial]['mouse_log']
# #    for i in range(len(data)):
# #        plt.axis([-2, 2, 0, 1.5])
# #        plt.plot(data[i][0], data[i][1],'r.', label='trajectory')
#     data = all_trials[subject][trial]['normalized_positions']
#     for i in range(len(data)):
#         #plt.axis([-1.25, 1.25, 0, 1.5])
#         plt.plot(data[i][0], data[i][1],'b.', label='normalization')
#
#     plt.xlabel("X coordenate")
#     plt.ylabel("Y coordenate")
#     plt.title(title)
#
# def plot_per_subject(subject, all_trials, info, expected_response, data_type, color):
#     startX, startY = 0, 0
#     falseX, falseY = -1, 1
#     trueX, trueY = 1, 1
#     x = info[subject]['normalized_button_size']['x']
#     y = info[subject]['normalized_button_size']['y']
#
#     currentAxis = plt.gca()
#
#     currentAxis.add_patch(Rectangle((startX -(x/2), startY - y ), x, y, ec='b', fill=False))
#     currentAxis.add_patch(Rectangle((falseX - x, falseY), x, y, ec='b', fill=False))
#     currentAxis.add_patch(Rectangle((trueX, trueY ), x, y, ec='b', fill=False))
#
#     #plt.axis([-1-x-.25, 1+x+.25, 0-y , 1+y+.1])
#     plt.ylabel('y coordenate')
#     plt.xlabel('x coordenate')
#     title = data_type
#     plt.title(title)
#     plt.text(1+(x/2),1+(y/2),'B')
#     plt.text(-1-(x/2),1+(y/2),'A')
#
#     for t in range(len(all_trials[subject])):
#         px = []
#         py = []
#
#         if all_trials[subject][t]['accuracy'] == 1 and all_trials[subject][t]['expected_response']== expected_response and all_trials[subject][t][data_type] != 'NA':
#             for i in range(len(all_trials[subject][t][data_type])):
#                 px.append(all_trials[subject][t][data_type][i][0])
#                 py.append(all_trials[subject][t][data_type][i][1])
#
#             plt.plot(px,py,color)
#
#
#     plt.plot(0,0,'go')
#     plt.show()
#
#
# def super_plot(subject,all_trials,info,data_type,experiment):
#     name = str(subject)+'_ALL'
#     plt.title(name)
#
#     if experiment == 'scope':
#         A = 'surface'
#         B = 'inverse'
#     else:
#         A = 'cum'
#         B = 'dist'
#
#     plt.subplot(3,1,1)
#     plot_per_subject(subject, all_trials, info, A, 'True', data_type, 'b.-')
#     meanCurve_A = mean_trajectory_subject(subject,all_trials,A,'True','normalized_positions_space',experiment)
#     simple_plot(meanCurve_A, 'ko-','mean')
#
#     plt.subplot(3,1,2)
#     plot_per_subject(subject, all_trials, info, B, 'True', data_type, 'r.-')
#     meanCurve_B = mean_trajectory_subject(subject,all_trials,B,'True','normalized_positions_space',experiment)
#     simple_plot(meanCurve_B, 'ko-','mean')
#
#     plt.subplot(3,1,3)
#     simple_plot(meanCurve_A, 'b.-',0)
#     simple_plot(meanCurve_B, 'r.-',0)
#
#     plt.savefig(name, format='pdf')
#
# # def quick_plotting(all_trials,experiment,info):
# #
# #
# #     mean_difference(all_trials, 'long', info, 'difference', '1', 'A', experiment)
# #     mean_difference(all_trials, 'long', info, 'difference', '1', 'B', experiment)
# #     mean_difference(all_trials, 'long', info, 'difference', '2', 'B', experiment)
# #     mean_difference(all_trials, 'long', info, 'difference', '2', 'A', experiment)
# #
# #
# #     meanCurve_ControlP_T = mean_trajectory(all_trials_clean, info, 'controlp', 'True', 'False',
# #                                                'normalized_positions_space', experiment)
# #     meanCurve_ControlN_T = mean_trajectory(all_trials_clean, info, 'controln', 'True', 'False',
# #                                                'normalized_positions_space', experiment)
# #
# #         if experiment == 'scope':
# #             os.chdir('/Users/moramaldonado/Dropbox/Stage/M2/MT Online Experiment/Plots_Scope/MT_general_data')
# #         else:
# #             os.chdir('/Users/moramaldonado/Dropbox/Stage/M2/MT Online Experiment/Plots_Plurals/MT_general_data')
# #
# #         plt.figure(1)
# #         plt.subplot(211)
# #         plt.title('Mean Difference of distance (TRUE)')
# #         simple_plot_2(meanDistance_ControlP_T, 'b.-', 'controlp')
# #         simple_plot_2(meanDistance_ControlN_T, 'r.-', 'controln')
# #         plt.axhline(y=0)
# #
# #         plt.subplot(212)
# #         plt.title('Mean Difference of distance (FALSE)')
# #         simple_plot_2(meanDistance_ControlP_F, 'b.-', 'controlp')
# #         simple_plot_2(meanDistance_ControlN_F, 'r.-', 'controln')
# #         plt.axhline(y=0)
# #
# #         plt.tight_layout()
# #
# #         plt.savefig('Difference in controls (both experiments)', format='pdf')
# #         plt.close()
# #
# #         plt.figure(2)
# #         for s in range(len(all_trials_clean)):
# #             meanCurve_subject = mean_trajectory_subject(s, all_trials_clean, 'controlp', 'True',
# #                                                         'normalized_positions_space')
# #             simple_plot(meanCurve_subject, 'b.-', s)
# #
# #         simple_plot(meanCurve_ControlP_T, 'k.-', 'surface')
# #         plt.title('ControlP all subjects')
# #         plt.savefig('ControlP_True_all subjects', format='pdf')
# #         plt.close()
# #
# #         plt.figure(3)
# #         for s in range(len(all_trials_clean)):
# #             meanCurve_subject = mean_trajectory_subject(s, all_trials_clean, 'controln', 'True',
# #                                                         'normalized_positions_space')
# #             simple_plot(meanCurve_subject, 'r.-', s)
# #
# #         simple_plot(meanCurve_ControlN_T, 'k.-', 'surface')
# #         plt.title('ControlN all subjects')
# #         plt.savefig('ControlN_True_all subjects', format='pdf')
# #         plt.close()
# #
# #         plt.figure(num=4, figsize=(15, 10), dpi=80, facecolor='w', edgecolor='k')
# #         plt.subplot(211)
# #         plt.title('Trajectories TRUE items', fontsize=14)
# #         simple_plot(meanCurve_ControlP_T, 'b.-', 'controlp')
# #         simple_plot(meanCurve_ControlN_T, 'r.-', 'controln')
# #         plt.axhline(y=0)
# #
# #         plt.subplot(212)
# #         plt.title('Trajectories FALSE items', fontsize=14)
# #         simple_plot(meanCurve_ControlP_F, 'b.-', 'controlp')
# #         simple_plot(meanCurve_ControlN_F, 'r.-', 'controln')
# #         plt.tight_layout()
# #
# #         plt.savefig('Trajectories', format='pdf')
# #         plt.close()
# #
# #
