#!/usr/bin/env python
# a bar plot with errorbars
import numpy as np
import matplotlib.pyplot as plt
import csv
import sys
import argparse


#Setup arg parser and parse args
parser = argparse.ArgumentParser(description='Collects and plots data output from OpenSoC Fabric')
parser.add_argument('-r','--router',	action='store',	dest='routerUtilFile',	required=True, 	help='CSV filename with router utilizations')
parser.add_argument('-c','--channel',	action='store',	dest='channelUtilFile',	required=True, 	help='CSV filename with per channel utilizations')
parser.add_argument('-l','--latency',	action='store',	dest='latencyUtilFile',	required=True,	help='CSV filename with latency per flit data')
args 		= parser.parse_args(sys.argv[1:])
routerUtilFile 	= open(args.routerUtilFile, 'r')
channelUtilFile = open(args.channelUtilFile, 'r')
latencyUtilFile = open(args.latencyUtilFile, 'r')

recordsToSkip = 10

routerData = np.genfromtxt(routerUtilFile, delimiter=',', skip_header=recordsToSkip)
routerCount = len(routerData[0])
routerAvgUtil = []
routerStdDev  = []
for router in range(routerCount):
	routerAvgUtil.append(np.average(routerData.T[router]))
	routerStdDev.append(np.std(routerData.T[router]))

channelData = np.genfromtxt(channelUtilFile, delimiter=',', skip_header=recordsToSkip)
channelCount = len(channelData[0])
channelPerRouter = channelCount / routerCount
channelAvgUtil = []
channelStdDev  = []
for channel in range(channelCount):
	channelAvgUtil.append(np.average(channelData.T[channel]))
	channelStdDev.append(np.average(channelData.T[channel]))

latencyData = np.genfromtxt(latencyUtilFile, delimiter=',', skip_header=1)

def autolabel(rects, myAx):
    # attach some text labels
    for rect in rects:
        height = rect.get_height()
        myAx.text(rect.get_x()+rect.get_width()/2., 1.05*height, '%d'%int(height),
                ha='center', va='bottom')

# --------- Begin Router plot ----------
N = routerCount

ind = np.arange(N)  # the x locations for the groups
width = 0.35       # the width of the bars

#plt.figure(1)
#fig, ax = plt.subplots()
plt.figure(1)
ax = plt.subplot(111)
rects1 = ax.bar(ind, routerAvgUtil, width, color='r', yerr=routerStdDev)

# add some text for labels, title and axes ticks
ax.set_ylabel('Utilization (%) ')
ax.set_title('% Time Router is Busy')
ax.set_xticks(ind+width)
ax.set_xticklabels( ["Router " + str(x) for x in range(routerCount)] )

#ax.legend( (rects1[0]), ('Router!') )


autolabel(rects1, ax)
#autolabel(rects2)

#-------- Begin Latency plot -----------
plt.figure(2)
latAx = plt.subplot(111)
numBins = 20
print "min, max: ", latencyData.T[1].min(), latencyData.T[1].max()
latAx.hist(latencyData.T[1], numBins, range=(latencyData.T[1].min()+1, latencyData.T[1].max()),color='g', alpha=0.8) 
latAx.set_xlabel('Latency (clocks) ')
latAx.set_title('Latency per head flit')



#---------- Begin Channel plot -----------
for fig in range (routerCount / 4):
	plt.figure(3+fig)
	for sp in range(1, 5):
		channelAx = plt.subplot(2,2,sp)
		cc = channelPerRouter #channelCount

		chInd = np.arange(cc)  # the x locations for the groups
		chWidth = 0.35       # the width of the bars

		chRects = channelAx.bar(chInd, channelAvgUtil[(fig*4+(sp-1)):(fig*4+(sp-1))+(channelPerRouter)], chWidth, color='r') #, yerr=channelStdDev)

		# add some text for labels, title and axes ticks
		channelAx.set_ylabel('Utilization (%) ')
		channelAx.set_title('% Time Channel is Busy')
		channelAx.set_xticks(chInd+chWidth)
 
		channelAx.set_xticklabels(["r:" + str(fig*4+sp) + "c:" + str(c) for c in range(channelPerRouter)])


		autolabel(chRects, channelAx)


plt.show()

routerUtilFile.close()
channelUtilFile.close()
latencyUtilFile.close()
