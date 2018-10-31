#!/usr/bin/env python

#----------------------------------------------------------------------------------------
# name:        extend_ApsimMet.py
# written by:  Loretta Clancy
# date:        30 October 2018
# purpose:     Takes an apsim formatted '.met' file and adds additional columns/
#              calculations so that it can be used for summarising information
#              at a later time (when re-matched with the output from Apsim Simulations
#----------------------------------------------------------------------------------------

#Import the required libraries
import argparse
import sys
import os
import datetime
import numpy as np
import pandas as pd
import math


#these are required for calculations of thermal time
num3hr = int(24 / 3)
#print("num3hr: ", num3hr)
t_range_fract = []

# pre calculate t_range_fract for speed reasons
for period in range(num3hr):
    calcValue = 0.92105 + 0.1140 * period - 0.0703 * math.pow(period, 2) + 0.0053 * math.pow(period, 3)
    t_range_fract.append(calcValue)

# these XYPairs are use when calculating Thermal Time and are specific to Wheat only
xp = [0, 26, 37]
fp = [0, 26, 0]



def getFileDetails(filename, rowno):

	rowno = int(rowno)
	metdf = pd.read_csv(filename)
	#print(metdf.head())
	infile = metdf['fullname'].iloc[rowno-1]
	outfile = metdf['filename'].iloc[rowno-1]
	latitude = metdf['latitude'].iloc[rowno-1]

	return infile, outfile, latitude


def linint_3hrly_Temperature(tmax, tmin, xp, fp):
	'''
	Eight interpolations of the air temperature are calculated using 
	a three-hour correction factor.
	For each air three-hour air temperature, a value is calculated.
	The eight three-hour estimates are then averaged to obtain the daily value.
	'''

	#Local Variables
	tot = 0.0            #sum_of of 3 hr interpolations

	for period in range(1, num3hr):
		#get mean temperature for 3 hr period (oC)
		tmean_3hour = tmin + (t_range_fract[period-1] * (tmax - tmin))
		tot = tot + np.interp(tmean_3hour, xp,fp)
		#print("tmean_3hour: ", tmean_3hour, " - tot: ", tot)

	return tot / num3hr;


def process_ApsimWeather(filename, outfile, latitude):
	'''
	Reads an apsim weather ('.met') file, removes the header information,
	calculates and adds a date column (based on year and day), and the
	average temperature (based on maxt and mint).
	'''

	lineNo = 0
	with open(filename, "r") as f:
		for line in f:
			lineNo = lineNo + 1
			if line.startswith('year'):
				break;

	# return the data using the starting line no (determined above)
	# original column names=['year','day', 'radn', 'maxt', 'mint', 'rain', 'evap', 'vp']
	metData = pd.read_table(filename, sep='\s+', header=None, skiprows=lineNo+1, index_col=False,
							names=['year','dayofYear', 'radiation', 'maxTemp', 'minTemp', 'rain', 'evap', 'vp'])

	# add the calculated columns
	metData['runDate'] = pd.to_datetime(metData['year'].astype(str) + " " + metData['dayofYear'].astype(str), format="%Y %j")

	# this may need to be the thermal time, not just average temp
	metData['avgTemp'] = (metData['maxTemp'] + metData['minTemp']) / 2

	# calculate the Apsim Thermal Time
	metData['ApsimTT'] = metData.apply(lambda x: linint_3hrly_Temperature(x['maxTemp'], x['minTemp'], xp, fp), axis=1)

	#convert the radiation from MJ/m2/day to Photosynthetically active radiation (PAR)
	metData['PARIO'] = metData['radiation'] * 0.47

	#convert the measurement unit for the radiation from MJ/m2/day to J/m2/day
	metData['radnJ'] = metData['radiation'] * 1000000

	metData['PQ'] = metData['PARIO'] / metData['avgTemp']

	# calculation the day length
	radians = math.pi/180
	lambdaRadians = float(latitude) * radians

	sinLAT = math.sin(lambdaRadians)
	cosLAT = math.cos(lambdaRadians)
	sinDMC = math.sin(radians * 23.45)

	#print("radians: ", radians)
	#print("lambdaRadians: ", lambdaRadians)
	#print("sinLAT: ", sinLAT)
	#print("cosLAT: ", cosLAT)
	#print("sinDMC: ", sinDMC)

	metData['sinDEC'] = -sinDMC * np.cos(2 * math.pi * (metData['dayofYear'] + 10) / 365)
	metData['cosDEC'] = np.sqrt(1 - (metData['sinDEC'] * metData['sinDEC']))
	metData['a'] = sinLAT * metData['sinDEC']
	metData['b'] = cosLAT * metData['cosDEC']

	metData['daylength'] = 12 * (1 + (2 / math.pi) * np.arcsin(metData['a']/metData['b']))

	# calculate the Fraction Disfused Radiation (FDR)
	metData['hour'] = np.mod(metData['dayofYear'], 1) * 24
	metData['sinB'] = metData['a'] + metData['b'] * np.cos(2 * math.pi * (metData['hour'] - 12) / 24)
	metData['SC'] = 1367 * (1 + 0.033 * np.cos(2 * math.pi * (metData['dayofYear'] - 10) / 365))
	metData['sinINT'] = metData['a'] * metData['daylength'] + (24 * metData['b'] / math.pi) * \
						np.cos((math.pi / 2) * ((metData['daylength'] / 12) - 1))

	metData['Ta'] = metData['radnJ'] / (metData['sinINT'] * 3600 * metData['SC'])
	metData['FDR'] = metData['Ta'] * -1.4545 + 1.2182

	# calculate the Evapotranspiration
	metData['vpsl'] = 238.102 * 17.32491 * ((metData['minTemp'] + metData['maxTemp']) /2) / \
						(((metData['minTemp'] + metData['maxTemp']) / 2) + 238.102) ** 2
	metData['ETpt'] = 1.26 * (metData['radnJ']  * (metData['vpsl'] / (metData['vpsl'] + 0.067))) / 2454000

	# round the values to 5 decimal places
	decPlaces = 4
	metData['ApsimTT'] = np.round(metData['ApsimTT'], decimals=decPlaces)
	metData['PARIO'] = np.round(metData['PARIO'], decimals=decPlaces)
	metData['PQ'] = np.round(metData['PQ'], decimals=decPlaces)
	metData['sinDEC'] = np.round(metData['sinDEC'], decimals=decPlaces)
	metData['cosDEC'] = np.round(metData['cosDEC'], decimals=decPlaces)
	metData['a'] = np.round(metData['a'], decimals=decPlaces)
	metData['b'] = np.round(metData['b'], decimals=decPlaces)
	metData['daylength'] = np.round(metData['daylength'], decimals=decPlaces)
	metData['hour'] = np.round(metData['hour'], decimals=decPlaces)
	metData['sinB'] = np.round(metData['sinB'], decimals=decPlaces)
	metData['SC'] = np.round(metData['SC'], decimals=decPlaces)
	metData['sinINT'] = np.round(metData['sinINT'], decimals=decPlaces)
	metData['Ta'] = np.round(metData['Ta'], decimals=decPlaces)
	metData['FDR'] = np.round(metData['FDR'], decimals=decPlaces)
	metData['vpsl'] = np.round(metData['vpsl'], decimals=decPlaces)
	metData['ETpt'] = np.round(metData['ETpt'], decimals=decPlaces)

	metData.to_csv(outfile, encoding='utf-8', index=False)

	#return metData
	#return metData



def main(args):
	parser = argparse.ArgumentParser(description="Addes addition columns to an Apsim formatted '.met' file")
	parser.add_argument("-f", "--filename", help="The name of the file containing the list of .met iles")
	parser.add_argument("-o", "--output", help="The Dirctory path that the file will be output toning the list of .met iles")
	parser.add_argument("-r", "--rowno", help="The row number in the file that is to be processed")

	args = parser.parse_args()
	filename = args.filename
	outpath = args.output
	rowno = args.rowno

	infile, outfile, latitude = getFileDetails(filename, rowno)
	outfile = outpath + "/" + outfile
	#print(infile, ", ", outfile,  ", ", latitude)
	process_ApsimWeather(infile, outfile, latitude)


if __name__ == '__main__':
	main(sys.argv[1:])
