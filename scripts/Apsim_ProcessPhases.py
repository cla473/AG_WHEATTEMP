#!/usr/bin/env python

# import files
import argparse
import sys
import os
import datetime
import sqlite3
import numpy as np
import pandas as pd
import math


# define the working directories
apsim_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source"
apsim_outfiledir = "/OSM/CBR/AG_WHEATTEMP/work/output"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/met"

#these are the filter options to be used for all databases
startDate = pd.to_datetime("1957-01-01", format="%Y-%m-%d")
endDate = pd.to_datetime("2016-12-31", format="%Y-%m-%d")
varietylist = ['agt_katana','agt_scythe','axe','baxter','bolac','ega_gregory',
               'ega_wylie','h46','h45','janz','lancer','mace','sentinel','sunbri',
               'sunstate','sunvale','ventura','westonia','yitpi']



#these are required for calculations of thermal time
num3hr = int(24 / 3)
#print("num3hr: ", num3hr)    
t_range_fract = []

# pre calculate t_range_fract for speed reasons
for period in range(num3hr):
    calcValue = 0.92105 \
                + 0.1140 * period \
                - 0.0703 * math.pow(period, 2) \
                + 0.0053 * math.pow(period, 3)
    t_range_fract.append(calcValue)
#print("t_range_fract: ", t_range_fract)



def get_simulation_details(dbname):
	'''
	Opens the specified SQL Database and extracts the 'Name' details from the Simulation
	Table, and splits it to Simulation ID, Longitude, Latitude, Variety, SowDate, and
	returns ad dataframe.
	'''

	# connect to the Database
	con = sqlite3.connect(dbname)
	cur = con.cursor()

	# get contents of the _Simulation Table
	strSql = "SELECT ID as SimulationID, Name FROM _Simulations"
	dfSim = pd.read_sql_query(strSql, con, index_col = 'SimulationID')

	# split the 'Name' field into long, lat, variety and sowdate columns
	dfSim[['long','lat','variety','sowdate']] = \
	dfSim['Name'].str.extract("^(?P<long>\d+)_(?P<lat>-?\d+)_(?P<variety>\S+)_(?P<sowdate>\d+-\S+)$", expand=True)

	# format the columns
	pd.options.display.float_format = '{:,.2f}'.format
	dfSim['long'] = dfSim['long'].astype(float) / 100
	dfSim['lat'] = dfSim['lat'].astype(float) / 100

	# create a SimId column (as the original SimulationID is now an index column)
	dfSim['SimID'] = dfSim.index 

	return dfSim



def get_variety_SimIDs(dfSim, varieties)
    '''
    Creates a list of SimulationIDs based on a list of varieties
    '''

    dfSimVar = dfSim[dfSim['variety'].isin(varieties)]
    #print(dfSimVar.shape)
    #dfSimVar

    #now create a list of the SimId's
    simIds = dfSimVar.index.tolist()
    simIdStr = ', '.join(str(e) for e in simIds)

    return simIdStr




def get_report_details(dbname, simIdStr, startDate, endDate):
	'''
	Opens the specified SQL Database and extracts the details from the Report
	Table, formats the columns correctly and returns a dataframe
	'''

	# connect to the Database
	con = sqlite3.connect(dbname)
	cur = con.cursor()

	# get contents of the Report Table
    strSql = "SELECT DISTINCT SimulationID, substr([Clock.Today], 1, 10) as runDate, \
          [Wheat.Leaf.LAI] as LAI, [Wheat.AboveGround.Wt] as Biomass, \
          [Wheat.Grain.Wt] as Yield, [Wheat.Phenology.Zadok.Stage] as ZadokStage, \
          [Wheat.WaterSupplyDemandRatio] as WSDR \
          FROM Report \
          WHERE SimulationID IN (" + simIdStr + ") \
            AND [Wheat.Phenology.Zadok.Stage] > 0 \
          ORDER BY SimulationID, runDate"

	#print(datetime.datetime.now())
	dfReport = pd.read_sql_query(strSql, con, index_col="SimulationID" )
	#print(datetime.datetime.now())

	# format the date columns
	dfReport['runDate'] = pd.to_datetime(dfReport['runDate'], format="%Y-%m-%d")

	# create the SimId column
	dfReport['SimID'] = dfReport.index

	#filter the date based on the start and end dates
	dfReport = dfReport[(dfReport['runDate'] >= startDate) & (dfReport['runDate'] <= endDate) ]

	return dfReport



def get_filename(dbname):
    '''
    Takes the full path and filename for the database file, and creates the filename
    that is used for the weather file, and to save the output.

    Note:  cannot use the db filename as it doesn't have the long & lat that we require
           need to manipulate the filename to add the underscrore '_' char
           ONLY need to allow for negative (south) latitudes
    '''

    filename = os.path.basename(dbname)
    filename = os.path.splitext(filename)[0]
    nameparts = filename.split('-')
    filename = nameparts[0] + '_-' + nameparts[1]
    lat = '-' + nameparts[1]

    return filename, lat


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



def read_ApsimWeather(filename, latitude, startDate, endDate):
	'''
	Reads an apsim weather ('.met') file, removes the header information,
	calculates and adds a date column (based on year and day), and the
	average temperature (based on maxt and mint).
	'''

    # these XYPairs are use when calculating Thermal Time
    # and are specific to Wheat only
    xp = [0, 26, 37]
    fp = [0, 26, 0]

	lineNo = 0
	with open(filename, "r") as f:
		for line in f:
			lineNo = lineNo + 1
			if line.startswith('year'):
				break;

    # return the data using the starting line no (determined above)
    # original column names=['year','day', 'radn', 'maxt', 'mint', 'rain']
    metData = pd.read_table(filename, sep='\s+', header=None, skiprows=lineNo+1,
                            names=['year','dayofYear', 'radiation', 'maxTemp', 'minTemp', 'rain'])
    # add the calculated columns
    metData['runDate'] = pd.to_datetime(metData['year'].astype(str) + " " + \
                         metData['dayofYear'].astype(str), format="%Y %j")

	#filter this file based on the dates we are working with
    metData = metData[(metData['runDate'] >= startDate) & (metData['runDate'] <= endDate)] 

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
    metData['sinB'] = metData['a'] + metData['b'] * np.cos(2 * math.pi * \
					  (metData['hour'] - 12) / 24)
    metData['SC'] = 1367 * (1 + 0.033 * np.cos(2 * math.pi * (metData['dayofYear'] - 10) / 365))
    metData['sinINT'] = metData['a'] * metData['daylength'] + (24 * metData['b'] / math.pi) * \
                        np.cos((math.pi / 2) * ((metData['daylength'] / 12) - 1))

    metData['Ta'] = metData['radnJ'] / (metData['sinINT'] * 3600 * metData['SC'])
    metData['FDR'] = metData['Ta'] * -1.6 +1.32

    # calculate the Evapotranspiration
    metData['vpsl'] = 238.102 * 17.32491 * ((metData['minTemp'] + metData['maxTemp']) /2) / \
                      (((metData['minTemp'] + metData['maxTemp']) / 2) + 238.102) ** 2
    metData['ETpt'] = 1.26 * (metData['radnJ']  * (metData['vpsl'] / (metData['vpsl'] \
					  + 0.067))) / 2454000

    # sort the columns to be a little more logical
	cols=['year', 'dayofYear', 'runDate', 'daylength', 'maxTemp', 'minTemp', 'avgTemp', \
		  'ApsimTT', 'rain', 'PARIO', 'PQ', 'FDR', 'vpsl', 'ETpt']
    metData = metData[cols]


	return metData



def get_weather_details(filename, latitude, startDate, endDate):
    '''
    Retrieves the weather data for the location (long,lat) specified in the dbname,
    formats the data, and returns a dataframe
    '''

    fullfilename = metfile_sourcedir + "/c_" + filename + ".met"
    dfWeather = read_ApsimWeather(fullfilename, latitude, startDate, endDate)

    return dfWeather




def process_Apsim_dbfile(dbname):

	print("processing file: ", dbname)
	print("started at ", datetime.datetime.now())

	# retrieve the weather data from the weather '.met' file
	filename, latitude = get_filename(dbname)
	print("filename: ", filename)
	print("latitude: ", latitude)


	dfWeather = get_weather_details(filename, latitude, startDate, endDate)
	#print(dfWeather.shape)
	#print(dfWeather.head(5))


	# retrieve the Simulation Details from the DB._Sumulation table
	dfSim = get_simulation_details(dbname) 
	#print(dfSim.shape)
	#print(dfSim.head(5))

	simIDstr = get_variety_SimIDs(dfSim, varietylist)


	# retrieve the Details from the DB.Report table
	dfReport = get_report_details(dbname, simIDstr, startDate, endDate)
	#print(dfReport.shape)
	#print(dfReport.head(5))


	# combine the data with the Simulation details, so that we can get the sow date
	# and filter it again
	dfCombined = dfReport.merge(dfSim, on="SimID", how='left')

	# create a sowing date (with current year)
	dfCombined['sowingdate'] = dfCombined['sowdate'] + '-' + dfCombined['runDate'].dt.year.map(str)
	dfCombined['sowingdate'] = pd.to_datetime(dfCombined['sowingdate'], format="%d-%b-%Y")

	#may not be necessary now:
	# filter any data prior to the sowing dates for each year
	dfCombined = dfCombined[(dfCombined['runDate'] >= dfCombined['sowingdate'])]

	# filter out columns we do not require and re-order the columns
	cols = ['SimID', 'long', 'lat', 'variety', 'sowdate', 'sowingdate', 'runDate', \
			'LAI', 'Biomass', 'Yield', 'ZadokStage', 'WSDR']
	dfCombined = dfCombined[cols] 

	# now combine the report data with the weather data
	dfCombined = dfCombined.merge(dfWeather, on='runDate', how='left')

	#create bins for the phases
	bins = [0, 7, 10, 31, 39, 65, 71, 87, 89, 90]
	group_names = ['01_Germinating', '02_Emerging', '03_Vegetative', \
				   '04_StemElongation', '05_EarlyReproductive', '06_GrainSet', \
				   '07_GrainFilling', '08_Maturing', '09_Ripening']
	dfCombined['phases'] = pd.cut(dfCombined['ZadokStage'], bins, labels=group_names)


	# calculate the cumulative values required for the season
	dfCombined['cumAvgTemp'] = dfCombined.groupby(by=['SimID','sowingdate'])['avgTemp'].cumsum()
	dfCombined['cumApsimTT'] = dfCombined.groupby(by=['SimID','sowingdate'])['ApsimTT'].cumsum()
	dfCombined['cumRain'] = dfCombined.groupby(by=['SimID','sowingdate'])['rain'].cumsum()

	# these are cumulative calculations for each phase
	dfCombined['cumPhaseAvgTemp'] = dfCombined.groupby(by=['SimID','sowingdate','phases'])['avgTemp'].cumsum()
	dfCombined['cumPhaseApsimTT'] = dfCombined.groupby(by=['SimID','sowingdate','phases'])['ApsimTT'].cumsum()
	dfCombined['cumPhaseRain'] = dfCombined.groupby(by=['SimID','sowingdate','phases'])['rain'].cumsum()
	dfCombined['cumPhaseETpt'] = dfCombined.groupby(by=['SimID','sowingdate','phases'])['ETpt'].cumsum()
	dfCombined['cumPhasePARIO'] = dfCombined.groupby(by=['SimID','sowingdate','phases'])['PARIO'].cumsum()

	# need to add the '10_Harvest' phase, but cannot modify categorical data
    # so need to convert phases to strings
	dfCombined['phases'] = dfCombined['phases'].astype(str)
	dfCombined.loc[(dfCombined['ZadokStage'] == 90) & (dfCombined['cumPhaseApsimTT'] >= 300 ), \
					'phases'] = '10_Harvest'

	#  only need to keep the first row that is for '10_Harvest', so need to
    #  identify (flag) those records that need to be kept.
	dfCombined.loc[dfCombined.groupby(['SimID','sowingdate','phases']) \ 
				['cumPhaseApsimTT'].idxmin(),'firstHarvest'] = 1
	dfCombined.loc[(dfCombined['phases'] != '10_Harvest' ),'firstHarvest'] = 1
	dfCombined['firstHarvest'] = dfCombined.firstHarvest.fillna(0).astype(int)
	# now filter based on the flag we crated
	dfCombined = dfCombined[(dfCombined['firstHarvest'] == 1)]

	# -----------------------------------------------------
	#now to create the summary output file
	# -----------------------------------------------------

	#these are the figures for the season
	dfSummary = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumAvgTemp'].max().reset_index()
	dfSummary.columns = ['SimID', 'sowingdate', 'phases', 'season_cumAvgTemp']

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumApsimTT'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'season_cumApsimTT']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumRain'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'season_cumRain']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')


	#how many days in each of the phases
	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'dayCount']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	#what is the minimimum temperature for the phase
	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['minTemp'].min().reset_index()
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	#what is the maximum temperature for the phase
	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['maxTemp'].max().reset_index()
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	#what is the average of average temp for the phase
	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['avgTemp'].mean().reset_index()
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	#what is the average of average ApsimTT for the phase
	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['ApsimTT'].mean().reset_index()
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')



	# geth the values for the last day in each phase 
	fSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumPhaseavgTemp'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'cumavgTemp']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	fSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumPhaseApsimTT'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'cumApsimTT']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumPhaseRain'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'cumRain']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['WSDR'].mean().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'avgWSDR']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumPhaseETpt'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'cumETpt']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['cumPhasePARIO'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'cumPARIO']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['PARIO'].mean().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'avgPARIO']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['FDR'].mean().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'avgFDR']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['PQ'].mean().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'avgPQ']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['daylength'].mean().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'avgdaylength']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['runDate'].min().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'startDate']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['runDate'].max().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'endDate']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['Biomass'].last().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'Biomass']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['LAI'].last().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'LAI']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined.groupby(by=['SimID','sowingdate','phases'])['Yield'].last().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'Yield']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')


	# get the counts of various miniumum temperatures
	dfSum = dfCombined[dfCombined['minTemp'] <= 0].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'day<=0']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['minTemp'] <= -1].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'day<=-1']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['minTemp'] <= -2].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'day<=-2']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['minTemp'] <= -3].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days<=-3']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')


	# get the counts of various maximum temperatures
	dfSum = dfCombined[dfCombined['maxTemp'] >= 30].groupby(by=['SimID','sowingdate', 'phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=30']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['maxTemp'] >= 32].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=32']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['maxTemp'] >= 34].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=34']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['maxTemp'] >= 36].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=36']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['maxTemp'] >= 38].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=38']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')

	dfSum = dfCombined[dfCombined['maxTemp'] >= 40].groupby(by=['SimID','sowingdate','phases'])['runDate'].count().reset_index()
	dfSum.columns = ['SimID', 'sowingdate', 'phases', 'days>=40']
	dfSummary = dfSummary.merge(dfSum, on=['SimID', 'sowingdate', 'phases'], how='left')


	# now save the summary file
	outfilename = apsim_outfiledir + "/" + filename + "_summary.csv"
	dfSummary.to_csv(outfilename, encoding='utf-8', index=False)

	# to append to the file, need to use mode='a'
	#newData2.to_csv(filename, encoding='utf-8', index=False, header=False, mode='a')
	print("finished at ", datetime.datetime.now())



def main(args):
	parser = argparse.ArgumentParser(description="Processes ApsimX file")
	parser.add_argument("-f", "--filename", help="The name of the SQLite Database file")

	args = parser.parse_args()
	dbname = args.filename
	process_Apsim_dbfile(dbname)


if __name__ == '__main__':
	main(sys.argv[1:])

