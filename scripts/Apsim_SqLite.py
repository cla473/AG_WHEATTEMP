# import files
import argparse
import sys
import os
import datetime
import sqlite3
import numpy as np
import pandas as pd


# define the working directories
apsim_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source"
apsim_outfiledir = "/OSM/CBR/AG_WHEATTEMP/work"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/met"


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



def get_report_details(dbname):
	'''
	Opens the specified SQL Database and extracts the details from the Report
	Table, formats the columns correctly and returns a dataframe
	'''

	# connect to the Database
	con = sqlite3.connect(dbname)
	cur = con.cursor()

	# get contents of the Report Table
	strSql = "SELECT SimulationID, substr([Clock.Today], 1, 10) as runDate, \
          [Wheat.Leaf.LAI] as LeafLAI, [Wheat.AboveGround.Wt] as AboveGroundWeight, \
          [Wheat.Grain.Wt] as GrainWeight, [Wheat.Phenology.Zadok.Stage] as ZadokStage, \
          [Wheat.WaterSupplyDemandRatio] as WaterSupplyDemandRatio, \
          [Wheat.Root.NUptake] as RootNUptake, [Wheat.Leaf.Fn] as LeafFn \
          FROM Report \
          ORDER BY SimulationID, runDate"

	#print(datetime.datetime.now())
	dfReport = pd.read_sql_query(strSql, con, index_col="SimulationID" )
	#print(datetime.datetime.now())

	# format the date columns
	dfReport['runDate'] = pd.to_datetime(dfReport['runDate'], format="%Y-%m-%d")

	# create the SimId column
	dfReport['SimID'] = dfReport.index

	return dfReport



def get_weather_filename(sourceDir, dbname):
	'''
	Takes in the sourceDirectory for the met files, and the SQLite database filename
	and returns the fullpath/filename of the met file

	Note:  cannot use the db filename as it doesn't have the long & lat that we require
	       need to manipulate the filename to add the underscrore '_' char
	'''

	filename = os.path.basename(dbname)
	filename = os.path.splitext(filename)[0]
	nameparts = filename.split('-')

	filename = nameparts[0] + '_-' + nameparts[1]
	filename = sourceDir + "/c_" + filename + ".met"

	return filename



def read_ApsimWeather(filename):
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
	# original column names=['year','day', 'radn', 'maxt', 'mint', 'rain']
	metData = pd.read_table(filename, sep='\s+', header=None, skiprows=lineNo+1,
                            names=['year','dayofYear', 'radiation', 'maxTemp', 'minTemp', 'rain'])

	# add the calculated columns
	metData['runDate'] = pd.to_datetime(metData['year'].astype(str) + " " + metData['dayofYear'].astype(str), format="%Y %j")

	# this may need to be the thermal time, not just average temp
	metData['avgTemp'] = (metData['maxTemp'] + metData['minTemp']) / 2

	# sort the columns to be a little more logical
	cols=['year', 'dayofYear', 'runDate', 'maxTemp', 'minTemp', 'avgTemp', 'rain', 'radiation']
	metData = metData[cols]

	return metData



def get_weather_details(dbname):
	'''
	Retrieves the weather data for the location (long,lat) specified in the dbname,
	formats the data, and returns a dataframe
	'''

	filename = get_weather_filename(metfile_sourcedir, dbname)
	dfWeather = read_ApsimWeather(filename)

	return dfWeather



def process_Apsim_dbfile(dbname):

	print("dbname: ", dbname)

	# retrieve the Simulation Details from the DB._Sumulation table
	dfSim = get_simulation_details(dbname) 
	print(dfSim.shape)
	print(dfSim.head(5))

	# retrieve the weather data from the weather '.met' file
	dfWeather = get_weather_details(dbname)
	print(dfWeather.shape)
	print(dfWeather.head(5))

	# retrieve the Details from the DB.Report table
	dfReport = get_report_details(dbname)
	print(dfReport.shape)
	print(dfReport.head(5))





def main(args):
	parser = argparse.ArgumentParser(description="Processes ApsimX file")
	parser.add_argument("-f", "--filename", help="The name of the SQLite Database file")

	args = parser.parse_args()
	dbname = args.filename



if __name__ == '__main__':
	main(sys.argv[1:])

