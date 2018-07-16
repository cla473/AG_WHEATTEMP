#import files
import argparse
import sys
import os
import datetime
import sqlite3
import numpy as np
import pandas as pd


#define the working directories
apsim_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source"
apsim_outfiledir = "/OSM/CBR/AG_WHEATTEMP/work"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/met"


def get_simulation_details(dbname):
	'''
	Opens the specified SQL Database and extracts the 'Name' details from the Simulation
	Table, and splits it to Simulation ID, Longitude, Latitude, Variety, SowDate, and
	returns ad dataframe.
	'''

	con = sqlite3.connect(dbname)
	cur = con.cursor()

	#get contents of the _Simulation Table
	strSql = "SELECT ID as SimulationID, Name FROM _Simulations"
	dfSim = pd.read_sql_query(strSql, con, index_col = 'SimulationID')

	#Splite the 'Name' field into long, lat, variety and sowdate columns
	dfSim[['long','lat','variety','sowdate']] = \
	dfSim['Name'].str.extract("^(?P<long>\d+)_(?P<lat>-?\d+)_(?P<variety>\S+)_(?P<sowdate>\d+-\S+)$", expand=True)

	#format the columns
	pd.options.display.float_format = '{:,.2f}'.format
	dfSim['long'] = dfSim['long'].astype(float) / 100
	dfSim['lat'] = dfSim['lat'].astype(float) / 100

	#create a SimId column (as the original SimulationID is now an index column)
	dfSim['SimID'] = dfSim.index 

	return dfSim



def process_Apsim_dbfile(dbname):

	print("dbname: ", dbname)
	#retrieve the Simulation Details from the DB ._Sumulation table
	dfSim = get_simulation_details(dbname) 
	print(dfSim.shape)
	print(dfSim.head(5))






def main(args):
	parser = argparse.ArgumentParser(description="Processes ApsimX file")
	parser.add_argument("-f", "--filename", help="The name of the SQLite Database file")

	args = parser.parse_args()
	dbname = args.filename



if __name__ == '__main__':
	main(sys.argv[1:])

