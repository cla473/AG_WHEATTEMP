#import files
import argparse
import sys
import os
import sqlite3
import pandas as pd


#define the working directories
apsim_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source"
apsim_outfiledir = "/OSM/CBR/AG_WHEATTEMP/work"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/met"


def getReportData(dbname):
	print("dbname: ", dbname)



def main(args):
	parser = argparse.ArgumentParser(description="Processes ApsimX file")
	parser.add_argument("-f", "--filename", help="The name of the SQLite Database file")

	args = parser.parse_args()
	dbname = args.filename



if __name__ == '__main__':
	main(sys.argv[1:])

