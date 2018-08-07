#!/usr/bin/env python

# import files
import argparse
import sys
import os
import datetime
import numpy as np
import pandas as pd


# define the working directories
sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/output"



def process_summary_files(filename, filter_phase, filter_year):

	print("processing file: ", filename)
	print("started at ", datetime.datetime.now())

	filelist_df = pd.read_csv(filename, header=None)
	filelist_df.columns=['filename']

	for fname in filelist_df.filename:
	    print(fname)

		#read the file
		dfData = pd.read_csv(fname)

		#filter and re-format that data as required
		dfData = dfData[(dfData['phases'] == filter_phase)]
		dfData['sowingdate'] = pd.to_datetime(dfData['sowingdate'], format="%Y-%m-%d")
		dfData['year'] = dfData['sowingdate'].dt.year
		dfData['sowdate'] = dfData['sowingdate'].dt.strftime("%d-%B")
		dfData = dfData[(dfData['year'] == filter_year)]

		#get rid of the columns we don't want, and rename the 'dodgy' names
		cols = ['SimID', 'variety', 'long', 'lat', 'sowdate', 'phases', 'dayCount', \
				'maxTemp', 'avgTemp', 'days>=30', 'days>=32']
		dfData = dfData[cols]

		dfData.rename(columns={'days>=30': 'daysGreaterEqual30', 'days>=32': 'daysGreaterEqual32'}, inplace=True)

		#creath the output filename
		outfile = sourcedir + "/" + filter_phase + "_" + str(filter_year) + ".csv"

		#output the data to a new file, if it doesn't exists, or append if it does
		if not os.path.isfile(outfile):
			dfData.to_csv(outfile, header=True, encoding='utf-8', index=False)
		else:
			dfData.to_csv(outfile, header=False, mode='a', encoding='utf-8', index=False)


	print("finished at ", datetime.datetime.now())



def main(args):
	parser = argparse.ArgumentParser(description="Processes Summary files")
	parser.add_argument("-f", "--filename", help="The list of files to proces.")
	parser.add_argument("-p", "--phase", help="The phase to filter the data by (ie, '07_GrainFilling').")
	parser.add_argument("-y", "--year", help="The year to filter the data by (between 1957 and 2017').")

	args = parser.parse_args()
    filename = args.filename
	fphase = args.phase
    fyear = args.year
	process_summary_files(filename, fphase, fyear)



if __name__ == '__main__':
	main(sys.argv[1:])

