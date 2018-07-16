#import files
import os
import sqlite3
import pandas as pd


#define the working directories
apsim_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source"
apsim_outfiledir = "/OSM/CBR/AG_WHEATTEMP/work"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/met"



def main():

	#read the filenames
	dbfile_df = pd.DataFrame(columns=['filename'])
	dbfile_df.filenames = sorted(apsim_sourcedir+'/'+f for f in os.listdir(apsim_sourcedir) if f.endswith('.db'))
	#print(dbfile_df.head())

	for filename in dbfile.filenames:
		print(filename)



if __name__ == '__main__':
	main()
