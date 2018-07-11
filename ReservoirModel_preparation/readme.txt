Set up intelmpi environment on henry2. 
  
 source /usr/local/apps/R/R-3.4.2.csh
 source /usr/local/apps/mpich3/centos7/intelmpi2017.csh
 

Set up fortran compiler path on neer. 

export PATH=/opt/intel/bin/:/opt/intel/compilers_and_libraries/linux/mpi/intel64/bin:${PATH} 

Data preparation: 

1. Go to 'ReservoirModel_preparation' folder. 
2. In 'make_outputs.R' first check the 'fold.path'. If not 'sudo user' copy everything to your local directory. Change fold.path accordingly. 
3. Change date.start, date.end. Save and exit. 
4. RUN:  (sudo) Rscript make_outputs.R 
5. Copy all files from /output_files to Validation/
6. copy Release.dat to decisionvar_details.dat
7. change the number of time steps in 'input.dat' (first number in line #1) 
8. copy the path of validation folder. 
9. Be at the folder where you have /DATA
10. for user_details.dat :   RUN: ./TUNE.sh <copied folder path> 
11. for reservoir_details.dat:   RUN: ./TUNE_reser_details.sh <ntimesteps> <copied folder path> <starting_month_number> <end_month_number>

Simulation: 
12. RUNE: make clean > make > ./multireservoir 

FSQP: 

1. Uncomment lines #135 -  140 and 184. Comment out constr_res function call at #190 
 




