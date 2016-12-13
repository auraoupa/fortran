FC=ifort
NCDFF=/opt/software/libraries/netcdf/bullxmpi/netcdf-fortran-4.4.1-4.3.3-rc2

FFLAGS= -O -I/opt/software/libraries/netcdf/bullxmpi/netcdf-4.3.3-rc2/include   -L/opt/software/libraries/netcdf/bullxmpi/netcdf-4.3.3-rc2/lib -I$(NCDFF)/include -L$(NCDFF)/lib -lnetcdf -lnetcdff -L/opt/software/libraries/hdf5/hdf5_with_bullxmpi/1.8.14/lib -lhdf5_hl -lhdf5 -ldl -lm -lz

squeeze_dim: squeeze_dim.f90
	$(FC) $(FFLAGS) squeeze_dim.f90 -o squeeze_dim -lnetcdf

