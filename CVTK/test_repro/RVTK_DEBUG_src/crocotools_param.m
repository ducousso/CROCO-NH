%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% crocotools_param: common parameter file for the preprocessing
%                  of CROCO simulations using CROCOTOOLS
%
%                  This file is used by make_grid.m, make_forcing.m, 
%                  make_clim.m, make_biol.m, make_bry.m, make_tides.m,
%                  make_NCEP.m, make_OGCM.m, make_...
% 
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2005-2006 by Patrick Marchesiello and Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    6-Sep-2006 by Pierrick Penven
%  Updated    2006/10/05 by Pierrick Penven  (add tidegauge observations)
%  Updated    24-Oct-2006 by Pierrick Penven (diagnostics, chla etc...)
%  Updated    08-Apr-2009 by Gildas Cambon
%  Updated    23-Oct-2009 by Gildas Cambon
%  Updated    17-Nov-2011 by Pierrick Penven (CFSR)
%  Updated    07-Nov-2012 by Patrick Marchesiello (cleaning)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 1  - Configuration parameters
%      used by make_grid.m (and others..)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  CROCO title names and directories
%
CROCO_title  = 'Benguela Model';
CROCO_config = 'Benguela_LR';
%
% Grid dimensions:
%
lonmin =   8;   % Minimum longitude [degree east]
lonmax =  22;   % Maximum longitude [degree east]
latmin = -38;   % Minimum latitudeF  [degree north]
latmax = -26;   % Maximum latitude  [degree north]
%
% Grid resolution [degree]
%
dl = 1/3;
%
% Number of vertical Levels (! should be the same in param.h !)
%
N = 32;
%
%  Vertical grid parameters (! should be the same in croco.in !)
%
theta_s    =  6.;
theta_b    =  0.;
hc         = 10.;
vtransform =  1.; % s-coordinate type (1: old- ; 2: new- coordinates)
%
% Minimum depth at the shore [m] (depends on the resolution,
% rule of thumb: dl=1, hmin=300, dl=1/4, hmin=150, ...)
% This affect the filtering since it works on grad(h)/h.
%
hmin = 75;
%
% Maximum depth at the shore [m] (to prevent the generation
% of too big walls along the coast)
%
hmax_coast = 500;
%
% Maximum depth [m] (cut the topography to prevent
% extrapolations below WOA data)
%
hmax = 5000;
%
% Slope parameter (r=grad(h)/h) maximum value for topography smoothing
%
rtarget = 0.25;
%
% Number of pass of a selective filter to reduce the isolated
% seamounts on the deep ocean.
%
n_filter_deep_topo=4;
%
% Number of pass of a single hanning filter at the end of the
% smooting procedure to ensure that there is no 2DX noise in the 
% topography.
%
n_filter_final=2;
%
%  GSHSS user defined coastline (see m_map) 
%  XXX_f.mat    Full resolution data
%  XXX_h.mat    High resolution data
%  XXX_i.mat    Intermediate resolution data
%  XXX_l.mat    Low resolution data
%  XXX_c.mat    Crude resolution data
%
coastfileplot = 'coastline_l.mat';
coastfilemask = 'coastline_l_mask.mat';
%
% Objective analysis decorrelation scale [m]
% (if Roa=0: nearest extrapolation method; crude but much cheaper)
%
%Roa=300e3;
Roa=0;
%
interp_method = 'linear';         % Interpolation method: 'linear' or 'cubic'
%
makeplot     = 1;                 % 1: create a few graphics after each preprocessing step
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2 - Generic file and directory names 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%  CROCOTOOLS directory
%
CROCOTOOLS_dir = '../';
%
%  Run directory
%
RUN_dir=[pwd,'/'];
%
%  CROCO input netcdf files directory
%
CROCO_files_dir=[RUN_dir,'CROCO_FILES/'];
%
%  Global data directory (etopo, coads, datasets download from ftp, etc..)
%
DATADIR='../../croco_tools/'; 
%
%  Forcing data directory (ncep, quikscat, datasets download with opendap, etc..)
%
FORC_DATA_DIR = [RUN_dir,'DATA/'];
%
eval(['!mkdir ',CROCO_files_dir])
%
% CROCO file names (grid, forcing, bulk, climatology, initial)
%
grdname  = [CROCO_files_dir,'croco_grd.nc'];
frcname  = [CROCO_files_dir,'croco_frc.nc'];
blkname  = [CROCO_files_dir,'croco_blk.nc'];
clmname  = [CROCO_files_dir,'croco_clm.nc'];
bryname  = [CROCO_files_dir,'croco_bry.nc'];
ininame  = [CROCO_files_dir,'croco_ini.nc'];
bioname  = [CROCO_files_dir,'croco_frcbio.nc']; % Iron Dust forcing for PISCES
rivname =  [CROCO_files_dir,'croco_runoff.nc'];
%
% intermediate z-level data files (not used in simulations)
%
oaname   = [CROCO_files_dir,'croco_oa.nc'];    % for climatology data processing
Zbryname = [CROCO_files_dir,'croco_bry_Z.nc']; % for boundary data processing
%
% Generic forcing file root names for interannual simulations (NCEP/GFS)
%
frc_prefix=[CROCO_files_dir,'croco_frc'];      % forcing file name 
blk_prefix=[CROCO_files_dir,'croco_blk'];      % bulk file name
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Topography netcdf file name (ETOPO 2 or any other netcdf file
%  in the same format)
%
topofile = [DATADIR,'Topo/etopo2.nc'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 3 - Surface forcing parameters
%     used by make_forcing.m and by make_bulk.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% COADS directory (for climatology runs)
%
coads_dir=[DATADIR,'COADS05/'];
%
% COADS time (for climatology runs)
%
coads_time=(15:30:345); % days: middle of each month
coads_cycle=360;        % repetition of a typical year of 360 days  
%
%coads_time=(15.2188:30.4375:350.0313); % year of 365.25 days in the case
%coads_cycle=365.25;                    % of QSCAT experiments with 
%                                         climatological heat flux.
%
% Pathfinder SST data used by pathfinder_sst.m
%
pathfinder_sst_name=[DATADIR,...
                    'SST_pathfinder/climato_pathfinder.nc'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 4 - Open boundaries and initial conditions parameters
%     used by make_clim.m, make_biol.m, make_bry.m
%             make_OGCM.m and make_OGCM_frcst.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Open boundaries switches (! should be consistent with cppdefs.h !)
%
obc = [1 1 1 1]; % open boundaries (1=open , [S E N W])
%
%  Level of reference for geostrophy calculation
%
zref = -1000;
%
%  initial/boundary data options (1 = process)
%  (used in make_clim, make_biol, make_bry,
%   make_OGCM.m and make_OGCM_frcst.m)
%
makeini    = 1;   % initial data
makeclim   = 1;   % climatological data (for boundaries and nudging layers)
makebry    = 1;   % lateral boundary data
makenpzd   = 0;   % initial and boundary data for NChlPZD and N2ChlPZD2 models
makebioebus= 0;   % initial and boundary data for BioEBUS model
makepisces = 0;   % initial and boundary data for PISCES model
%
%
makeoa     = 1;   % oa data (intermediate file)
makeZbry   = 1;   % boundary data in Z coordinate (intermediate file)
insitu2pot = 1;   % transform in-situ temperature to potential temperature
%
%  Day of initialisation for climatology experiments (=0 : 1st january 0h)
%
tini=0;  
%
% Select Climatology Atlas (temp, salt and biological variables) from:
%    - World Ocean Atlas directory (WOA2009)  OR ...
%    - CARS2009 climatology directory (CARS2009)
%
woa_dir       = [DATADIR,'WOA2009/'];
cars2009_dir  = [DATADIR,'CARS2009/'];
climato_dir   = woa_dir;
%
% Pisces biogeochemical seasonal climatology
%
woapisces_dir = [DATADIR,'WOAPISCES/'];  % only compatible with woa_dir
%
% Surface chlorophyll seasonal climatology (SeaWifs)
%
chla_dir=[DATADIR,'SeaWifs/'];
%
% Runoff monthly seasonal climatology (Dai and Trenberth)
%
global_clim_riverdir=[DATADIR,'RUNOFF_DAI/'];
global_clim_rivername=[global_clim_riverdir,'Dai_Trenberth_runoff_global_clim.nc'];
%
%  Set times and cycles for the boundary conditions: 
%   monthly climatology 
%
woa_time=(15:30:345); % days: middle of each month
woa_cycle=360;        % repetition of a typical year of 360 days  
%
%woa_time=(15.2188:30.4375:350.0313); % year of 365.25 days in case
%woa_cycle=365.25;                    % of QSCAT experiments with 
%                                     % climatological boundary conditions
%
%   Set times and cycles for runoff conditions:
%   monthly climatology
%
qbar_time=[15:30:365]; 
qbar_cycle=360;
%
%   Tracer runoff concentration processing flag
%     pource_ts = 1  => Runoff tracers concentration processing is activated. 
%                       It needs the climatology file created with make_clim.m
%     psource_ts = 0 => No Runoff tracers concentration processing 
%                       It reads analytical values in croco.in 
%                       or use default value defined in analytical.F
%
psource_ts=0; 
%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 5 - Parameters for tidal forcing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TPXO file name (TPXO6 or TPXO7)
%
tidename=[DATADIR,'TPXO7/TPXO7.nc'];
%
% Self-Attraction and Loading GOT99.2 file name
%
sal_tides=1;
salname=[DATADIR,'GOT99.2/GOT99_SAL.nc'];
%
% Number of tides component to process
%
Ntides=10;
%
% Chose order from the rank in the TPXO file :
% "M2 S2 N2 K2 K1 O1 P1 Q1 Mf Mm"
% " 1  2  3  4  5  6  7  8  9 10"
%
tidalrank=[1 2 3 4 5 6 7 8 9 10];
%
% Compare with tidegauge observations
%
lon0 =  18.37;   % Example: 
lat0 = -33.91;   % Cape Town location
Z0   =  1;       % Mean depth of tide gauge
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 6 - Reference date and simulation times
%     (used for make_tides, make_CFSR (or make_NCEP), make_OGCM)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
Yorig         = 2000;          % reference time for vector time
                               % in croco initial and forcing files
%
Ymin          = 2000;          % first forcing year
Ymax          = 2000;          % last  forcing year
Mmin          = 1;             % first forcing month
Mmax          = 3;             % last  forcing month
%
Dmin          = 1;             % Day of initialization
Hmin          = 0;             % Hour of initialization
Min_min       = 0;             % Minute of initialization
Smin          = 0;             % Second of initialization
%
SPIN_Long     = 0;             % SPIN-UP duration in Years
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 7 - Parameters for Interannual forcing (SODA, ECCO, CFSR, NCEP, ...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
Download_data = 1;   % Get data from OPENDAP sites  
level         = 0;   % AGRIF level; 0 = parent grid
%					  
NCEP_version  = 3;   % NCEP version: 
                     % [ CFSR up-to-date product are recommandated ]
                     %  1: NCEP/NCAR Reanalysis, 1/1/1948 - present
                     %  2: NCEP-DOE Reanalysis, 1/1/1979 - present
                     %  3: CFSR (Climate Forecast System Reanalysis), 
                     %           1/1/1979 - 31/3/2011
%					      
% Option for using local datasets (previously downloaded)
% rather than online opendap procedure
%
Get_My_Data   = 0;     % 1: use local datasets
%
% Provide corresponding directory paths
%
if NCEP_version  == 1;
  My_NCEP_dir  = [DATADIR,'NCEP_REA1/'];
elseif NCEP_version  == 2;
  My_NCEP_dir  = [DATADIR,'NCEP_REA2/'];
elseif NCEP_version  == 3;
  My_NCEP_dir  = [DATADIR,'CFSR/'];
end
My_QSCAT_dir = [DATADIR,'QSCAT/'];
My_SODA_dir  = [DATADIR,'SODA/'];
My_ECCO_dir  = [DATADIR,'ECCO/'];
%
%--------------------------------------------
% Options for make_NCEP and make_QSCAT_daily
%--------------------------------------------
%
% NCEP data directory for files downloaded via opendap
%
if NCEP_version  == 1;
  NCEP_dir= [FORC_DATA_DIR,'NCEP1_',CROCO_config,'/']; 
elseif NCEP_version  == 2;
  NCEP_dir= [FORC_DATA_DIR,'NCEP2_',CROCO_config,'/'];
elseif NCEP_version  == 3;
  NCEP_dir= [FORC_DATA_DIR,'CFSR_',CROCO_config,'/']; 
end
makefrc      = 1;       % 1: create forcing files
makeblk      = 1;       % 1: create bulk files
QSCAT_blk    = 0;       % 1: a) correct NCEP frc/bulk files with
                        %        u,v,wspd fields from daily QSCAT data
                        %    b) download u,v,wspd in QSCAT frc file
add_tides    = 0;       % 1: add tides
%
% Overlap parameters 
%
itolap_qscat = 2;      % 2 records for daily  QSCAT
itolap_ncep  = 8;      % 8 records for 4-daily NCEP
%
%--------------------------------------------------
% Options for make_QSCAT_daily and make_QSCAT_clim   
%--------------------------------------------------
%
QSCAT_dir        = [FORC_DATA_DIR,'QSCAT_',CROCO_config,'/']; % QSCAT data directory
QSCAT_frc_prefix = [frc_prefix,'_QSCAT_'];                   %  generic file name
                                                             %  for interannual simulations
QSCAT_clim_file  = [DATADIR,'QuikSCAT_clim/',...             % QuikSCAT climatology file
                    'croco_SCOW_month_clim_1999_2009.nc'];    % for make_QSCAT_clim.
%
%--------------------------------------------------
%  Options for make_ECMWF and make_ECMWF_daily  
%--------------------------------------------------
%
ECMWF_dir= [FORC_DATA_DIR,'ECMWF_',CROCO_config,'/'];    % ECMWF data directory
My_ECMWF_dir=[FORC_DATA_DIR,'ERAI/'];                 % ERA-I data downloaded with python script
itolap_ecmwf = 3;                                      % 3 records for daily  ECMWF
% 
%
%-----------------------
% Options for make_OGCM 
%-----------------------
%
OGCM        = 'SODA';        % Select the OGCM: SODA, ECCO
%
OGCM_dir    = [FORC_DATA_DIR,OGCM,'_',CROCO_config,'/']; % OGCM data directory
bry_prefix  = [CROCO_files_dir,'croco_bry_',OGCM,'_'];    % generic boundary file name
clm_prefix  = [CROCO_files_dir,'croco_clm_',OGCM,'_'];    % generic climatology file name
ini_prefix  = [CROCO_files_dir,'croco_ini_',OGCM,'_'];    % generic initial file name
OGCM_prefix = [OGCM,'_'];                               % generic OGCM file name 
%
% Number of OGCM bottom levels to remove 
% (usefull if CROCO depth is shallower than OGCM depth)
%
rmdepth     = 2;
%
% Overlap parameters : nb of records around each monthly sequence
%
itolap_a    = 1;   % before
itolap_p    = 1;   % after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 8 - Parameters for the forecast system
%
%     --> select OGCM name above (mercator ...)
%     --> don't forget to define in cppdefs.h:
%                    - ROBUST_DIAG
%                    - CLIMATOLOGY
%                    - BULK_FLUX
%                    - TIDES if you choose so, but without TIDERAMP
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
FRCST_dir = [FORC_DATA_DIR,'Forecast/'];  % path to local OGCM data directory
%
% Number of hindcast/forecast days
%
if strcmp(OGCM,'ECCO')
  hdays=1;
  fdays=6;
elseif strcmp(OGCM,'mercator')
  hdays=2;
  fdays=3;
end
%
% Local time= UTC + timezone
%
timezone = +7;
%
% Add tides
%
add_tides_fcst = 1;       % 1: add tides
%
% MERCATOR case: Set login/password (see http://marine.copernicus.eu)
%                and path to your motu-client-python/ package to
%                download the data.
%                Various sets of data are proposed in the 
%                Copernicus web site (Mercator, UK Met Office ...)
%
if strcmp(OGCM,'mercator')
  user     = 'XXX';
  password = 'XXX';
  pathMotu ='../Forecast_tools/';
  mercator_type=2;   % 1 -->  1/12 deg Mercator forecast
                     % 2 -->  1/4  deg Met-Office forecast (GloSea5)
end
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 9 Parameters for the diagnostic tools
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
DIAG_dir = [CROCOTOOLS_dir,'Diagnostic_tools/'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











