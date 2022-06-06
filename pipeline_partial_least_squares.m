% pipeline_partial_least_squares.m
% Sarah West
% 6/4/22 
% Sets up variables for & runs partial least squares regression analysis
% for Random Motorized Treadmill experiements. 

%% Initial Setup  
% Put all needed paramters in a structure called "parameters", which you
% can then easily feed into your functions. 
% Use correlations, Fisher transformed, mean removed within mice (mean
% removed for at least the cases when you aren't using mice as response
% variables).

clear all; 

% Create the experiment name.
parameters.experiment_name='Random Motorized Treadmill';

% Output directory name bases
parameters.dir_base='Y:\Sarah\Analysis\Experiments\';
parameters.dir_exper=[parameters.dir_base parameters.experiment_name '\']; 

% Load mice_all, pass into parameters structure
load([parameters.dir_exper '\mice_all.mat']);
parameters.mice_all = mice_all;

% ****Change here if there are specific mice, days, and/or stacks you want to work with**** 
parameters.mice_all = parameters.mice_all([1:6 8]);

% Other parameters
parameters.digitNumber = 2;
parameters.yDim = 256;
parameters.xDim = 256;
number_of_sources = 32; 

% Load periods_nametable_PLSR.m, if it exists yet. (Otherwise is created in
% first step).
if isfile([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat'])
    load([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat']);
    parameters.periods = periods;

    % Also load the indices to remove
    load([parameters.dir_exper 'PLSR\indices_to_remove.mat']);
    parameters.indices_to_remove = indices_to_remove;
end

% Put relevant variables into loop_variables.
parameters.loop_variables.mice_all = parameters.mice_all;
parameters.loop_variables.periods = periods.condition; 
parameters.loop_variables.conditions = {'motorized'; 'spontaneous'};
parameters.loop_variables.conditions_stack_locations = {'stacks'; 'spontaneous'};

%% Create periods_nametable_forPLSR.m
% If hasn't been created already. 
if ~isfile([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat'])
    create_periods_nametable_forPLSR
    load([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat']);
    parameters.periods = periods; 

    % Also load the indices to remove
    load([parameters.dir_exper 'PLSR\indices_to_remove.mat']);
    parameters.indices_to_remove = indices_to_remove;
end

%% Remove correlations for periods you don't want to use. 

%% Remove correlations for first 2 seconds of motorized warning/ prep periods.

%% Replicate static response variables per instance. 
% Ones that are static across instances. Put into a new table/cell array/matrix. 
% (Motorized vs spontaneous, type,  motorized speed & accel, motorized & spontaneous duration


%% Put in varying response variables. 
% Ones that vary by instance: spontaneous speed, accel. Are save 
% elsewhere & need to be loaded in.

