% create_PLSR_comparisons_level1_warningPeriods_categorical.m
% Sarah West
% 6/13/22

% A script that creates a structure of information to run comparisons
% between different subsets of data for the Random Motorized Treadmill
% experiments. Each comparison will be run per mouse, eventually run a
% second-level analysis across mice. 

% (This is for first-level PLSR comparisons);

% 3 main pieces for each comparison:
% name --> the working name of the comparison, for file names.
% variablesToUse --> the response variable categories you're interested in
% indices --> the period indices from periods_nametable that are relevant
% to this comparison & will find the related brain data & response
% variable pairs. Need these indices when you're first getting the comparison's
% dataset from the larger 
% type --> just the name of the behavior type in its own entry, to make it
% easier to manipulate later.

%% Initial setup
clear all; 

% Create the experiment name.
parameters.experiment_name='Random Motorized Treadmill';

% Output directory name basis
parameters.dir_base='Y:\Sarah\Analysis\Experiments\';
parameters.dir_exper=[parameters.dir_base parameters.experiment_name '\']; 

% Load the periods_nametable for this experiment/analysis.
% (For PLSR, was created with create_PLSR_comparisons.m).
load([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat'], 'periods');

% Continuous variable names
continuous_variable_names = {'speed_vector', 'accel_vector', 'duration_vector', 'pupil_diameter_vector'};

% can search tables like this:
% g = periods_table(string(periods_table.condition)=="m_accel" & string(periods_table.speed)=="2000", :);

counter = 0; 

%% All walking warnings against walk warning maintaining

%% Rest (start) warning vs rest warning maintaining

%% Warned transitions vs unwarned matching transitions
% Do a second (mid-level) comparison across these before moving up to
% across mice.
% start
% stop
% accel 
% decel 

%% Walk maintaining vs walk m_p no change

%% Rest maintaining vs rest m_p no change

%% Different (walk) transition warnings
% See if there's any clear indication the mouse can tell the difference.
% accel vs decel
% decel vs stop
% accel vs stop



%% Save 
save([parameters.dir_exper 'PLSR\comparisons_level1_warningPeriods_categorical.mat'], 'comparisons');