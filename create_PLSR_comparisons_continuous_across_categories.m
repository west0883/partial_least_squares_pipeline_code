% create_PLSR_comparisons_continuous_across_categories.m
% Sarah West
% 6/29/22

% A script that creates a structure of information to run comparisons
% between different subsets of data for the Random Motorized Treadmill
% experiments. Each comparison will be run per mouse, eventually run a
% second-level analysis across mice. 

% (This is for PLSR comparisons of continuous variables across behavior periods);

% 3 main pieces for each comparison:
% name --> the working name of the comparison, for file names.
% variablesToUse --> the response variable categories you're interested in
% indices --> the period indices from periods_nametable that are relevant
% to this comparison & will find the related brain data & response
% variable pairs. Need these indices when you're first getting the comparison's
% dataset from the larger 
% type --> just the name of the behavior type in its own entry, to make it
% easier to manipulate later.
% Mice not to use--> mice that aren't relevent to the comparison, such as
% mice that didn't walk very much during a spontaneous walk condition.

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

% Pull out the relevant columns/info from periods
period_motorized_vs_spon = periods.motorized_vs_spon;
period_types = periods.type;
indices_motorized = strcmp(period_motorized_vs_spon, 'motorized');
indices_spontaneous = strcmp(period_motorized_vs_spon, 'spontaneous');

% can search tables like this:
% g = periods_table(string(periods_table.condition)=="m_accel" & string(periods_table.speed)=="2000", :);

counter = 0; 

%% Rest, motorized vs spontaneous

counter = counter + 1;

comparisons(counter).name = 'rest_motorizedvsspon_continuous_across_categories';
comparisons(counter).variablesToUse = {'pupil_diameter_vector' };
comparisons(counter).mice_not_to_use = {};
comparisons(counter).subtraction_order  = {'motorized', 'spontaneous'}; % Subtract spontaneous from motorized

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

comparisons(counter).indices = find(indices_type);


%% Walk, motorized vs spontaneous
counter = counter + 1;

comparisons(counter).name = 'walk_motorizedvsspon_continuous_across_categories';
comparisons(counter).variablesToUse = {'speed_vector','pupil_diameter_vector' };
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).subtraction_order  = {'motorized', 'spontaneous'}; % Subtract spontaneous from motorized

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

comparisons(counter).indices = find(indices_type);

%% Save 
save([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat'], 'comparisons');