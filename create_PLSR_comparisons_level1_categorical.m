% create_PLSR_comparisons_level1_categorical.m
% Sarah West
% 6/9/22

% A script that creates a structure of information to run comparisons
% between different subsets of data for the Random Motorized Treadmill
% experiments. Each comparison will be run per mouse, eventually will have
% a second-level comparison across mice.

% (This is for first-level PLSR comparisons);

% 3 main pieces for each comparison:
% name --> the working name of the comparison, for file names.
% variablesToUse --> the response variable categories you're interested in
% indices --> the period indices from periods_nametable that are relevant
% to this comparison & will find the related brain data & response
% variable pairs. Is important for when comparison dataset is first
% removecd from larger dataset. 
% variablesToSubtract --> continuous variables whose effect have already
% been calculated in a level 1 continuous variable comparison, which you
% want to subtract out of the data here before running a categorical
% comparison.

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
continuous_variable_names = {'speed_vector', 'accel_vector', 'duration_vector'};

% Pull out the relevant columns/info from periods
period_motorized_vs_spon = periods.motorized_vs_spon;
period_types = periods.type;
indices_motorized = strcmp(period_motorized_vs_spon, 'motorized');
indices_spontaneous = strcmp(period_motorized_vs_spon, 'spontaneous');

% Output filename
filename_out = [parameters.dir_exper 'PLSR\comparisons_level1_categorical.mat'];
filename_out_variablesToSubtract = [parameters.dir_exper 'PLSR\variablesToSubtract_level1_categorical.mat'];

% can search tables like this:
% g = periods_table(string(periods_table.condition)=="m_accel" & string(periods_table.speed)=="2000", :);

counter = 1;

%% Set up list of variables to subtract from each behavior type. 
variablesToSubtract.rest = [];
variablesToSubtract.walk = {'speed_vector'};
variablesToSubtract.start = {'speed_vector', 'accel_vector', 'duration_vector'};
variablesToSubtract.stop = {'speed_vector', 'accel_vector', 'duration_vector'};
variablesToSubtract.accel = {'speed_vector', 'accel_vector', 'duration_vector'};
variablesToSubtract.decel = {'speed_vector', 'accel_vector', 'duration_vector'};
variablesToSubtract.finished = {'speed_vector', 'duration_vector'};
variablesToSubtract.finished_stop = {'duration_vector'};

save(filename_out_variablesToSubtract, 'variablesToSubtract');

%% All types 
% A first-pass to see if this is viable at all? 

% % Motorized
% comparisons(counter).name = 'motorized_alltypes';
% comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
% 
% % Get relevent indices for this type. (all the motorized)
% comparisons(counter).indices = find(indices_motorized);
% 
% counter = counter + 1; 
% 
% % Spontaneous
% comparisons(counter).name = 'spontaneous_alltypes';
% comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
% 
% % Get relevent indices for this type. (all the spontaneous)
% comparisons(counter).indices = find(indices_spontaneous);
% 
% counter = counter + 1; 


%% Rest vs walk 
% Motorized
comparisons(counter).name = 'motorized_restvswalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_restvswalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 


%% All transitions vs walk. 
% A first-pass comparison to see if this is viable at all?
% Motorized
comparisons(counter).name = 'motorized_walkvstransitions';
comparisons(counter).variablesToUse = {'transition_or_not_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_transitionsvswalk';
comparisons(counter).variablesToUse = {'transition_or_not_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Start vs stop categorical
% (Not the betas of the continuous -- that will happen later)

% Motorized
comparisons(counter).name = 'motorized_startvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_startvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'stop');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Accel vs decel categorical.
% Motorized
comparisons(counter).name = 'motorized_accelvsdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% start vs accel categorical 

% Motorized
comparisons(counter).name = 'motorized_startvsaccel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'accel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% stop vs decel categorical 
% Motorized
comparisons(counter).name = 'motorized_stopvsdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'stop') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Walk vs Finished categorical
% Motorized
comparisons(counter).name = 'motorized_walkvsfinished_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'finished');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Rest vs finished stop categorical 

% Motorized
comparisons(counter).name = 'motorized_restvsfinishedstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_restvsfinishedstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};


% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;


%% Finished vs all active transitions categorical. 
% Motorized
comparisons(counter).name = 'motorized_transitionsvsfinished';
comparisons(counter).variablesToUse = {'transition_or_not_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Rests, motorized vs spon.
comparisons(counter).name = 'rest_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Walks, motorized vs spon.
comparisons(counter).name = 'walk_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Starts, motorized vs spon
comparisons(counter).name = 'start_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Stops, motorized vs spon
comparisons(counter).name = 'stop_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'stop');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Finished stops, motorized vs spon
comparisons(counter).name = 'finished_stop_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Save 
save(filename_out, 'comparisons');