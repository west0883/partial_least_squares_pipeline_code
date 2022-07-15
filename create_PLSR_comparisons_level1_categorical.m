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
% Will also have a "figure_type" identifier, which is for determining the
% color range of final figures. Can be "continued", "startstop", or
% "acceldecel".

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
variablesToSubtract.finished_start = {'speed_vector', 'duration_vector'};
variablesToSubtract.finished_stop = {'duration_vector'};
variablesToSubtract.finished_accel = {'speed_vector', 'duration_vector'};
variablesToSubtract.finished_decel = {'speed_vector', 'duration_vector'};

save(filename_out_variablesToSubtract, 'variablesToSubtract');

%% Rest vs walk 
% Motorized
comparisons(counter).name = 'motorized_restvswalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_restvswalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Start vs stop categorical
% (Not the betas of the continuous -- that will happen later)

% Motorized
comparisons(counter).name = 'motorized_startvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_startvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

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
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'acceldecel';

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
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'acceldecel';

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
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'stop') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 


%% Rest vs finished stop categorical 

% Motorized
comparisons(counter).name = 'motorized_restvsfinishedstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_restvsfinishedstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;

%% Rests, motorized vs spon.
comparisons(counter).name = 'rest_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Walks, motorized vs spon.
comparisons(counter).name = 'walk_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Starts, motorized vs spon
comparisons(counter).name = 'start_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Stops, motorized vs spon
comparisons(counter).name = 'stop_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'stop');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Finished stops, motorized vs spon
comparisons(counter).name = 'finished_stop_motorizedvsspon_categorical';
comparisons(counter).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

comparisons(counter).indices = find(indices_type);

counter = counter + 1;

%% Motorized walk vs accel
comparisons(counter).name = 'motorized_walkvsaccel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'accel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Motorized walk vs decel 
comparisons(counter).name = 'motorized_walkvsdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%%  walk vs start
% motorized
comparisons(counter).name = 'motorized_walkvsstart_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

% spontaneous
comparisons(counter).name = 'spontaneous_walkvsstart_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% walk vs stop 
% motorized
comparisons(counter).name = 'motorized_walkvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

% spontaneous
comparisons(counter).name = 'spontaneous_walkvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'stop');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% rest vs start 
% motorized
comparisons(counter).name = 'motorized_restvsstart_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

% spontaneous
comparisons(counter).name = 'spontaneous_restvsstart_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'start');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% rest vs stop 
% motorized
comparisons(counter).name = 'motorized_restvsstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

% spontaneous
comparisons(counter).name = 'spontaneous_restvstop_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'stop');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Accel vs finished accel 
% motorized
comparisons(counter).name = 'motorized_accelvsfaccel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'accel') | strcmp(period_types, 'finished_accel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Decel vs finished decel    
% motorized
comparisons(counter).name = 'motorized_decelvsfdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'decel') | strcmp(period_types, 'finished_decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Walk vs finished accel
% motorized
comparisons(counter).name = 'motorized_walkvsfaccel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'finished_accel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Walk vs finished decel 
comparisons(counter).name = 'motorized_walkvsfdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'finished_decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Finished accel vs finished decel
comparisons(counter).name = 'motorized_faccelvsfdecel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_accel') | strcmp(period_types, 'finished_decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Finished accel vs finished start
comparisons(counter).name = 'motorized_fstartvsfaccel_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_accel') | strcmp(period_types, 'finished_start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;
%% Start vs finished start 
comparisons(counter).name = 'motorized_startvsfstart_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = -1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'finished_start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Finished start vs walk
comparisons(counter).name = 'motorized_fstartvswalk_categorical';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'finished_start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Save 
save(filename_out, 'comparisons');