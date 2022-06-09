% create_PLSR_comparisons.m
% Sarah West
% 6/9/22

% A script that creates a structure of information to run comparisons
% between different subsets of data for the Random Motorized Treadmill
% experiments. Each comparison will be run per mouse, then averaged across
% mice. 

% (This is for first-level PLSR comparisons);

% 3 main pieces for each comparison:
% name --> the working name of the comparison, for file names.
% variablesToUse --> the response variable categories you're interested in
% indices --> the period indices from periods_nametable that are relevant
% to this comparison & will find the related brain data & response
% variable pairs. 

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

% can search tables like this:
% g = periods_table(string(periods_table.condition)=="m_accel" & string(periods_table.speed)=="2000", :);

counter = 0; 

%% Transitions -- continuous variables per type.**
% All continuous variables. 

% First motorized
types = {'start', 'stop', 'accel', 'decel'};

for typei = 1:numel(types)
    comparisons(counter + typei).name = ['motorized_transitions_continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = continuous_variable_names;

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});

    % Get intersection of motorized & type. 
    comparisons(counter + typei).indices = find(indices_motorized & indices_type);

end

counter = counter + typei; 

% Now spontaneous.

types = {'start', 'stop'};

for typei = 1:numel(types)
    comparisons(counter + typei).name = ['spontaneous_transitions_continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = continuous_variable_names;

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});

    % Get intersection of spontaneous & type. 
    comparisons(counter + typei).indices = find(indices_spontaneous & indices_type);

end

counter = counter + typei; 

%% Continuous variables, Finished periods (not stopping)
% Use speed & duration. 

% Only relevant for motorized.
comparisons(counter + 1).name = ['motorized_finished_continuousVars'];
comparisons(counter + 1).variablesToUse = continuous_variable_names;

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Continuous variables, finished stop periods. 

% Motorized
comparisons(counter + 1).name = ['motorized_finished_stop_continuousVars'];
comparisons(counter + 1).variablesToUse = {'speed_vector', 'duration_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = ['spontaneous_finished_stop_continuousVars'];
comparisons(counter + 1).variablesToUse = {'speed_vector', 'duration_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% 
% From here down is categorical comparisons. 

%% All types 
% No continuous variables in here yet. 

% Motorized
comparisons(counter + 1).name = 'motorized_alltypes';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type. (all the motorized)
comparisons(counter + 1).indices = find(indices_motorized);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_alltypes';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type. (all the spontaneous)
comparisons(counter + 1).indices = find(indices_spontaneous);

counter = counter + 1; 


%% Rest vs walk 
% Motorized
comparisons(counter + 1).name = ['motorized_restvswalk'];
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = ['spontaneous_restvswalk'];
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'rest');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% All transitions vs walk. 
% Motorized
comparisons(counter + 1).name = 'motorized_walkvstransitions';
comparisons(counter + 1).variablesToUse = {'transition_or_not_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_transitionsvswalk';
comparisons(counter + 1).variablesToUse = {'transition_or_not_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Start vs stop categorical
% (Not the betas of the continuous -- that will happen later)

% Motorized
comparisons(counter + 1).name = 'motorized_startvsstop_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'stop');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_startvsstop_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'stop');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Accel vs decel categorical.
% Motorized
comparisons(counter + 1).name = ['motorized_accelvsdecel_categorical'];
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% start vs accel categorical 

% Motorized
comparisons(counter + 1).name = 'motorized_startvsaccel_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'start') | strcmp(period_types, 'accel');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% stop vs decel categorical 
% Motorized
comparisons(counter + 1).name = 'motorized_stopvsdecel_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'stop') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Walk vs Finished categorical
% Motorized
comparisons(counter + 1).name = 'motorized_walkvsfinished_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk') | strcmp(period_types, 'finished');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 


%% Rest vs finished stop categorical 

% Motorized
comparisons(counter + 1).name = 'motorized_restvsfinishedstop_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_restvsfinishedstop_categorical';
comparisons(counter + 1).variablesToUse = {'type_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest') | strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;


%% Finished vs all active transitions categorical. 
% Motorized
comparisons(counter + 1).name = 'motorized_transitionsvsfinished';
comparisons(counter + 1).variablesToUse = {'transition_or_not_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished') | strcmp(period_types, 'start') ...
    | strcmp(period_types, 'stop') | strcmp(period_types, 'accel') | strcmp(period_types, 'decel');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Rests, motorized vs spon.
comparisons(counter + 1).name = 'rest_motorizedvsspon_categorical';
comparisons(counter + 1).variablesToUse = {'motorized_vs_spon_dummyvars_vector'};

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

comparisons(counter + 1).indices = find(indices_type);

counter = counter + 1;

%% Save 
save([parameters.dir_exper 'PLSR\comparisons_firstlevel.mat'], 'comparisons');