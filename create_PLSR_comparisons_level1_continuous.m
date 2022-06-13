% create_PLSR_comparisons_level1_continuous.m
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
    comparisons(counter + typei).type = types{typei};

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
    comparisons(counter + typei).type = types{typei};

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});

    % Get intersection of spontaneous & type. 
    comparisons(counter + typei).indices = find(indices_spontaneous & indices_type);

end

counter = counter + typei; 

%% Continuous variables, Finished periods (not stopping)
% Use speed & duration. 

% Only relevant for motorized.
comparisons(counter + 1).name = 'motorized_finished_continuousVars';
comparisons(counter + 1).variablesToUse = {'speed_vector', 'duration_vector'};
comparisons(counter + 1).type = 'finished';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Continuous variables, finished stop periods. 

% Motorized
comparisons(counter + 1).name = 'motorized_finished_stop_continuousVars';
comparisons(counter + 1).variablesToUse = {'duration_vector'};
comparisons(counter + 1).type = 'finished_stop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_finished_stop_continuousVars';
comparisons(counter + 1).variablesToUse = {'duration_vector'};
comparisons(counter + 1).type = 'finished_stop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Continuous variables, walk with speed
% Motorized
comparisons(counter + 1).name = 'motorized_walk_continuousVars';
comparisons(counter + 1).variablesToUse = {'speed_vector'};
comparisons(counter + 1).type = 'walk';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

% Get intersection of motorized & type. 
comparisons(counter + 1).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter + 1).name = 'spontaneous_walk_continuousVars';
comparisons(counter + 1).variablesToUse = {'speed_vector'};
comparisons(counter + 1).type = 'walk';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

% Get intersection of spontaneous & type. 
comparisons(counter + 1).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;


%% Save 
save([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat'], 'comparisons');