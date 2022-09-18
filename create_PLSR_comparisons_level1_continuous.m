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
continuous_variable_names = {'speed_vector', 'accel_vector', 'duration_vector', 'pupil_diameter_vector'};

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
    comparisons(counter + typei).mice_not_to_use = {};
    
    if typei == 1 || typei == 2
        comparisons(counter + typei).figure_type = 'startstop';
    else
        comparisons(counter + typei).figure_type = 'acceldecel'; 
    end

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
    comparisons(counter +typei).mice_not_to_use = {'1100'};
    comparisons(counter + typei).figure_type = 'startstop';

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});

    % Get intersection of spontaneous & type. 
    comparisons(counter + typei).indices = find(indices_spontaneous & indices_type);

end

counter = counter + typei + 1; 

%% Continuous variables, finished stop periods. 

% Motorized
comparisons(counter).name = 'motorized_finished_stop_continuousVars';
comparisons(counter).variablesToUse = {'duration_vector', 'pupil_diameter_vector'};
comparisons(counter).type = 'finished_stop';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_finished_stop_continuousVars';
comparisons(counter).variablesToUse = {'duration_vector', 'pupil_diameter_vector', 'speed_vector', 'accel_vector'};
comparisons(counter).type = 'finished_stop';
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_stop');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1; 

%% Motorized walk.
% Motorized
comparisons(counter).name = 'motorized_walk_continuousVars';
comparisons(counter).variablesToUse = {'speed_vector','pupil_diameter_vector' };
comparisons(counter).type = 'walk';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Spontaneous walk.
% Include accel as well.
comparisons(counter).name = 'spontaneous_walk_continuousVars';
comparisons(counter).variablesToUse = {'speed_vector', 'accel_vector', 'pupil_diameter_vector'};
comparisons(counter).type = 'walk';
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;

%% Continuous variables, rest
% Motorized
comparisons(counter).name = 'motorized_rest_continuousVars';
comparisons(counter).variablesToUse = {'pupil_diameter_vector' };
comparisons(counter).type = 'rest';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

% Spontaneous
comparisons(counter).name = 'spontaneous_rest_continuousVars';
comparisons(counter).variablesToUse = {'pupil_diameter_vector'};
comparisons(counter).type = 'rest';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'continued';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest');

% Get intersection of spontaneous & type. 
comparisons(counter).indices = find(indices_spontaneous & indices_type);

counter = counter + 1;

%% Continuous variables, finished_start 
% Use speed & duration. 

% Only relevant for motorized.
comparisons(counter).name = 'motorized_finished_start_continuousVars';
comparisons(counter).variablesToUse = {'speed_vector', 'duration_vector', 'pupil_diameter_vector'};
comparisons(counter).type = 'finished_start';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'startstop';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_start');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1; 

%% Continuous variables, finished_accel
% Use speed & duration. 

% Only relevant for motorized.
comparisons(counter).name = 'motorized_finished_accel_continuousVars';
comparisons(counter).variablesToUse = {'speed_vector', 'duration_vector', 'pupil_diameter_vector'};
comparisons(counter).type = 'finished_accel';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'finished_accel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Continuous variables, finished_decel
% Use speed & duration. 

% Only relevant for motorized.
comparisons(counter).name = 'motorized_finished_decel_continuousVars';
comparisons(counter).variablesToUse = {'speed_vector', 'duration_vector', 'pupil_diameter_vector'};
comparisons(counter).type = 'finished_decel';
comparisons(counter).mice_not_to_use = {};
comparisons(counter).figure_type = 'acceldecel';

% Get relevent indices for this ty pe.
indices_type = strcmp(period_types, 'finished_decel');

% Get intersection of motorized & type. 
comparisons(counter).indices = find(indices_motorized & indices_type);

counter = counter + 1;

%% Save 
save([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat'], 'comparisons');