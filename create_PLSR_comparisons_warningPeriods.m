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
load([parameters.dir_exper 'PLSR Warning Periods\periods_nametable_forPLSR_warningPeriods.mat'], 'periods');

% Continuous variable names
continuous_variable_names = {'speed_vector', 'accel_vector', 'duration_vector', 'pupil_diameter_vector'};

% Pull out the relevant columns/info from periods
period_types = periods.type;

% can search tables like this:
% g = periods_table(string(periods_table.condition)=="m_accel" & string(periods_table.speed)=="2000", :);

counter = 0; 

%% CONTINUOUS VARIABLES
% Continuous:
% Do motorized continued walk & both rests for simplicity. 

% Maintaining periods (?):
% Maintaining rest: pupil diameter
% Maintaining walk: speed, pupil diameter

% Warning periods: 
% Maintaining rest: pupil diameter
% Maintaining walk: speed, pupil diameter
% start: pupil diameter
% stop: speed, pupil diamter
% accel: speed, pupil diamter
% decel: speed, pupil diameter

% walking 
types = {'wstop', 'waccel', 'wdecel', 'walk_wmaint'};

for typei = 1:numel(types)
    comparisons(counter + typei).name = ['continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = {'speed_vector', 'duration_vector', 'pupil_diameter_vector'};
    comparisons(counter + typei).type = types{typei};
    comparisons(counter + typei).mice_not_to_use = {};
    comparisons(counter + typei).figure_type = 'warningPeriods';

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});

    comparisons(counter + typei).indices = find(indices_type);

end

counter = counter + typei; 

% rest (not continued)
types = {'wstart', 'rest_wmaint', 'prewalk'};

for typei = 1:numel(types)
    comparisons(counter + typei).name = ['continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = {'duration_vector', 'pupil_diameter_vector'};
    comparisons(counter + typei).type = types{typei};

    % Don't use mouse 1100 in prewalk.
    if strcmp(types{typei}, 'prewalk')
        comparisons(counter + typei).mice_not_to_use = {'1100'};
    else
        comparisons(counter + typei).mice_not_to_use = {};
    end

    comparisons(counter + typei).figure_type = 'warningPeriods';

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});
    comparisons(counter + typei).indices = find(indices_type);

end

counter = counter + typei; 


% continued rest 
types = {'motorized_rest', 'spontaneous_rest'};
for typei = 1:numel(types)
    comparisons(counter + typei).name = ['continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = {'pupil_diameter_vector'};
    comparisons(counter + typei).type = types{typei};
    comparisons(counter + typei).mice_not_to_use = {};
    comparisons(counter + typei).figure_type = 'warningPeriods';

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});
    comparisons(counter + typei).indices = find(indices_type);

end

counter = counter + typei; 

% continued walk
types = {'motorized_walk'};
for typei = 1:numel(types)
    comparisons(counter + typei).name = ['continuousVars_' types{typei}];
    comparisons(counter + typei).variablesToUse = {'speed_vector', 'pupil_diameter_vector'};
    comparisons(counter + typei).type = types{typei};
    comparisons(counter + typei).mice_not_to_use = {};
    comparisons(counter + typei).figure_type = 'warningPeriods';

    % Get relevent indices for this type.
    indices_type = strcmp(period_types, types{typei});
    comparisons(counter + typei).indices = find(indices_type);

end

counter = counter + typei; 

% Save 
save([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_continuous.mat'], 'comparisons');

%%  Categorical
comparisons_continuous = comparisons; 
clear comparisons counter;
counter = 1 ;

%% Maintains vs normal
% walk vs walk_wmaint.
comparisons(counter).name = 'wmaintvswalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk_wmaint') | strcmp(period_types, 'motorized_walk');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

% rest vs rest_wmaint.
comparisons(counter).name = 'wmaintvsrest';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest_wmaint') | strcmp(period_types, 'motorized_rest');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

%% Warnings against maint.
% wstart vs rest_wmaint
comparisons(counter).name = 'wstartvswmaint';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'rest_wmaint') | strcmp(period_types, 'wstart');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

% wstop vs walk_wmaint
comparisons(counter).name = 'wstopvswmaint';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk_wmaint') | strcmp(period_types, 'wstop');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

% waccel vs walk_wmaint
comparisons(counter).name = 'waccelvswmaint';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk_wmaint') | strcmp(period_types, 'waccel');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

% wdecel vs walk_wmaint
comparisons(counter).name = 'wdecelvswmaint';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'walk_wmaint') | strcmp(period_types, 'wdecel');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

%% Prewalk
% prewalk vs spontaneous rest
comparisons(counter).name = 'prewalkvsrest';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'prewalk') | strcmp(period_types, 'spontaneous_rest');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

% wstart vs prewalk
comparisons(counter).name = 'wstartvsprewalk';
comparisons(counter).variablesToUse = {'type_dummyvars_vector'};
comparisons(counter).mice_not_to_use = {'1100'};
comparisons(counter).plotMultiplier = 1;
comparisons(counter).figure_type = 'warningPeriods';

% Get relevent indices for this type.
indices_type = strcmp(period_types, 'prewalk') | strcmp(period_types, 'wstart');
comparisons(counter).indices = find(indices_type);

counter = counter + 1; 

%% Save 
save([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_categorical.mat'], 'comparisons');


%% Probably won't do:

% Walk maintaining vs walk m_p no change
% maint vs stop
% maint vs accel
% maint vs decel

% Rest maintaining vs rest m_p no change
% maint vs start

% Different (walk) transition warnings
% See if there's any clear indication the mouse can tell the difference.
% stop vs accel
% stop vs decel
% accel vs decel

% Warned transitions vs not-warned transitions.
% start 
% stop
% accel
% decel
