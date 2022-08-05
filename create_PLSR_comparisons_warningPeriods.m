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
load([parameters.dir_exper 'PLSR\periods_nametable.mat'], 'periods');

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

%% Assign by "types"
%

% First motorized
types = {'w', 'stop', 'accel', 'decel'};

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

%%
% Continuous:
% (normal transitions, continued have already been done)

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

% Spontaneous prewalk: duration, pupil diameter

% Save 
save([parameters.dir_exper 'PLSR\comparisons_warningPeriods_continuous.mat'], 'comparisons');

%% 
% Categorical

%% Maintains vs normal
% walk vs walk_wmaint.
% rest vs rest_wmaint.


%% Warnings against maint.
% wstart vs rest_wmaint
% wstop vs walk_wmaint
% waccel vs walk_wmaint
% wdecel vs walk_wmaint



%% Save 
save([parameters.dir_exper 'PLSR\comparisons_warningPeriods_categorical.mat'], 'comparisons');


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
