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

%% Assign "types"
% make no change warnings into regular warnings.

%%
% Continuous:
% (normal transitions, continued have already been done)

% Motor periods:
% Maintaining rest: pupil diameter
% Maintaining walk: speed, pupil diameter
% start no warning: speed, accel, duration, pupil diamter
% stop no warning: speed, accel, duration, pupil diamter
% accel no warning: speed, accel, duration, pupil diamter
% decel no warning: speed, accel, duration, pupil diamter

% Warning periods (now includes the no change warnings here, too):
% Maintaining rest: pupil diameter
% Maintaining walk: speed, pupil diameter
% start: pupil diameter
% stop: speed, pupil diamter
% accel: speed, pupil diamter
% decel: speed, pupil diameter

% Save 

%% 
% Categorical

%% Walking warnings against walk warning maintaining
% maintain vs stop
% maint vs accel
% maint vs decel

%% Rest (start) warning vs rest warning maintaining
% maint vs start

%% Walk maintaining vs walk m_p no change
% maint vs stop
% maint vs accel
% maint vs decel

%% Rest maintaining vs rest m_p no change
% maint vs start

%% Different (walk) transition warnings
% See if there's any clear indication the mouse can tell the difference.
% stop vs accel
% stop vs decel
% accel vs decel

%% Warned transitions vs not-warned transitions.
% start 
% stop
% accel
% decel

%% Save 
save([parameters.dir_exper 'PLSR\comparisons_level1_warningPeriods_categorical.mat'], 'comparisons');

%% Second-level (within mice)
% all walk maintaining warnings vs walk transition warnings
% all walk warned transitions vs not-warned transitions (stop, accel,
% decel)

% Save

%% Third-level (sort of) (within mice) 
% rest warnings (stop) vs all walk warnings (they're differences from maintain
% warnings)
% rest nochange (stop) vs all walk nochange
% rest unwarned (stop) vs all walk unwarned

% Save

