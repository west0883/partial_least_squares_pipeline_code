% create_periods_nametable_forPLSR_warningPeriods.m
% Sarah West
% 6/29/22

% Script that organizes periods_nametable for easy creation of response
% variables for partial least squares regression for Random Motorized
% Treadmill experiment. 

% This focuses on warning/preparation periods.

clear all; 

% Create the experiment name.
parameters.experiment_name='Random Motorized Treadmill';

% Output directory name bases
parameters.dir_base='Y:\Sarah\Analysis\Experiments\';
parameters.dir_exper=[parameters.dir_base parameters.experiment_name '\']; 

% Load mice_all, pass into parameters structure
load([parameters.dir_exper '\mice_all.mat']);
parameters.mice_all = mice_all;

% ****Change here if there are specific mice, days, and/or stacks you want to work with**** 
parameters.mice_all = parameters.mice_all;

% Load names of motorized periods
load([parameters.dir_exper 'periods_nametable.mat']);
periods_motorized = periods;

% Load names of spontaneous periods
load([parameters.dir_exper 'periods_nametable_spontaneous.mat']);
periods_spontaneous = periods(1:6, :);
clear periods; 

% Create a shared motorized & spontaneous list to edit as your new
% nametable.
periods = [periods_motorized; periods_spontaneous]; 

fps = 20;
window_size = 20;
window_step_size = 5; 

% Make a partial least squares regression folder. 
if ~isdir([parameters.dir_exper 'PLSR Warning Periods\'])
    mkdir([parameters.dir_exper 'PLSR Warning Periods\']);
end

%% Make a list of dummy variable categories for using later. 
% What to put the creation of this here in this script to keep it with the
% other organizing period_nametable code.
% No prep for now.
% categories.motorized_vs_spon = {'motorized', 'spontaneous'};
% categories.type = {'rest', 'walk', 'start', 'stop', 'accel', 'decel', 'finished', 'finished_stop'}; % 'prep'
% 
% save([parameters.dir_exper 'PLSR\response_categories.mat'], 'categories');

%% Make values for speed, accel into numerics. 
speeds = periods{:,'speed'};
periods.speed = cellfun(@str2num, speeds, 'UniformOutput', false); 

accels = periods{:,'accel'};
periods.accel = cellfun(@str2num, accels, 'UniformOutput', false); 

clear speeds accels; 

%% Make all speeds & accels into cm/s 
% Only values in here so far are motorized, so can apply to all speed
% values.
original_speeds = [1600, 2000, 2400, 2800];
cms_speeds = [2.25, 2.77, 3.33, 3.88];
conversion_factor = round(mean(original_speeds./cms_speeds),-1);

speeds = periods{:,'speed'};
periods.speed = cellfun(@(x) x/conversion_factor , speeds, 'UniformOutput', false); 

accels = periods{:,'accel'};
periods.accel = cellfun(@(x) x/conversion_factor, accels, 'UniformOutput', false); 

%% Remove behavior fields you don't need.
% previous speed, two speeds ago, previous accel.
columns_to_remove = {'previous_speed', 'previous_accel', 'two_speeds_ago'};
for coli = 1:numel(columns_to_remove)
    periods.(columns_to_remove{coli}) = [];
end

clear columns_to_remove; 

%% Create labels for "spontaneous" vs "motorized"
% % Do this before removing periods you don't need so you can use size of
% % periods_motorized and periods_spontaneous.
% motorized_vs_spon = cell(size(periods,1),1);
% motorized_vs_spon([1:size(periods_motorized,1)]) = repmat({'motorized'}, size(periods_motorized,1) , 1);
% motorized_vs_spon([size(periods_motorized,1) + 1:end]) = repmat({'spontaneous'}, size(periods_spontaneous,1) , 1);
% 
% periods.motorized_vs_spon = motorized_vs_spon;
% clear motorized_vs_spon;

%% Keep only the set of periods you want to use for PLS regression 
% For first-pass of PLSR, don't use probe trials, maintaining, or full onset/offset. 
% Save the indices you removed, too, so you can easily remove them from
% correlation data variables later. 
% DON'T re-number the index field--> that would make it very hard to find
% the corresponding velocities and accels later. 

% Remove finished periods, probes, transitions, most spontaneous periods.
conditions_to_remove = {'m_f', 'w_p_', 'm_p_', 'm_maint' 'm_start', 'm_stop', 'm_accel', 'm_decel', 'startwalk', 'stopwalk', 'postwalk', 'full_onset', 'full_offset'};

% Also include some of the meaningless ones.
indices_to_remove = [71; 76; 80; 81; 82; 134; 139; 143;144; 145; [149:153]'; 175]; 

for i = 1:size(periods,1)
        
    if contains(cell2mat(periods{i, 'condition'}), conditions_to_remove)
        indices_to_remove = [indices_to_remove; i];
    end
 
end 

% Specifically throw out spontaneous walk.
indices_to_remove = [indices_to_remove; 190];

periods(indices_to_remove, :) = [];
save([parameters.dir_exper 'PLSR Warning Periods\indices_to_remove_warningPeriods.mat'], 'indices_to_remove');

%% Separate maintaining at rest vs at walk, add "type"

look_for = {'c_rest', 'rest', 'c_walk', 'w_maint', ...
            'w_start','w_stop', 'w_accel', 'w_decel', 'prewalk'};

% Holder for types.        
types = cell(size(periods,1), 1);

for i = 1:size(periods, 1)
   
    this_condition = periods{i, 'condition'}{1};
    type = find(strcmp(look_for, this_condition));

    switch type

        case 1   % motorized rest
            types{i} = 'motorized_rest';


        case 2 % spontaneous rest
            types{i} = 'spontaneous_rest';
    
        case 3 % motorized walk
            types{i} = 'motorized_walk';

        case 4 % maintaining

             % split into rest or walk versions, based on speed.
            % Rest version
            if periods.speed{i} == 0
              types{i} = 'rest_wmaint';
            % Walk versions.
            else
              types{i} = 'walk_wmaint';
            end

        case 5
             types{i} = 'wstart';

        case 6
             types{i} = 'wstop';

        case 7
             types{i} = 'waccel';

        case 8
             types{i} = 'wdecel';

        case 9
             types{i} = 'prewalk';
    end
end
periods.type = types; 
clear types type type1 type2 look_for_pattern look_for this_condition;

%% Make column for pupil diameter 
% All periods. Are all NaN for now, will be put in at "populate response
% variables" step in main pipline.
pupil_diameter = repmat({NaN}, size(periods,1),1);
periods.pupil_diameter = pupil_diameter;

clear pupil_diameter;

%% Put in accel = 0 for all.
accels = repmat({0}, size(periods,1), 1);
periods.accel = accels;
% for i = 1:size(periods,1)
% 
%     type1 = strcmp(look_for_noAccel, periods{i, 'type'});
% 
%     % If this type is any of the no accelration types,
%     if any(type1)
%         % Make acceleration 0.
%        periods{i, 'accel'} = {0};      
%     end
% end

clear accels;
%% Make speed = 0 for any speeds that are still NaNs.
for i = 1:size(periods,1)
    
    if isnan(periods{i, 'speed'}{1})
           periods{i, 'speed'} = {0} ;    
    end
end

clear i;

%% If a warning/prep period, take off the first 2 seconds of "duration"
% Save the indices of the motorized prep, so you can easily find them &
% remove data from correlation data variables later. 

look_for_shorten = {'w_'};

indices_to_shorten = [];
indices_to_shorten_original_index = []; 
for i = 1:size(periods,1)

    type = cellfun(@contains, periods{i, 'condition'}, look_for_shorten);

    if type
        periods{i, 'duration'} = {periods{i, 'duration'}{1} - fps *2};
        indices_to_shorten = [indices_to_shorten; i];
        indices_to_shorten_original_index = [indices_to_shorten_original_index; periods{i, 'index'}];
    end

end
save([parameters.dir_exper 'PLSR Warning Periods\indices_to_shorten.mat'], 'indices_to_shorten', 'indices_to_shorten_original_index');

%% Calculate new "roll numbers" for periods & duration you have left. 
% Figure out how many rolls you can get out of it (should be a whole number). 
number_of_rolls = cell(size(periods,1), 1);
for i = 1:size(periods,1)
   number_of_rolls{i} = CountRolls(periods{i, 'duration'}{1}, window_size, window_step_size);
end
periods.number_of_rolls = number_of_rolls;

%% Create "instantaneous" duration vectors 
% Based on roll numbers for all periods.
% The time of a given roll is the center of the roll window.
duration_vector = cellfun(@(x) ([1:x] + 1) * (window_step_size/fps) , number_of_rolls, 'UniformOutput', false);
periods.duration_vector = duration_vector;

%% Replicate speed, accels, & pupil diameter by roll number.
speed_vector =  cell(size(periods,1),1);
accel_vector = cell(size(periods,1),1);
pupil_diameter_vector = cell(size(periods,1),1);
for i = 1:size(periods,1)
    speed_vector(i) = {repmat(periods{i, 'speed'}{1}, 1, periods{i,'number_of_rolls'}{1})};
    accel_vector(i) = {repmat(periods{i, 'accel'}{1}, 1, periods{i,'number_of_rolls'}{1})};
    pupil_diameter_vector(i) = {repmat(periods{i, 'pupil_diameter'}{1}, 1, periods{i,'number_of_rolls'}{1})};
end 
periods.speed_vector = speed_vector;
periods.accel_vector = accel_vector;
periods.pupil_diameter_vector = pupil_diameter_vector;

%% Make a column that says if this period is one of the walk "active" warnings 
% Is for later when you want to compare all transitions to other things. 
walk_active_warning = cell(size(periods, 1), 1); 
for i = 1:size(periods,1)

    if strcmp(periods{i, 'type'}{1}, 'wstop') || strcmp(periods{i, 'type'}{1}, 'waccel') || strcmp(periods{i, 'type'}{1}, 'wdecel')
        walk_active_warning{i} = 'walk_active_warning';
    else 
        walk_active_warning{i} = 'not_walk_active_warning';
    end
    
end
periods.walk_active_warning = walk_active_warning; 


%% Create dummy variables for categoricals --  type & walk active warnings or not

categories.type = {'waccel', 'wdecel', 'wstop', 'walk_wmaint', 'motorized_walk', 'wstart', 'prewalk', 'rest_wmaint', 'motorized_rest', 'spontaneous_rest'};
type_dummyvars = cell(size(periods,1),1);
walk_active_warning_dummyvars = cell(size(periods,1),1);

for i = 1:size(periods,1)

    % Type 
    type = strcmp(repmat(periods{i, 'type'}, size(categories.type)), categories.type);
    type_dummyvars{i} = double(type)';

    % Walk active warnings.
    if strcmp(periods{i, 'walk_active_warning'}{1}, 'walk_active_warning')

        walk_active_warning_dummyvars{i} = [1; 0];

    else

        walk_active_warning_dummyvars{i} = [0; 1];

    end

end 

periods.type_dummyvars = type_dummyvars;
periods.walk_active_warning_dummyvars = walk_active_warning_dummyvars;

%% Replicate dummy vars by roll number. 

type_dummyvars_vector = cell(size(periods,1),1);
walk_active_warning_dummyvars_vector = cell(size(periods,1),1);

for i = 1:size(periods,1)

    % Type 
    type_dummyvars_vector{i} = repmat(periods{i, 'type_dummyvars'}{1}, [1 number_of_rolls{i}]);

    % Walk active warnings
    walk_active_warning_dummyvars_vector{i} = repmat(periods{i, 'walk_active_warning_dummyvars'}{1}, [1 number_of_rolls{i}]);

end 

periods.type_dummyvars_vector = type_dummyvars_vector;
periods.walk_active_warning_dummyvars_vector = walk_active_warning_dummyvars_vector;

%% Save 
save([parameters.dir_exper 'PLSR Warning Periods\periods_nametable_forPLSR_warningPeriods.mat'], 'periods', '-v7.3');

%clear all;