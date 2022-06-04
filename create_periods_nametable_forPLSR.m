% create_periods_nametable_forPLSR.m
% Sarah West
% 6/4/22

% Script that organizes periods_nametable for easy creation of response
% variables for partial least squares regression for Random Motorized
% Treadmill experiment.

clear all; 

% Create the experiment name.
parameters.experiment_name='Random Motorized Treadmill';

% Output directory name bases
parameters.dir_base='Y:\Sarah\Analysis\Experiments\';
parameters.dir_exper=[parameters.dir_base parameters.experiment_name '\']; 


% Load names of motorized periods
load([parameters.dir_exper 'periods_nametable.mat']);
periods_motorized = periods;

% Load names of spontaneous periods
load([parameters.dir_exper 'periods_nametable_spontaneous.mat']);
periods_spontaneous = periods;
clear periods; 

% Create a shared motorized & spontaneous list.
periods_bothConditions = [periods_motorized; periods_spontaneous]; 

% Make a partial least squares regression folder. 
if ~isdir([parameters.dir_exper 'PLSR\'])
    mkdir([parameters.dir_exper 'PLSR\']);
end

%% Make a list of dummy variable categories for using later. 
% What to put the creation of this here in this script to keep it with the
% other organizing period_nametable code.
categories.motorized_vs_spon = {'motorized', 'spontaneous'};
categories.type = {'rest', 'walk', 'prep', 'start', 'stop', 'accel', 'decel', 'finished'};

save([parameters.dir_exper 'PLSR\response_categories.mat'], 'categories');

%% Create labels for "spontaneous" vs "motorized"

%% Keep only the set of periods you want to use for PLS regression 
% For first-pass of PLSR, don't use probe trials, maintaining, or full onset/offset. 
% Save the indices you removed, too, so you can easily remove them from
% correlation data variables later. 


%% Label by "super-type"
% prep, start, stop, accel, decel, rest, walk, or finished. For 
% spontaneous: prewalk = prep, startwalk = start, stopwalk = stop, postwalk 
% = finished.

%% If a motorized prep, take off the first 2 seconds of "duration"
% Save the indices of the motorized prep, so you can easily find them &
% remove data from correlation data variables later. 

%% Calculate new "roll numbers" for periods & duration you have left. 

%% Create "instantaneous" duration vectors 
% Based on duration, roll numbers for all periods.

%% Convert the rest & walk periods' duration vectors to just the average duration of motorized walk.
% (the average duration from actual Arduino code)

%% For motorized, calculate "instantaneous" speed 
% For certain periods, based on speed, previous speed, accel. (or duration,
% roll number, speed, accel)/ Leave spontaneous blank for now. 


%% Put in accel = 0 for prep, finished, motorized rest, motorized walk periods. 
% Leave spontaneous non-continued blank ('Nan') for now. 


%% Save 

