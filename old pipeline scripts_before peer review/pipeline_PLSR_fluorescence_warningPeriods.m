% pipeline_PLSR_fluorescence_warningPeriods.m
% Sarah West
% 9/29/22

% Pipeline of PLSR on fluorescence traces. 

% Fluorescence timeseries are already segmented, grouped by behavior,
% rolled
% ** WIll need a normalized version of fluorescence--> just do change in
% z-score, which PLSR will calculate (just don't multiply by the orignal
% fluorescence sigma at the end)--> I think that's what Clancy et al 2019
% reports activity as.***

% Initial set-up
%% Initial Setup  
% Put all needed paramters in a structure called "parameters", which you
% can then easily feed into your functions. 
% Use correlations, Fisher transformed, mean removed within mice (mean
% removed for at least the cases when you aren't using mice as response
% variables).

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

% Other parameters
parameters.digitNumber = 2;
parameters.yDim = 256;
parameters.xDim = 256;
parameters.number_of_sources = 16; 
parameters.indices = 1:16;

% Load the motorized/spontaneous list of periods, to fit in with
% correlations pipeline 

% Load names of motorized periods
load([parameters.dir_exper 'periods_nametable.mat']);
periods_motorized = periods;

% Load names of spontaneous periods
load([parameters.dir_exper 'periods_nametable_spontaneous.mat']);
periods_spontaneous = periods(1:6, :);
clear periods; 

% Create a shared motorized & spontaneous list.
periods_bothConditions = [periods_motorized; periods_spontaneous]; 
parameters.periods_bothConditions = periods_bothConditions; 

% Load periods_nametable_PLSR.m, if it exists yet. (Otherwise is created in
% first step).
if isfile([parameters.dir_exper 'PLSR Warning Periods\periods_nametable_forPLSR_warningPeriods.mat'])
    load([parameters.dir_exper 'PLSR Warning Periods\periods_nametable_forPLSR_warningPeriods.mat']);
    parameters.periods = periods;

    % Also load the indices to remove
    load([parameters.dir_exper 'PLSR Warning Periods\indices_to_remove_warningPeriods.mat']);
    parameters.indices_to_remove = indices_to_remove;

    % And indices to shorten.
    load([parameters.dir_exper 'PLSR Warning Periods\indices_to_shorten.mat']);
    parameters.indices_to_shorten = [indices_to_shorten indices_to_shorten_original_index];

    % Load lists of response categories
    load([parameters.dir_exper 'PLSR\response_categories.mat']);
    parameters.loop_variables.response_categories = categories;
    parameters.categories = categories;

    clear periods indices_to_remove categories;

    clear indices_to_remove indices_to_shorten indices_to_shorten_original_index;

end

% Load comparisons for  continuous, if it exists yet.
if isfile([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_continuous.mat'])
    load([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_continuous.mat']);
    parameters.comparisons_continuous = comparisons;
    parameters.loop_variables.comparisons_continuous = parameters.comparisons_continuous; 
    clear comparisons;
end

% Load comparisons for categorical, if it exists yet.
if isfile([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_categorical.mat'])
    load([parameters.dir_exper 'PLSR Warning Periods\comparisons_warningPeriods_categorical.mat']);
    parameters.comparisons_categorical = comparisons;
    parameters.loop_variables.comparisons_categorical = parameters.comparisons_categorical;
    clear comparisons;
end

% Make color ranges for each type of comparison, for final figures.
parameters.color_range.warningPeriods.categorical = [-0.1 0.1];
parameters.color_range.warningPeriods.speed = [-0.1 0.1];
parameters.color_range.warningPeriods.accel = [-0.06 0.06];
parameters.color_range.warningPeriods.duration = [-0.2 0.2];
parameters.color_range.warningPeriods.pupil_diameter = [-0.02 0.02 ];

parameters.color_range.specials = {
                                    'prewalkvsrest', 'categorical', [-0.2 0.2];
                                    'wstartvsprewalk', 'categorical', [-0.2 0.2];
                                    };
                                    
                                    
% Names of all continuous variables.
parameters.continuous_variable_names = {'speed', 'accel', 'duration', 'pupil_diameter','tail', 'nose', 'FL', 'HL'};

% Put relevant variables into loop_variables.
parameters.loop_variables.mice_all = parameters.mice_all;
parameters.loop_variables.periods = parameters.periods.condition; 
parameters.loop_variables.periods_bothConditions = parameters.periods_bothConditions.condition; 
parameters.loop_variables.conditions = {'motorized'; 'spontaneous'};
parameters.loop_variables.conditions_stack_locations = {'stacks'; 'spontaneous'};
parameters.loop_variables.variable_type = {'response variables', 'correlations'};
%parameters.loop_variables.categories.type = parameters.categories.type;
parameters.loop_variables.comparison_types = {'categorical', 'continuous'};

parameters.average_and_std_together = false;

%% *** Run the PLSR pipeline ***

%% Remove correlations for periods you don't want to use, trim warning periods
% From saved indices from creation of response variables .
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Evaluation instructions.
parameters.evaluation_instructions = {{
          'data = parameters.data;'...
          'data(parameters.indices_to_remove) = [];'... 
          'for i = 1:numel(parameters.indices_to_shorten(:,1));'...
               'index = parameters.indices_to_shorten(i,1);'...
               'data_sub = data{index};'...
               'data{index} = data_sub(:, 9:17,:);'...
          'end;'...
          'data_evaluated = data;'
          }};
% Input 
% The reshaped correlations per mouse from fluorescence analysis pipeline.
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR fluorescence\permuted timeseries\'], 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'timeseries_permuted.mat'};
parameters.loop_list.things_to_load.data.variable= {'timeseries_permuted'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.data_evaluated.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_save.data_evaluated.filename= {'values.mat'};
parameters.loop_list.things_to_save.data_evaluated.variable= {'values'}; 
parameters.loop_list.things_to_save.data_evaluated.level = 'mouse';

RunAnalysis({@EvaluateOnData}, parameters); 

%% Put in response variables, no vertical concatenation. Also trim pupil diameter.
% if isfield(parameters, 'loop_list')
% parameters = rmfield(parameters,'loop_list');
% end
% 
% % Iterators
% parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};
% 
% % Variables to replicate
% parameters.response_variable_names = {'type_dummyvars_vector', 'walk_active_warning_dummyvars_vector', 'speed_vector', 'accel_vector', 'duration_vector', 'pupil_diameter_vector', 'tail_vector', 'nose_vector', 'FL_vector', 'HL_vector' };
% parameters.variables_static = {'type_dummyvars_vector', 'duration_vector', 'walk_active_warning_dummyvars_vector'};
% % parameters.motorized_variables_static = {'speed_vector', 'accel_vector'}; % These are the ones that are static in motorized, not static in spontaneous
% 
% % Original order of spontaneous (for velocity & accel indexing)
% parameters.spontaneous_periods_order = {'rest', 'walk', 'prewalk', 'startwalk', 'stopwalk', 'postwalk'};
% 
% parameters.concatenate_vertically = false;
% 
% % Additional variables -- pupil, tail, nose, FL, HL; always present & loaded in
% parameters.additional_variables = parameters.response_variable_names(6:end);
% 
% % Evaluation instructions.
% parameters.evaluation_instructions = {cell(numel(parameters.additional_variables), 1)};
% for variablei = 1:numel(parameters.additional_variables)
%     variable = parameters.additional_variables{variablei};
%     parameters.evaluation_instructions(variablei)= {[
%           'data = parameters.' variable ';'...
%           'for i = 1:numel(parameters.indices_to_shorten(:,2));'...
%                'index = parameters.indices_to_shorten(i,2);'...
%                'data_sub = data{index};'...
%                'data{index} = data_sub(9:17,:);'...
%           'end;'...
%           'data_evaluated = data;']
%           };
% end
% 
% % Input
% % Correlations (for instances count)
% parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
% parameters.loop_list.things_to_load.data.filename= {'values.mat'};
% parameters.loop_list.things_to_load.data.variable= {'values'}; 
% parameters.loop_list.things_to_load.data.level = 'mouse';
% 
% % Spontaneous velocity
% parameters.loop_list.things_to_load.speed_vector.dir = {[parameters.dir_exper 'behavior\spontaneous\rolled concatenated velocity\'], 'mouse', '\'};
% parameters.loop_list.things_to_load.speed_vector.filename= {'velocity_averaged_by_instance.mat'};
% parameters.loop_list.things_to_load.speed_vector.variable= {'velocity_averaged_by_instance'}; 
% parameters.loop_list.things_to_load.speed_vector.level = 'mouse';
% 
% % Spontaneous accel.
% parameters.loop_list.things_to_load.accel_vector.dir = {[parameters.dir_exper 'behavior\spontaneous\rolled concatenated velocity\'], 'mouse', '\'};
% parameters.loop_list.things_to_load.accel_vector.filename= {'accel_averaged_by_instance.mat'};
% parameters.loop_list.things_to_load.accel_vector.variable= {'accel_averaged_by_instance'}; 
% parameters.loop_list.things_to_load.accel_vector.level = 'mouse';
% 
% % Pupil diameter
% parameters.loop_list.things_to_load.pupil_diameter_vector.dir = {[parameters.dir_exper 'behavior\eye\rolled concatenated diameters\'], 'mouse', '\'};
% parameters.loop_list.things_to_load.pupil_diameter_vector.filename= {'diameter_averaged_by_instance.mat'};
% parameters.loop_list.things_to_load.pupil_diameter_vector.variable= {'diameter_averaged_by_instance'}; 
% parameters.loop_list.things_to_load.pupil_diameter_vector.level = 'mouse';
% 
% % Output 
% parameters.loop_list.things_to_save.response_variables.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\response variables\'], 'mouse', '\'};
% parameters.loop_list.things_to_save.response_variables.filename= {'response_variables_table.mat'};
% parameters.loop_list.things_to_save.response_variables.variable= {'response_variables'}; 
% parameters.loop_list.things_to_save.response_variables.level = 'mouse';
% 
% 
% parameters.loop_list.things_to_rename = cell(numel(parameters.additional_variables), 1);
% for variablei = 1:numel(parameters.additional_variables)
%     variable = parameters.additional_variables{variablei};
%     parameters.loop_list.things_to_rename{variablei} = {'data_evaluated', variable};
% 
% end
% 
% functions = [repmat({@EvaluateOnData}, 1, numel(parameters.additional_variables)) {@PopulateResponseVariables}];
% RunAnalysis(functions, parameters);
%% Prepare datasets per continuous comparison. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator'     
               };

% Specify which comparisons should be used for this dataset prep. 
parameters.this_comparison_set = parameters.comparisons_continuous;

% Remove outliers from explanatory variables.
parameters.removeOutliers = false;

% Flag for whether or not missing data (NaNs) should be imputed.
parameters.imputeMissing = true; 

% Number of PLSR components that should be used for imputing missing data.
% Using just 85% instead of 90% usually cuts number of components needed by
% half.
parameters.imputation_components_variance_explained = 85; % in percents
parameters.imputation_max_components = 10; 

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_load.response.variable= {'response_variables'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

parameters.loop_list.things_to_load.explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.explanatory.filename= {'values.mat'};
parameters.loop_list.things_to_load.explanatory.variable= {'values'}; 
parameters.loop_list.things_to_load.explanatory.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrep}, parameters);

parameters.removeOutliers = false;
parameters.imputeMissing = false;

%% PLSR Level 1, continuous: optimize components.
% Don't run any permutations yet.
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

% Parameters for calculating best number of components. If
% "findBestNComponents" = false, just run the ncomponents_max
parameters.findBestNComponents = true;
parameters.ncomponents_max = 10; 
parameters.contiguous_partitions = true; 
parameters.kFolds = 10;
parameters.MonteCarloReps = 10;
parameters.comparison_type = 'continuous';
parameters.stratify = false;

% Do you want permutations?
parameters.permutationGeneration = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);  

parameters.findBestNComponents = false;


%% PLSR Level 1, continuous: check components 
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.max_response_vars = 4;

% Plot weights?
parameters.plot_weights = false;

% Plot MSEPs?
parameters.plot_MSEPs = true;

% Plot BICs?
parameters.plot_BICs = true;

% Plot percent vars? 
parameters.plot_percentVars = false;

% Input
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
% parameters.loop_list.things_to_save.fig_weights.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\with 20 components\' 'mouse', '\'};
% parameters.loop_list.things_to_save.fig_weights.filename= {'PLSR_weights.fig'};
% parameters.loop_list.things_to_save.fig_weights.variable= {'fig_weights'}; 
% parameters.loop_list.things_to_save.fig_weights.level = 'comparison';

parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
parameters.loop_list.things_to_save.fig_BICs_response.level = 'mouse';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'mouse';

RunAnalysis({@CheckComponents}, parameters);
close all;

%% Plot Betas from continuous level 1 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator'     
               };

% Adjust beta values based on zscore sigmas?
parameters.adjust_beta = false;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'}; 
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Also load in dataset values for the zscore sigma.
parameters.loop_list.things_to_load.dataset_info.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset_info.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset_info.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset_info.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_Covs.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotBetas}, parameters);

close all;

%% Remove continuous variables effects from each behavior type. 
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

parameters.removeOutliers = false; 
parameters.imputeMissing = false; 
% Amount of variance explained you want for the number of PCs used in
% missing values imputation.
parameters.imputation_components_variance_explained = 75; % in percents
parameters.imputation_max_components = 10; 

% Input 
% The variables from the comparison
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% The results from the continuous regression (for the Betas)
parameters.loop_list.things_to_load.PLSR_results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.PLSR_results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.PLSR_results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.PLSR_results.level = 'comparison';
% Old correlation values 
parameters.loop_list.things_to_load.values_old.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.values_old.filename= {'values.mat'};
parameters.loop_list.things_to_load.values_old.variable= {'values'}; 
parameters.loop_list.things_to_load.values_old.level = 'mouse';

% Output
parameters.loop_list.things_to_save.values_new.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_save.values_new.filename= {'correlations_continuousSubtracted_averageOptimizedComponents.mat'};
parameters.loop_list.things_to_save.values_new.variable= {'correlations'}; 
parameters.loop_list.things_to_save.values_new.level = 'mouse';
% Info about outliers
if parameters.removeOutliers
parameters.loop_list.things_to_save.dataset_out.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'mouse', '\'};
parameters.loop_list.things_to_save.dataset_out.filename= {'residuals_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset_out.variable= {'residuals_dataset_info'}; 
parameters.loop_list.things_to_save.dataset_out.level = 'comparison';
end

RunAnalysis({@ResidualsFromContinuous}, parameters); 

parameters.removeOutliers = false; 
parameters.imputeMissing =false;

%% Level 1 categorical -- Prepare datasets, continuous subtracted.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator'     
               };

% Specify which comparisons should be used for this dataset prep. 
parameters.this_comparison_set = parameters.comparisons_categorical;

% Flag for whether or not missing data (NaNs) should be imputed. (Don't
% need it for these comparisons, already did it at residual level in previous step.)
parameters.removeOutliers = false;
parameters.imputeMissing = false; 

% Input 
% Don't need any outliers-removed responses with categorical
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_load.response.variable= {'response_variables'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

parameters.loop_list.things_to_load.explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.explanatory.filename= {'correlations_continuousSubtracted_averageOptimizedComponents.mat'};
parameters.loop_list.things_to_load.explanatory.variable= {'correlations'}; 
parameters.loop_list.things_to_load.explanatory.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrep}, parameters);

%% Level 1 categorical -- optimize number of components
% Will look at the outputs from 10 calculated components.

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

% Parameters for calculating best number of components. If
% "findBestNComponents" = false, just run the ncomponents_max
parameters.findBestNComponents = true;
parameters.ncomponents_max = 10; 
parameters.contiguous_partitions = true; 
parameters.kFolds = 10;
parameters.MonteCarloReps = 10;
parameters.comparison_type = 'categorical';
parameters.stratify = true;

% Do you want permutations?
parameters.permutationGeneration = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);  

parameters.findBestNComponents = false;

%% Level 1 categorical -- check components
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.max_response_vars = 2;

parameters.plot_weights = true;
parameters.plot_MSEPs = false;
parameters.plot_BICs = false;
parameters.plot_percentVars = false;

% Input
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig_weights.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig_weights.filename= {'PLSR_weights.fig'};
parameters.loop_list.things_to_save.fig_weights.variable= {'fig_weights'}; 
parameters.loop_list.things_to_save.fig_weights.level = 'comparison';

% parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
% parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
% parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
% parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
% parameters.loop_list.things_to_save.fig_BICs_response.level = 'mouse';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'mouse';

RunAnalysis({@CheckComponents}, parameters);

close all;

%% Level 1 categorical -- plot betas
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator'     
               };
% Adjust beta values based on zscore sigmas?
parameters.adjust_beta = false;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Also load in dataset values for the zscore sigma.
parameters.loop_list.things_to_load.dataset_info.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset_info.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset_info.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset_info.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_Cov.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotBetas}, parameters); 

close all;

%% RUN AVERAGES FOR LEVEL 2

%% Level 2 categorical -- Prep betas & mouse variables
% For any spontaneous, don't include mouse 1100

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

% Remove outliers & average
parameters.averaging_across_mice = true;
parameters.removeOutliers = false; 

% If the first level was categorical:
parameters.firstLevelCategorical = true; 

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.response.variable= {'PLSR_results.Cov'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_Cov.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% Level 2 continuous-- prep betas & mouse variables
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

% If the first level was categorical:
parameters.firstLevelCategorical = false; 
% Remove outliers & average
parameters.averaging_across_mice = true;
parameters.removeOutliers = false; 

parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.response.variable= {'PLSR_results.Cov'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_Cov.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% SIGNIFICANCE STUFF 

%% Level 1 continuous -- run random permutations.
% With best number of components.
% Always clear loop list first. 
do = true; 
if do 
    if isfield(parameters, 'loop_list')
    parameters = rmfield(parameters,'loop_list');
    end
    
    % Iterators
    parameters.loop_list.iterators = {
                   'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
                   'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };
    
    % Do you want permutations?
    parameters.permutationGeneration = true;
    parameters.useBootstrapping = false;
    parameters.n_permutations = 1000;
    parameters.stratify = false;
    parameters.comparison_type = 'continuous';
    
    % Input 
    % dataset
    parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
    parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
    parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
    parameters.loop_list.things_to_load.dataset.level = 'comparison';
    % optimized number of components to use.
    parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\', 'mouse', '\'};
    parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
    parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
    parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';

    % Output
    parameters.loop_list.things_to_save.Covs_randomPermutations.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
    parameters.loop_list.things_to_save.Covs_randomPermutations.filename= {'PLSR_Covs_randomPermutations.mat'};
    parameters.loop_list.things_to_save.Covs_randomPermutations.variable= {'Covs_randomPermutations'}; 
    parameters.loop_list.things_to_save.Covs_randomPermutations.level = 'comparison';
    
    RunAnalysis({@PLSR_forRunAnalysis}, parameters);  
    
    parameters.permutationGeneration = false;
end 

%% Level 1 categorical -- run random permutations.
do = true;
if do 
    % Always clear loop list first. 
    if isfield(parameters, 'loop_list')
    parameters = rmfield(parameters,'loop_list');
    end
    
    % Iterators
    parameters.loop_list.iterators = {
                   'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
                   'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' }; 
    
    % Do you want permutations?
    parameters.permutationGeneration = true;
    parameters.useBootstrapping = false;
    parameters.n_permutations = 1000;
    parameters.stratify = true;
    parameters.comparison_type = 'categorical';
    
    % Input 
    parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
    parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
    parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
    parameters.loop_list.things_to_load.dataset.level = 'comparison';
    % optimized number of components to use.
    parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\', 'mouse', '\'};
    parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
    parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
    parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';
    
    % Output
    parameters.loop_list.things_to_save.Covs_randomPermutations.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
    parameters.loop_list.things_to_save.Covs_randomPermutations.filename= {'PLSR_Covs_randomPermutations.mat'};
    parameters.loop_list.things_to_save.Covs_randomPermutations.variable= {'Covs_randomPermutations'}; 
    parameters.loop_list.things_to_save.Covs_randomPermutations.level = 'comparison';
    
    RunAnalysis({@PLSR_forRunAnalysis}, parameters);  
    
    parameters.permutationGeneration = false;
end

%% Level 2 continuous -- prep permutation shuffled datasets
do = true;
if do 
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

% If the first level was categorical:
parameters.firstLevelCategorical = false; 

parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';
parameters.averaging_across_mice = true;
parameters.removeOutliers = false;

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_Covs_randomPermutations.mat'};
parameters.loop_list.things_to_load.response.variable= {'Covs_randomPermutations'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_randomPermutations_Cov.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);
end 

%% Level 2 categorical -- prep permutation shuffled datasets
%Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

% If the first level was categorical:
parameters.firstLevelCategorical = true; 

% Remove outliers & average
parameters.averaging_across_mice = true;
parameters.removeOutliers = false; 

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 1 categorical\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_Covs_randomPermutations.mat'};
parameters.loop_list.things_to_load.response.variable= {'Covs_randomPermutations'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_randomPermutations_Cov.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% Level 2 continuous -- check significance with permutations
do = true;
if do 
% % Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };
parameters.shufflesDim = 2; % After the EvaluateOnData reduction
parameters.find_significance = true;

% The statistical alpha value
parameters.alphaValue = 0.05;  %0.05/496; %0.001; %/numel(parameters.comparisons_continuous);

% Say that you do want to use bootstrapping.
parameters.useBootstrapping = false;

% If you want to fit a normal distribution before t-test (default = true)
parameters.useNormalDistribution = true; 

% Use false discovery rate correction?
parameters.useFDR = true;

% Inputs:
%Test values (will grab only the intercepts with EvaluateOnData)
parameters.loop_list.things_to_load.test_values.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
parameters.loop_list.things_to_load.test_values.filename= {'PLSR_dataset_info_Cov.mat'};
parameters.loop_list.things_to_load.test_values.variable= {'dataset_info.average_across_mice'}; 
parameters.loop_list.things_to_load.test_values.level = 'comparison';
% Null distribution
parameters.loop_list.things_to_load.null_distribution.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
parameters.loop_list.things_to_load.null_distribution.filename= {'PLSR_dataset_info_randomPermutations_Cov.mat'};
parameters.loop_list.things_to_load.null_distribution.variable= {'dataset_info.average_across_mice'}; 
parameters.loop_list.things_to_load.null_distribution.level = 'comparison';

% Outputs
parameters.loop_list.things_to_save.significance.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\'], 'comparison', '\'};
parameters.loop_list.things_to_save.significance.filename= {'PLSR_significance_randomPermutations_Cov_FDR.mat'};
parameters.loop_list.things_to_save.significance.variable= {'PLSR_significance'}; 
parameters.loop_list.things_to_save.significance.level = 'comparison';

RunAnalysis({@SignificanceCalculation}, parameters);

parameters.useFDR = false;

end 

%% Level 2 categorical -- check significance with permutations
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

parameters.shufflesDim = 2;
parameters.find_significance = true;

% The statistical alpha value
parameters.alphaValue = 0.05; %/496; %/150;  %/(numel(parameters.comparisons_categorical) - 4;

% Use the Bootstrapping method. 
parameters.useBootstrapping = false;

% If you want to fit a normal distribution before t-test (default = true)
parameters.useNormalDistribution = true; 

% Use false discovery rate correction?
parameters.useFDR = true;

% Inputs:
% Test values
parameters.loop_list.things_to_load.test_values.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison', '\'};
parameters.loop_list.things_to_load.test_values.filename= {'PLSR_dataset_info_Cov.mat'};
parameters.loop_list.things_to_load.test_values.variable= {'dataset_info.average_across_mice'}; 
parameters.loop_list.things_to_load.test_values.level = 'comparison';
% Null distribution
parameters.loop_list.things_to_load.null_distribution.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison', '\'};
parameters.loop_list.things_to_load.null_distribution.filename= {'PLSR_dataset_info_randomPermutations_Cov.mat'};
parameters.loop_list.things_to_load.null_distribution.variable= {'dataset_info.average_across_mice'}; 
parameters.loop_list.things_to_load.null_distribution.level = 'comparison';

% Outputs
parameters.loop_list.things_to_save.significance.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 categorical\'], 'comparison', '\'};
parameters.loop_list.things_to_save.significance.filename= {'PLSR_significance_randomPermutations_Cov_FDR.mat'};
parameters.loop_list.things_to_save.significance.variable= {'PLSR_significance'}; 
parameters.loop_list.things_to_save.significance.level = 'comparison';

RunAnalysis({@SignificanceCalculation}, parameters);

% Use false discovery rate correction?
parameters.useFDR = false;

%% *** start plotting stages***

%% Level 2 categorical -- concatenate & average sigmas
% for fluorescence, don't take out of fluorescence z-score space

% For each comparison. For adjusting betas in plots below. 
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.comparison_type = 'categorical';                                   
parameters.concatDim = 1;
parameters.removeOutliers = false;
parameters.concatenation_level = 'mouse';


%parameters.use_xZscore = true;
parameters.use_xZscore = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 categorical\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'mouse';

% Output 
parameters.loop_list.things_to_save.average_sigmas.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison','\'};
parameters.loop_list.things_to_save.average_sigmas.filename= {'average_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.average_sigmas.variable= {'average_zscore_sigmas'}; 
parameters.loop_list.things_to_save.average_sigmas.level = 'comparison';

if parameters.removeOutliers
parameters.loop_list.things_to_save.sigma_outliers.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison','\'};
parameters.loop_list.things_to_save.sigma_outliers.filename= {'outliers_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.sigma_outliers.variable= {'outliers_zscore_sigmas'}; 
parameters.loop_list.things_to_save.sigma_outliers.level = 'comparison';
end
RunAnalysis({@AverageSigmas}, parameters);

%% Level 2 continuous -- concatenate & average sigmas
% For each comparison. For adjusting betas in plots below. 
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.comparison_type = 'continuous';                                   
parameters.concatDim = 1;
parameters.removeOutliers = false;
parameters.concatenation_level = 'mouse';

%parameters.use_xZscore = true;
parameters.use_xZscore = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 1 continuous\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'mouse';

% Output 
parameters.loop_list.things_to_save.average_sigmas.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison','\'};
parameters.loop_list.things_to_save.average_sigmas.filename= {'average_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.average_sigmas.variable= {'average_zscore_sigmas'}; 
parameters.loop_list.things_to_save.average_sigmas.level = 'comparison';

if parameters.removeOutliers
parameters.loop_list.things_to_save.sigma_outliers.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison','\'};
parameters.loop_list.things_to_save.sigma_outliers.filename= {'outliers_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.sigma_outliers.variable= {'outliers_zscore_sigmas'}; 
parameters.loop_list.things_to_save.sigma_outliers.level = 'comparison';
end
RunAnalysis({@AverageSigmas}, parameters);

%% Level 2 continuous -- plot betas with significance from permutations
% Plot all the beta intercepts in a single plot 
parameters.plotIndividually = false;
% Do for each variation of significance & adjusted
true_false_vector = {false, true};
for i = 2 %1:numel(true_false_vector)
    % Adjust beta values based on zscore sigmas?
    parameters.adjustBetas = true_false_vector{i};

    for j = 2 %1:numel(true_false_vector)
         % Only include significant betas?
         parameters.useSignificance = true_false_vector{j};

        if isfield(parameters, 'loop_list')
        parameters = rmfield(parameters,'loop_list');
        end
        
        % Iterators
        parameters.loop_list.iterators = {
                       'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

        % Averaging? 
        parameters.averaging_across_mice = true;
        parameters.removeOutliers = false;

        % Color range for all plots (if betas are adjusted).
        parameters.useColorRange = false;

        % Comparison type (continuous or continuous)
        parameters.comparison_type = 'continuous';
        parameters.this_comparison_set = parameters.comparisons_continuous;
        
        title = 'PLSR_Covs_all_comparisons';
        if parameters.adjustBetas
            title = [title '_Adjusted'];
        end
        if parameters.useSignificance 
            title = [title '_withSignificance_randomPermutation'];
        end
        title = [title '_FDR.fig'];
        
        % Input
        parameters.loop_list.things_to_load.average_across_mice.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.average_across_mice.filename = {'PLSR_dataset_info_Cov.mat'};
        parameters.loop_list.things_to_load.average_across_mice.variable = {'dataset_info.average_across_mice'};
        parameters.loop_list.things_to_load.average_across_mice.level = 'comparison';
        % significance matrix
        if parameters.useSignificance
        parameters.loop_list.things_to_load.significance.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.significance.filename= {'PLSR_significance_randomPermutations_Cov_FDR.mat'};
        parameters.loop_list.things_to_load.significance.variable= {'PLSR_significance.all'}; 
        parameters.loop_list.things_to_load.significance.level = 'comparison';
        end
        % Average sigmas.
        if parameters.adjustBetas
        parameters.loop_list.things_to_load.average_sigmas.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 continuous\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.average_sigmas.filename= {'average_zscore_sigmas.mat'};
        parameters.loop_list.things_to_load.average_sigmas.variable= {'average_zscore_sigmas'}; 
        parameters.loop_list.things_to_load.average_sigmas.level = 'comparison';
        end
        
        % Output
        parameters.loop_list.things_to_save.speed_fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\']};
        parameters.loop_list.things_to_save.speed_fig.filename = {['speed_ ' title]};
        parameters.loop_list.things_to_save.speed_fig.variable = {'speed_fig'};
        parameters.loop_list.things_to_save.speed_fig.level = 'end';

        parameters.loop_list.things_to_save.accel_fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\']};
        parameters.loop_list.things_to_save.accel_fig.filename = {['accel_ ' title]};
        parameters.loop_list.things_to_save.accel_fig.variable = {'accel_fig'};
        parameters.loop_list.things_to_save.accel_fig.level = 'end';

        parameters.loop_list.things_to_save.duration_fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\']};
        parameters.loop_list.things_to_save.duration_fig.filename = {['duration_ ' title]};
        parameters.loop_list.things_to_save.duration_fig.variable = {'duration_fig'};
        parameters.loop_list.things_to_save.duration_fig.level = 'end';

        parameters.loop_list.things_to_save.pupil_diameter_fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 continuous\']};
        parameters.loop_list.things_to_save.pupil_diameter_fig.filename = {['pupil_diameter_ ' title]};
        parameters.loop_list.things_to_save.pupil_diameter_fig.variable = {'pupil_diameter_fig'};
        parameters.loop_list.things_to_save.pupil_diameter_fig.level = 'end';
        
        RunAnalysis({@PlotBetasSecondLevel}, parameters);
    end
end 
%close all;
clear i j true_false_vector;

%% Level 2 categorical -- plot betas with significance from permutations
% Plot all the beta intercepts in a single plot 
parameters.plotIndividually = false;
% Do for each variation of significance & adjusted
true_false_vector = {false, true};
for i = 2 %1:numel(true_false_vector)
    % Adjust beta values based on zscore sigmas?
    parameters.adjustBetas = true_false_vector{i};

    for j = 2 %1:numel(true_false_vector)
         % Only include significant betas?
         parameters.useSignificance = true_false_vector{j};

        if isfield(parameters, 'loop_list')
        parameters = rmfield(parameters,'loop_list');
        end
        
        % Iterators
        parameters.loop_list.iterators = {
                       'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

        % Averaging? 
        parameters.averaging_across_mice = true;
        parameters.removeOutliers = false;

        % Color range for all plots (if betas are adjusted).
        parameters.useColorRange = false;
        
        % Comparison type (categorical or continuous)
        parameters.comparison_type = 'categorical';
        parameters.this_comparison_set = parameters.comparisons_categorical;
        
        title = 'PLSR_Covs_all_comparisons';
        if parameters.adjustBetas
            title = [title '_Adjusted'];
        end
        if parameters.useSignificance 
            title = [title '_withSignificance_randomPermutation_Cov'];
        end
        title = [title '_FDR.fig'];
        
        % Input
        parameters.loop_list.things_to_load.average_across_mice.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.average_across_mice.filename = {'PLSR_dataset_info_Cov.mat'};
        parameters.loop_list.things_to_load.average_across_mice.variable = {'dataset_info.average_across_mice'};
        parameters.loop_list.things_to_load.average_across_mice.level = 'comparison';
        % significance matrix
        if parameters.useSignificance
        parameters.loop_list.things_to_load.significance.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 categorical\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.significance.filename= {'PLSR_significance_randomPermutations_Cov_FDR.mat'};
        parameters.loop_list.things_to_load.significance.variable= {'PLSR_significance.all'}; 
        parameters.loop_list.things_to_load.significance.level = 'comparison';
        end
        % Average sigmas.
        if parameters.adjustBetas
        parameters.loop_list.things_to_load.average_sigmas.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 categorical\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.average_sigmas.filename= {'average_zscore_sigmas.mat'};
        parameters.loop_list.things_to_load.average_sigmas.variable= {'average_zscore_sigmas'}; 
        parameters.loop_list.things_to_load.average_sigmas.level = 'comparison';
        end
        
        % Output
        parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 categorical\']};
        parameters.loop_list.things_to_save.fig.filename = {title};
        parameters.loop_list.things_to_save.fig.variable = {'PLSR_Covs'};
        parameters.loop_list.things_to_save.fig.level = 'end';
        
        RunAnalysis({@PlotBetasSecondLevel}, parameters);
    end
end 
%close all;
clear i j true_false_vector;

%% Reshape for dot plots
% make inputs match the average node inputs for figure creation.
comparison_types = {'categorical', 'continuous'};

for typei = 1:numel(comparison_types)

    comparison_type = comparison_types{typei};


    if isfield(parameters, 'loop_list')
    parameters = rmfield(parameters,'loop_list');
    end
        

    % Iterators
    parameters.loop_list.iterators = {
                   'comparison', {['loop_variables.comparisons_' comparison_type '(:).name']}, 'comparison_iterator' };
    
    
    parameters.evaluation_instructions = {{'b = repmat(parameters.data, 2,1);'...
                                          'data_evaluated = reshape(b, size(parameters.data,2) * 2 , size(parameters.data,1));'}};
    % Inputs 
    parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_load.data.filename= {'PLSR_dataset_info_Cov.mat'};
    parameters.loop_list.things_to_load.data.variable= {'dataset_info.average_across_mice'}; 
    parameters.loop_list.things_to_load.data.level = 'comparison';
    
    % Outputs
    parameters.loop_list.things_to_save.data_evaluated.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_save.data_evaluated.filename= {'averages_reshaped.mat'};
    parameters.loop_list.things_to_save.data_evaluated.variable= {'averages_reshaped'}; 
    parameters.loop_list.things_to_save.data_evaluated.level = 'comparison';
    
    RunAnalysis({@EvaluateOnData}, parameters);

end

%% Reshape significance for dots plots
comparison_types = {'categorical', 'continuous'};

for typei = 1:numel(comparison_types)

    comparison_type = comparison_types{typei};


    if isfield(parameters, 'loop_list')
    parameters = rmfield(parameters,'loop_list');
    end
        
    % Iterators
    parameters.loop_list.iterators = {
                   'comparison', {['loop_variables.comparisons_' comparison_type '(:).name']}, 'comparison_iterator' };
    
     
    parameters.evaluation_instructions = {{'b = repmat(parameters.data, 1, 2);'...
                                          'data_evaluated = transpose(reshape(transpose(b), size(parameters.data,2), size(parameters.data,1) * 2));'}};
    % Inputs 
    parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_load.data.filename= {'PLSR_significance_randomPermutations_Cov_FDR.mat'};
    parameters.loop_list.things_to_load.data.variable= {'PLSR_significance.all'}; 
    parameters.loop_list.things_to_load.data.level = 'comparison';
    
    % Outputs
    parameters.loop_list.things_to_save.data_evaluated.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_save.data_evaluated.filename= {'significance_reshaped.mat'};
    parameters.loop_list.things_to_save.data_evaluated.variable= {'significance_reshaped'}; 
    parameters.loop_list.things_to_save.data_evaluated.level = 'comparison';
    
    RunAnalysis({@EvaluateOnData}, parameters);

end

%% Calculate multipliers to get back into %F/F 
% ** Continuous**
% 1. convert back to absolute fluorescence units using average sigmas
% 2. multiply by overall fluorescence mean for that IC (from
% pipeline_DFF_means.m from fluorescence analysis pipeline folder)

comparison_types = {'categorical', 'continuous'};

for typei = 1:numel(comparison_types)

    comparison_type = comparison_types{typei};

    % Iterators
    parameters.loop_list.iterators = {
                   'comparison', {['loop_variables.comparisons_' comparison_type '(:).name']}, 'comparison_iterator' };
    

    if isfield(parameters, 'loop_list')
    parameters = rmfield(parameters,'loop_list');
    end
            
    % Iterators
    parameters.loop_list.iterators = {
                   'comparison', {'loop_variables.comparisons_' comparison_type '(:).name'}, 'comparison_iterator' };
    
    % Shape average sigmas to match results values, multiply results by average
    % sigmas.
    parameters.evaluation_instructions = {
                                           {'b = repmat(transpose(parameters.average_sigmas), 1, 2);'...
                                          'average_sigmas_reshaped = reshape(transpose(b), size(parameters.average_sigmas, 1), size(parameters.average_sigmas, 2) * 2);'...
                                          'data_evaluated = parameters.data .* transpose(average_sigmas_reshaped);'}
    
                                           {'data_evaluated = repmat(transpose(parameters.fluorescence_mean), size(parameters.data,1)./(parameters.number_of_sources * 2), 1);'}
                                          
                                          };
    
    % Inputs
    % PLSR COVs 
    parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_load.data.filename= {'averages_reshaped.mat'};
    parameters.loop_list.things_to_load.data.variable= {'averages_reshaped'}; 
    parameters.loop_list.things_to_load.data.level = 'comparison';
    
    % average sigmas 
    parameters.loop_list.things_to_load.average_sigmas.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\variable prep\datasets\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_load.average_sigmas.filename= {'average_zscore_sigmas.mat'};
    parameters.loop_list.things_to_load.average_sigmas.variable= {'average_zscore_sigmas'}; 
    parameters.loop_list.things_to_load.average_sigmas.level = 'comparison';
    
    % average fluorescence per source across mice (NOT renumbered --
    % renumbering happens in dot plots)
    parameters.loop_list.things_to_load.fluorescence_mean.dir = {[parameters.dir_exper '\preprocessing\stack means\']};
    parameters.loop_list.things_to_load.fluorescence_mean.filename = {'IC_means_acrossMice_homologousTogether.mat'};
    parameters.loop_list.things_to_load.fluorescence_mean.variable = {'source_mean'};
    parameters.loop_list.things_to_load.fluorescence_mean.level = 'start';
    
    % Outputs
    % value multipliers
    parameters.loop_list.things_to_save.DFF.dir = {[parameters.dir_exper 'PLSR fluorescence Warning Periods\results\level 2 ' comparison_type '\'], 'comparison', '\'};
    parameters.loop_list.things_to_save.DFF.filename= {'results_DFF.mat'};
    parameters.loop_list.things_to_save.DFF.variable= {'results_DFF'}; 
    parameters.loop_list.things_to_save.DFF.level = 'comparison';
    
    parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'}; 
                                             {'data_evaluated', 'fluorescence_mean'}};
    
    RunAnalysis({@EvaluateOnData, @EvaluateOnData, @DFF}, parameters);

end