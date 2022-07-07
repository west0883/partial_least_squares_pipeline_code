% pipeline_PLSR_multilevel.m
% Sarah West
% 6/9/22 
% Sets up variables for & runs partial least squares regression analysis
% for Random Motorized Treadmill experiements. 

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
parameters.number_of_sources = 32; 
parameters.indices = find(tril(ones(parameters.number_of_sources), -1));

% Load periods_nametable_PLSR.m, if it exists yet. (Otherwise is created in
% first step).
if isfile([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat'])
    load([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat']);
    parameters.periods = periods;

    % Also load the indices to remove
    load([parameters.dir_exper 'PLSR\indices_to_remove.mat']);
    parameters.indices_to_remove = indices_to_remove;

    % Load lists of response categories
    load([parameters.dir_exper 'PLSR\response_categories.mat']);
    parameters.loop_variables.response_categories = categories;
    parameters.categories = categories;

    clear periods indices_to_remove categories;

end

% Load comparisons for first level continuous, if it exists yet.
if isfile([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat'])
    load([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat']);
    parameters.comparisons_continuous = comparisons;
    parameters.loop_variables.comparisons_continuous = parameters.comparisons_continuous; 
    clear comparisons;
end

% Load comparisons for first level categorical, if it exists yet.
if isfile([parameters.dir_exper 'PLSR\comparisons_level1_categorical.mat'])
    load([parameters.dir_exper 'PLSR\comparisons_level1_categorical.mat']);
    parameters.comparisons_categorical = comparisons;
    parameters.loop_variables.comparisons_categorical = parameters.comparisons_categorical;
    clear comparisons;
end

% Load list of variables to subtract from level 1 categoricals, if it
% exists yet.
if isfile([parameters.dir_exper 'PLSR\variablesToSubtract_level1_categorical.mat'])
    load([parameters.dir_exper 'PLSR\variablesToSubtract_level1_categorical.mat']);
    parameters.variablesToSubtract = variablesToSubtract;
    clear variablesToSubtract;
end

% Names of all continuous variables.
parameters.continuous_variable_names = {'speed', 'accel', 'duration', 'pupil_diameter'};

% Put relevant variables into loop_variables.
parameters.loop_variables.mice_all = parameters.mice_all;
parameters.loop_variables.periods = parameters.periods.condition; 
parameters.loop_variables.conditions = {'motorized'; 'spontaneous'};
parameters.loop_variables.conditions_stack_locations = {'stacks'; 'spontaneous'};
parameters.loop_variables.variable_type = {'response variables', 'correlations'};
parameters.loop_variables.categories.type = parameters.categories.type;

%% Create periods_nametable_forPLSR.mat
% If hasn't been created already. 
if ~isfile([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat'])
    create_periods_nametable_forPLSR
    load([parameters.dir_exper 'PLSR\periods_nametable_forPLSR.mat']);
    parameters.periods = periods; 

    % Also load the indices to remove
    load([parameters.dir_exper 'PLSR\indices_to_remove.mat']);
    parameters.indices_to_remove = indices_to_remove;

     % Also load lists of response categories
    load([parameters.dir_exper 'PLSR\response_catecories.mat'])
    parameters.loop_variables.response_categories = categories;
    parameters.categories = categories;

    clear periods indices_to_remove categories;
end

%% Create comparisons_continuous
% If it hasn't been created already.
if ~isfile([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat'])
    create_periods_nametable_forPLSR
    load([parameters.dir_exper 'PLSR\comparisons_level1_continuous.mat']);
    parameters.comparisons_continuous = comparisons; 
    parameters.loop_variables.comparisons_continuous= comparisons;
    clear comparisons;
end

%% Create comparisons_categorical
% If it hasn't been created already.
if ~isfile([parameters.dir_exper 'PLSR\comparisons_level1_categorical.mat'])
    create_periods_nametable_forPLSR
    load([parameters.dir_exper 'PLSR\comparisons_level1_categorical.mat']);
    parameters.comparisons_continuous = comparisons; 
    parameters.loop_variables.comparisons_categorical = comparisons;
    clear comparisons;
end

%% Remove correlations for periods you don't want to use. 
% From saved indices from creation of response variables .
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Evaluation instructions.
parameters.evaluation_instructions = {{
          'data_evaluated = parameters.data;'...
          'data_evaluated(parameters.indices_to_remove) = [];'}};
% Input 
% The reshaped correlations per mouse from fluorescence analysis pipeline.
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'fluorescence analysis\correlations\Fisher transformed\'], 'mouse', '\instances reshaped\'};
parameters.loop_list.things_to_load.data.filename= {'values.mat'};
parameters.loop_list.things_to_load.data.variable= {'values'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.data_evaluated.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_save.data_evaluated.filename= {'values.mat'};
parameters.loop_list.things_to_save.data_evaluated.variable= {'values'}; 
parameters.loop_list.things_to_save.data_evaluated.level = 'mouse';

RunAnalysis({@EvaluateOnData}, parameters); 

%% Put in response variables, no vertical concatenation

if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Variables to replicate
parameters.response_variable_names = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'transition_or_not_dummyvars_vector', 'speed_vector', 'accel_vector', 'duration_vector', 'pupil_diameter_vector'};
parameters.variables_static = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'transition_or_not_dummyvars_vector', 'duration_vector'};
parameters.motorized_variables_static = {'speed_vector', 'accel_vector'}; % These are the ones that are static in motorized, not static in spontaneous
% Original order of spontaneous (for velocity & accel indexing)
parameters.spontaneous_periods_order = {'rest', 'walk', 'prewalk', 'startwalk', 'stopwalk', 'postwalk'};

parameters.concatenate_vertically = false;

% Input
% Correlations (for instances count)
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'values_relevent_periods.mat'};
parameters.loop_list.things_to_load.data.variable= {'values'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Spontaneous velocity
parameters.loop_list.things_to_load.speed_vector.dir = {[parameters.dir_exper 'behavior\spontaneous\rolled concatenated velocity\'], 'mouse', '\'};
parameters.loop_list.things_to_load.speed_vector.filename= {'velocity_averaged_by_instance.mat'};
parameters.loop_list.things_to_load.speed_vector.variable= {'velocity_averaged_by_instance'}; 
parameters.loop_list.things_to_load.speed_vector.level = 'mouse';

% Spontaneous accel.
parameters.loop_list.things_to_load.accel_vector.dir = {[parameters.dir_exper 'behavior\spontaneous\rolled concatenated velocity\'], 'mouse', '\'};
parameters.loop_list.things_to_load.accel_vector.filename= {'accel_averaged_by_instance.mat'};
parameters.loop_list.things_to_load.accel_vector.variable= {'accel_averaged_by_instance'}; 
parameters.loop_list.things_to_load.accel_vector.level = 'mouse';

% Pupil diameter
parameters.loop_list.things_to_load.diameter_vector.dir = {[parameters.dir_exper 'behavior\eye\rolled concatenated diameters\'], 'mouse', '\'};
parameters.loop_list.things_to_load.diameter_vector.filename= {'diameter_averaged_by_instance.mat'};
parameters.loop_list.things_to_load.diameter_vector.variable= {'diameter_averaged_by_instance'}; 
parameters.loop_list.things_to_load.diameter_vector.level = 'mouse';

% Output 
parameters.loop_list.things_to_save.response_variables.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_save.response_variables.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_save.response_variables.variable= {'response_variables'}; 
parameters.loop_list.things_to_save.response_variables.level = 'mouse';

RunAnalysis({@PopulateResponseVariables}, parameters);

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
parameters.removeOutliers = true;

% Flag for whether or not missing data (NaNs) should be imputed.
parameters.imputeMissing = true; 

% Number of PLSR components that should be used for imputing missing data.
% Using just 85% instead of 90% usually cuts number of components needed by
% half.
parameters.imputation_components_variance_explained = 75; % in percents
parameters.imputation_max_components = 15; 

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_load.response.variable= {'response_variables'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

parameters.loop_list.things_to_load.explanatory.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.explanatory.filename= {'values.mat'};
parameters.loop_list.things_to_load.explanatory.variable= {'values'}; 
parameters.loop_list.things_to_load.explanatory.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrep}, parameters);

parameters.removeOutliers = false;
parameters.imputeMissing = false;

%% Find the average ratio of NaNs in pupil diameter across continuous comparisons per mouse.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Is so you can use a single loop for calculations. 
parameters.loop_list.iterators = {
                'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
                'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

parameters.evaluation_instructions = {{'data_evaluated = parameters.data.responseVariables(end);'}}; % pupil diameter is always the last variable entered.
parameters.concatDim = 1;
parameters.concatenation_level = 'comparison';
parameters.averageDim = 1;
parameters.average_and_std_together = true;

% Input Values 
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.data.variable= {'dataset_info.NaN_ratios'}; 
parameters.loop_list.things_to_load.data.level = 'comparison';

% Output values
parameters.loop_list.things_to_save.average.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\NaN ratios\'], 'mouse', '\'};
parameters.loop_list.things_to_save.average.filename= {'average_missing_pupil_data_ratios.mat'};
parameters.loop_list.things_to_save.average.variable= {'average_missing_pupil_data_ratios'}; 
parameters.loop_list.things_to_save.average.level = 'mouse';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'}; 
                                         { 'concatenated_data', 'data'}};

RunAnalysis({@EvaluateOnData,@ConcatenateData, @AverageData}, parameters);

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
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);  

parameters.findBestNComponents = false;

%% Level 1 continuous -- plot histograms of number of components used .

% Concatenate the nubmer of components used per mouse.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator'     
               };

parameters.concatDim = 1;
parameters.concatenation_level = 'comparison';
parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.evaluation_instructions = {{}; 
                                      { 'if parameters.values{end} == size(parameters.this_comparison_set,2);'...
                                            'parameters.histogram = figure;' ...
                                            'histogram(parameters.concatenated_data);' ...
                                            'title(["mouse " parameters.values{1}]);' ...
                                            'data_evaluated = []; else; parameters.histogram = []; data_evaluated = []; end;'}};
% Input 
% Number of components used
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.data.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.data.level = 'comparison';

% Output 
% Concatenated data
parameters.loop_list.things_to_save.concatenated_data.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\ncomponents\outliers removed\'],'mouse', '\'};
parameters.loop_list.things_to_save.concatenated_data.filename= {'ncomponents_used_allcomparisons.mat'};
parameters.loop_list.things_to_save.concatenated_data.variable= {'ncomponents_used_allcomparisons'}; 
parameters.loop_list.things_to_save.concatenated_data.level = 'mouse';
% Histogram
parameters.loop_list.things_to_save.histogram.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\ncomponents\outliers removed\'], 'mouse', '\'};
parameters.loop_list.things_to_save.histogram.filename= {'ncomponents_used_allcomparisons.fig'};
parameters.loop_list.things_to_save.histogram.variable= {'ncomponents_used_allcomparisons_fig'}; 
parameters.loop_list.things_to_save.histogram.level = 'mouse';

RunAnalysis({@ConcatenateData, @EvaluateOnData}, parameters);

close all;

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
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
% parameters.loop_list.things_to_save.fig_weights.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\'], 'comparison', '\with 20 components\' 'mouse', '\'};
% parameters.loop_list.things_to_save.fig_weights.filename= {'PLSR_weights.fig'};
% parameters.loop_list.things_to_save.fig_weights.variable= {'fig_weights'}; 
% parameters.loop_list.things_to_save.fig_weights.level = 'comparison';

parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
parameters.loop_list.things_to_save.fig_BICs_response.level = 'mouse';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'mouse';

RunAnalysis({@CheckComponents}, parameters);

close all;

%% Level 1 continuous -- run random permutations.
% With best number of components.
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

% Do you want permutations?
parameters.permutationGeneration = true;
parameters.n_permutations = 5000;
parameters.stratify = false;
parameters.comparison_type = 'continuous';

% Input 
% dataset
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% optimized number of components to use.
parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';

% Output
parameters.loop_list.things_to_save.betas_randomPermutations.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.betas_randomPermutations.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_save.betas_randomPermutations.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_save.betas_randomPermutations.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);  

parameters.permutationGeneration = false;

%% Level 1 continuous -- run bootstrapping.
% With best number of components.
% profile off;
% profile on;
% % Always clear loop list first. 
% if isfield(parameters, 'loop_list')
% parameters = rmfield(parameters,'loop_list');
% end
% 
% % Iterators
% parameters.loop_list.iterators = {
%                'mouse', {'loop_variables.mice_all(6:end).name'}, 'mouse_iterator'; 
%                'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };
% 
% % Do you want permutations?
% parameters.useBootstrapping = true;
% parameters.n_bootstraps = 10000;
% parameters.stratify = false;
% parameters.comparison_type = 'continuous';
% 
% % Input 
% % dataset
% parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\'], 'comparison', '\' 'mouse', '\'};
% parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
% parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
% parameters.loop_list.things_to_load.dataset.level = 'comparison';
% % optimized number of components to use.
% parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
% parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
% parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
% parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';
% 
% % Output
% parameters.loop_list.things_to_save.betas_bootstrap.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
% parameters.loop_list.things_to_save.betas_bootstrap.filename= {'PLSR_betas_bootstrap.mat'};
% parameters.loop_list.things_to_save.betas_bootstrap.variable= {'betas_bootstrap'}; 
% parameters.loop_list.things_to_save.betas_bootstrap.level = 'comparison';
% 
% RunAnalysis({@PLSR_forRunAnalysis}, parameters);  
% 
% parameters.useBootstrapping = false;
% profile viewer; 


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
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'}; 
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Also load in dataset values for the zscore sigma.
parameters.loop_list.things_to_load.dataset_info.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset_info.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset_info.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset_info.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_betas.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotBetas}, parameters);

close all;

%% Remove continuous variables effects from each behavior type. 
% Continuous comparisons list is hard-coded in. 

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

parameters.removeOutliers = true; 
parameters.imputeMissing = true; 
% Amount of variance explained you want for the number of PCs used in
% missing values imputation.
parameters.imputation_components_variance_explained = 75; % in percents
parameters.imputation_max_components = 10; 

% Input 
% The variables from the comparison
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% The results from the continuous regression (for the Betas)
parameters.loop_list.things_to_load.PLSR_results.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.PLSR_results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.PLSR_results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.PLSR_results.level = 'comparison';
% Old correlation values 
parameters.loop_list.things_to_load.values_old.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.values_old.filename= {'values.mat'};
parameters.loop_list.things_to_load.values_old.variable= {'values'}; 
parameters.loop_list.things_to_load.values_old.level = 'mouse';

% Output
parameters.loop_list.things_to_save.values_new.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_save.values_new.filename= {'correlations_continuousSubtracted_withPupil_contiguousPartitions_outliersRemoved.mat'};
parameters.loop_list.things_to_save.values_new.variable= {'correlations'}; 
parameters.loop_list.things_to_save.values_new.level = 'mouse';
% Info about outliers
if parameters.removeOutliers
parameters.loop_list.things_to_save.dataset_out.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'mouse', '\'};
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
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_load.response.variable= {'response_variables'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

parameters.loop_list.things_to_load.explanatory.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.explanatory.filename= {'correlations_continuousSubtracted_withPupil_contiguousPartitions_outliersRemoved.mat'};
parameters.loop_list.things_to_load.explanatory.variable= {'correlations'}; 
parameters.loop_list.things_to_load.explanatory.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
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
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\results\\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
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

parameters.plot_weights = false;
parameters.plot_MSEPs = true;
parameters.plot_BICs = true;
parameters.plot_percentVars = false;

% Input
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
% parameters.loop_list.things_to_save.fig_weights.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
% parameters.loop_list.things_to_save.fig_weights.filename= {'PLSR_weights.fig'};
% parameters.loop_list.things_to_save.fig_weights.variable= {'fig_weights'}; 
% parameters.loop_list.things_to_save.fig_weights.level = 'comparison';

parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'mouse';

parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
parameters.loop_list.things_to_save.fig_BICs_response.level = 'mouse';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'mouse';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\MSEPs to 20\'],  'mouse', '\'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'mouse';

RunAnalysis({@CheckComponents}, parameters);

close all;

%% Level 1 categorical -- run bootstrapping.
% % With best number of components.
% profile off;
% profile on;
% 
% % Always clear loop list first. 
% if isfield(parameters, 'loop_list')
% parameters = rmfield(parameters,'loop_list');
% end
% 
% % Iterators
% parameters.loop_list.iterators = {
%                'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
%                'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };
% 
% % Do you want permutations?
% parameters.useBootstrapping = true;
% parameters.n_bootstraps = 10000;
% parameters.stratify = true;
% parameters.comparison_type = 'categorical';
% 
% % Input 
% % dataset
% parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\outliers removed\'], 'comparison', '\' 'mouse', '\'};
% parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
% parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
% parameters.loop_list.things_to_load.dataset.level = 'comparison';
% % optimized number of components to use.
% parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
% parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
% parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
% parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';
% 
% % Output
% parameters.loop_list.things_to_save.betas_bootstrap.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
% parameters.loop_list.things_to_save.betas_bootstrap.filename= {'PLSR_betas_bootstrap.mat'};
% parameters.loop_list.things_to_save.betas_bootstrap.variable= {'betas_bootstrap'}; 
% parameters.loop_list.things_to_save.betas_bootstrap.level = 'comparison';
% 
% RunAnalysis({@PLSR_forRunAnalysis}, parameters);  
% 
% parameters.useBootstrapping = false;
% profile viewer;

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
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Also load in dataset values for the zscore sigma.
parameters.loop_list.things_to_load.dataset_info.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset_info.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset_info.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset_info.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_betas.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotBetas}, parameters); 

close all;

%% Level 1 categorical -- plot histograms of number of components used .

% Concatenate the nubmer of components used per mouse.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator'     
               };

parameters.concatDim = 1;
parameters.concatenation_level = 'comparison';
parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.evaluation_instructions = {{}; 
                                      { 'if parameters.values{end} == size(parameters.this_comparison_set,2);'...
                                            'parameters.histogram = figure;' ...
                                            'histogram(parameters.concatenated_data);' ...
                                            'title(["mouse " parameters.values{1}]);' ...
                                            'data_evaluated = []; else; parameters.histogram = []; data_evaluated = []; end;'}};
% Input 
% Number of components used
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.data.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.data.level = 'comparison';

% Output 
% Concatenated data
parameters.loop_list.things_to_save.concatenated_data.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\ncomponents\'],'mouse', '\'};
parameters.loop_list.things_to_save.concatenated_data.filename= {'ncomponents_used_allcomparisons.mat'};
parameters.loop_list.things_to_save.concatenated_data.variable= {'ncomponents_used_allcomparisons'}; 
parameters.loop_list.things_to_save.concatenated_data.level = 'mouse';
% Histogram
parameters.loop_list.things_to_save.histogram.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\ncomponents\'], 'mouse', '\'};
parameters.loop_list.things_to_save.histogram.filename= {'ncomponents_used_allcomparisons.fig'};
parameters.loop_list.things_to_save.histogram.variable= {'ncomponents_used_allcomparisons_fig'}; 
parameters.loop_list.things_to_save.histogram.level = 'mouse';

RunAnalysis({@ConcatenateData, @EvaluateOnData}, parameters);

%% Level 1 categorical -- run random permutations.
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
parameters.n_permutations = 5000;
parameters.stratify = true;
parameters.comparison_type = 'categorical';

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% optimized number of components to use.
parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';

% Output
parameters.loop_list.things_to_save.betas_randomPermutations.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.betas_randomPermutations.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_save.betas_randomPermutations.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_save.betas_randomPermutations.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);  

parameters.permutationGeneration = false;

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

% If the first level was categorical:
parameters.firstLevelCategorical = true; 

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.response.variable= {'PLSR_results.BETA'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% RUN AVERAGES WITH OUTLIERS REMOVED INSTEAD

%% Level 2 categorical -- optimize number of components
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

% Parameters for calculating best number of compo
% nents. If "findBestNComponents" = false, just run the ncomponents_max
parameters.findBestNComponents = true;
parameters.ncomponents_max = 6; 
parameters.contiguous_partitions = true; 
parameters.kFolds = 7;
parameters.MonteCarloReps = 6;
parameters.comparison_type = 'categorical';
parameters.stratify = false; % Don't need to stratify on 2nd levels.
parameters.run_with_max_components = true; % In addition to finding best number of components

% Do you want permutations?
parameters.permutationGeneration = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters); 

parameters.findBestNComponents = false;
parameters.run_with_max_components = false;

%% Level 2 categorical -- check components
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };
parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.plot_weights = true;
parameters.plot_MSEPs = true;
parameters.plot_BICs = true;
parameters.plot_percentVars = false;
parameters.analysis_level = 2;

% Input
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig_weights.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.fig_weights.filename= {'PLSR_weights.fig'};
parameters.loop_list.things_to_save.fig_weights.variable= {'fig_weights'}; 
parameters.loop_list.things_to_save.fig_weights.level = 'comparison';

parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'end';

parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'end';

parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'end';

parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
parameters.loop_list.things_to_save.fig_BICs_response.level = 'end';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'end';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\MSEPs to 6\']};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'end';

RunAnalysis({@CheckComponents}, parameters);

close all;  

%% Level 2 categorical -- prep shuffled datasets for PLSR on shuffles.
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; };

% If the first level was categorical:
parameters.firstLevelCategorical = true; 

parameters.this_comparison_set = parameters.comparisons_categorical;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\results\level 1 categorical\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_load.response.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_randomPermutations.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% Level 2 categorical -- PLSR on shuffles
% Run a second-level PLSR on the random shuffles from level 1. 

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

% Say that you do want to run on permutations
parameters.onPermutations = true;

% If the first level was categorical:
parameters.firstLevelCategorical = true; 
parameters.stratify = false; % Don't need to stratify when running on already-found permutations.
parameters.comparison_type = 'categorical';

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info_randomPermutations.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% optimized number of components to use.
parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';

% Output
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);

parameters.onPermutations = false;

%% Level 2 categorical -- check significance
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };

parameters.evaluation_instructions = {{'data_evaluated = transpose(squeeze(parameters.test_values(1, :, :)));'}
                                      {'data_evaluated = squeeze(parameters.null_distribution(1, :, :));'}};
parameters.shufflesDim = 2; % After the EvaluateOnData reduction
parameters.find_significance = true;

% The statistical alpha value
parameters.alphaValue = 0.05; % / numel(parameters.comparisons_categorical);

% If you want to fit a normal distribution before t-test (default = true)
parameters.useNormalDistribution = false; 

% Inputs:
% Test values (will grab only the intercepts with EvaluateOnData)
parameters.loop_list.things_to_load.test_values.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.test_values.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.test_values.variable= {'PLSR_results.BETA'}; 
parameters.loop_list.things_to_load.test_values.level = 'comparison';
% Null distribution
parameters.loop_list.things_to_load.null_distribution.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.null_distribution.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_load.null_distribution.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_load.null_distribution.level = 'comparison';

% Outputs
parameters.loop_list.things_to_save.significance.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.significance.filename= {'PLSR_significance.mat'};
parameters.loop_list.things_to_save.significance.variable= {'PLSR_significance'}; 
parameters.loop_list.things_to_save.significance.level = 'comparison';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'test_values'}
                                          {'data_evaluated', 'null_distribution'}}; 

RunAnalysis({@EvaluateOnData, @EvaluateOnData, @SignificanceCalculation}, parameters);


%% Level 2 categorical -- concatenate & average sigmas
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

parameters.evaluation_instructions = {{'if strcmp(parameters.values(2), parameters.this_comparison_set(parameters.values{3}).mice_not_to_use);'... % Skip mice not to use
                                       'data_evaluated = [];'...
                                       'else;'...
                                       'ysig = parameters.dataset.zscoring.responseVariables.sigma;'... 
                                       'xsig = repmat(parameters.dataset.zscoring.explanatoryVariables.sigma, size(ysig,2),1);'...  % Make dimensions match (replicate corrs so there's a set for each response varaible.
                                       'data_evaluated = reshape(transpose(transpose(ysig)./xsig), 1, []);'...
                                       'end;'}};
                                   
parameters.concatDim = 3;
parameters.concatenation_level = 'mouse';
parameters.averageDim = 3;
parameters.average_and_std_together = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 categorical\optimized components\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'mouse';

% Output 
parameters.loop_list.things_to_save.average.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_save.average.filename= {'average_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.average.variable= {'average_zscore_sigmas'}; 
parameters.loop_list.things_to_save.average.level = 'comparison';

parameters.loop_list.things_to_save.std_dev.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_save.std_dev.filename= {'std_dev_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.std_dev.variable= {'std_dev_zscore_sigmas'}; 
parameters.loop_list.things_to_save.std_dev.level = 'comparison';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'};
                                         {}}; 

RunAnalysis({@EvaluateOnData, @ConcatenateData, @AverageData}, parameters);

%% Level 2 categorical -- plot betas
% Plot all the beta intercepts in a single plot 

% Do for each variation of significance & adjusted
true_false_vector = {false, true};
for i = 1:numel(true_false_vector)
    % Adjust beta values based on zscore sigmas?
    parameters.adjustBetas = true_false_vector{i};

    for j = 1:numel(true_false_vector)
         % Only include significant betas?
         parameters.useSignificance = true_false_vector{j};

        if isfield(parameters, 'loop_list')
        parameters = rmfield(parameters,'loop_list');
        end
        
        % Iterators
        parameters.loop_list.iterators = {
                       'comparison', {'loop_variables.comparisons_categorical(:).name'}, 'comparison_iterator' };
        
        % Color range for all plots (if betas are adjusted).
        parameters.useColorRange = true;
        parameters.color_range = [-0.02 0.02];
        
        % Comparison type (categorical or continuous)
        parameters.comparison_type = 'categorical';
        parameters.this_comparison_set = parameters.comparisons_categorical;
        
        title = 'PLSR_betas_all_comparisons';
        if parameters.adjustBetas
            title = [title '_Adjusted'];
        end
        if parameters.useSignificance 
            title = [title '_withSignificance'];
        end
        title = [title '.fig'];
        
        % Input
        parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.results.filename = {'PLSR_results.mat'};
        parameters.loop_list.things_to_load.results.variable = {'PLSR_results'};
        parameters.loop_list.things_to_load.results.level = 'comparison';
        % significance matrix
        if parameters.useSignificance
        parameters.loop_list.things_to_load.significance.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.significance.filename= {'PLSR_significance.mat'};
        parameters.loop_list.things_to_load.significance.variable= {'PLSR_significance.all'}; 
        parameters.loop_list.things_to_load.significance.level = 'comparison';
        end
        % Average sigmas.
        if parameters.adjustBetas
        parameters.loop_list.things_to_load.average_sigmas.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 categorical\optimized components\outliers removed\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.average_sigmas.filename= {'average_zscore_sigmas.mat'};
        parameters.loop_list.things_to_load.average_sigmas.variable= {'average_zscore_sigmas'}; 
        parameters.loop_list.things_to_load.average_sigmas.level = 'comparison';
        end
        
        % Output
        parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\level 2 categorical\optimized components\outliers removed\']};
        parameters.loop_list.things_to_save.fig.filename = {title};
        parameters.loop_list.things_to_save.fig.variable = {'PLSR_betas'};
        parameters.loop_list.things_to_save.fig.level = 'end';
        
        RunAnalysis({@PlotBetasSecondLevel}, parameters);
    end
end 
%close all;
clear i j true_false_vector;

%% *** LEVEL 2 CONTINUOUS ***

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

parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.max_mice = size(parameters.mice_all, 2);
parameters.concatenation_level = 'mouse';

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.response.variable= {'PLSR_results.BETA'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% Level 2 continuous -- optimize number of components
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

% Parameters for calculating best number of compo
% nents. If "findBestNComponents" = false, just run the ncomponents_max
parameters.findBestNComponents = true;
parameters.ncomponents_max = 6; 
parameters.contiguous_partitions = true; 
parameters.kFolds = 6;
parameters.MonteCarloReps = 6;
parameters.comparison_type = 'continuous';
parameters.run_with_max_components = true;
parameters.stratify = false;

% Do you want permutations?
parameters.permutationGeneration = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters); 

parameters.findBestNComponents = false;
parameters.run_with_max_components = false;

%% Level 2 continuous -- check components
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };
parameters.analysis_level = 2;
parameters.this_comparison_set = parameters.comparisons_continuous;
parameters.plot_weights = false;
parameters.plot_MSEPs = true;
parameters.plot_BICs = true;
parameters.plot_percentVars = false;

parameters.max_response_vars = 4;
parameters.analysis_level = 2;

% Input
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.variable= {'fig_MSEPs_explanatory'}; 
parameters.loop_list.things_to_save.fig_MSEPs_explanatory.level = 'end';

parameters.loop_list.things_to_save.fig_MSEPs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
parameters.loop_list.things_to_save.fig_MSEPs_response.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.fig_MSEPs_response.variable= {'fig_MSEPs_response'}; 
parameters.loop_list.things_to_save.fig_MSEPs_response.level = 'end';

parameters.loop_list.things_to_save.fig_BICs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
parameters.loop_list.things_to_save.fig_BICs_explanatory.filename= {'PLSR_BICs_explanatory.fig'};
parameters.loop_list.things_to_save.fig_BICs_explanatory.variable= {'fig_BICs_explanatory'}; 
parameters.loop_list.things_to_save.fig_BICs_explanatory.level = 'end';

parameters.loop_list.things_to_save.fig_BICs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
parameters.loop_list.things_to_save.fig_BICs_response.filename= {'PLSR_BICs_response.fig'};
parameters.loop_list.things_to_save.fig_BICs_response.variable= {'fig_BICs_response'}; 
parameters.loop_list.things_to_save.fig_BICs_response.level = 'end';

% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.filename= {'PLSR_PCTVARs_explanatory.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.variable= {'fig_PCTVARs_explanatory'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_explanatory.level = 'end';
% 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\MSEPs to ' num2str(parameters.ncomponents_max) '\']};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.filename= {'PLSR_PCTVARs_response.fig'};
% parameters.loop_list.things_to_save.fig_PCTVARs_response.variable= {'fig_PCTVARs_response'}; 
% parameters.loop_list.things_to_save.fig_PCTVARs_response.level = 'end';

RunAnalysis({@CheckComponents}, parameters);

close all;
 
%% Level 2 continuous -- prep shuffled datasets for PLSR on shuffles.
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

% Input 
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\results\level 1 continuous\optimized components\outliers removed\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.response.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_load.response.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_load.response.level = 'mouse';

% Output
parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info_randomPermutations.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

RunAnalysis({@DatasetPrepSecondLevel}, parameters);

%% Level 2 continuous -- PLSR on shuffles
% Run a second-level PLSR on the random shuffles from level 1. 

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

% Say that you do want to run on permutations
parameters.onPermutations = true;

% If the first level was categorical:
parameters.firstLevelCategorical = false; 

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info_randomPermutations.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'comparison';
% optimized number of components to use.
parameters.loop_list.things_to_load.ncomponents_max.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.ncomponents_max.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.ncomponents_max.variable= {'PLSR_results.ncomponents_used'}; 
parameters.loop_list.things_to_load.ncomponents_max.level = 'comparison';

% Output
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_save.betas_randomPermutations_2ndlevel.level = 'comparison';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);

parameters.onPermutations = false;

%% Level 2 continuous -- check significance

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };

parameters.evaluation_instructions = {{'data_evaluated = transpose(squeeze(parameters.test_values(1, :, :)));'}
                                      {'data_evaluated = squeeze(parameters.null_distribution(1, :, :));'}};
parameters.shufflesDim = 2; % After the EvaluateOnData reduction
parameters.find_significance = true;

% The statistical alpha value
parameters.alphaValue = 0.05; %/ numel(parameters.comparisons_continuous);

% If you want to fit a normal distribution before t-test (default = true)
parameters.useNormalDistribution = false; 

% Inputs:
% Test values (will grab only the intercepts with EvaluateOnData)
parameters.loop_list.things_to_load.test_values.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.test_values.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.test_values.variable= {'PLSR_results.BETA'}; 
parameters.loop_list.things_to_load.test_values.level = 'comparison';
% Null distribution
parameters.loop_list.things_to_load.null_distribution.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_load.null_distribution.filename= {'PLSR_betas_randomPermutations.mat'};
parameters.loop_list.things_to_load.null_distribution.variable= {'betas_randomPermutations'}; 
parameters.loop_list.things_to_load.null_distribution.level = 'comparison';

% Outputs
parameters.loop_list.things_to_save.significance.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
parameters.loop_list.things_to_save.significance.filename= {'PLSR_significance.mat'};
parameters.loop_list.things_to_save.significance.variable= {'PLSR_significance'}; 
parameters.loop_list.things_to_save.significance.level = 'comparison';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'test_values'}
                                          {'data_evaluated', 'null_distribution'}}; 

RunAnalysis({@EvaluateOnData, @EvaluateOnData, @SignificanceCalculation}, parameters);

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

parameters.evaluation_instructions = {{'if strcmp(parameters.values(2), parameters.this_comparison_set(parameters.values{3}).mice_not_to_use);'... % Skip mice not to use
                                       'data_evaluated = [];'...
                                       'else;'...
                                       'ysig = parameters.dataset.zscoring.responseVariables.sigma;'... 
                                       'xsig = repmat(parameters.dataset.zscoring.explanatoryVariables.sigma, size(ysig,2),1);'...  % Make dimensions match (replicate corrs so there's a set for each response varaible.
                                       'data_evaluated = reshape(transpose(transpose(ysig)./xsig), 1, []);'...
                                       'end;'}};
                                   
parameters.concatDim = 3;
parameters.concatenation_level = 'mouse';
parameters.averageDim = 3;
parameters.average_and_std_together = false;

% Input 
parameters.loop_list.things_to_load.dataset.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 1 continuous\outliers removed\'], 'comparison','\', 'mouse', '\'};
parameters.loop_list.things_to_load.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset.level = 'mouse';

% Output 
parameters.loop_list.things_to_save.average.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_save.average.filename= {'average_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.average.variable= {'average_zscore_sigmas'}; 
parameters.loop_list.things_to_save.average.level = 'comparison';

parameters.loop_list.things_to_save.std_dev.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\outliers removed\'], 'comparison','\'};
parameters.loop_list.things_to_save.std_dev.filename= {'std_dev_zscore_sigmas.mat'};
parameters.loop_list.things_to_save.std_dev.variable= {'std_dev_zscore_sigmas'}; 
parameters.loop_list.things_to_save.std_dev.level = 'comparison';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'};
                                         {}}; 

RunAnalysis({@EvaluateOnData, @ConcatenateData, @AverageData}, parameters);

%% Level 2 continuous -- plot betas
% Plot all the beta intercepts in a single plot 

% Do for each variation of significance & adjusted
true_false_vector = {false, true};
for i = 1:numel(true_false_vector)
    % Adjust beta values based on zscore sigmas?
    parameters.adjustBetas = true_false_vector{i};

    for j = 1:numel(true_false_vector) % No significance right now.
         % Only include significant betas?
         parameters.useSignificance = true_false_vector{j};

        % Always clear loop list first. 
        if isfield(parameters, 'loop_list')
        parameters = rmfield(parameters,'loop_list');
        end
        
        % Iterators
        parameters.loop_list.iterators = {
                       'comparison', {'loop_variables.comparisons_continuous(:).name'}, 'comparison_iterator' };
        
        parameters.max_response_vars = 4;

        % Color range for all plots (if betas are adjusted).
        parameters.useColorRange = false;
        
        % Comparison type (categorical or continuous, is just for plot titles)
        parameters.comparison_type = 'continuous';
        parameters.this_comparison_set = parameters.comparisons_continuous;
        
        % Make figure file title.
        title = '_betas_all_comparisons';
        if parameters.adjust_beta
            title = [title '_Adjusted'];
        end
        if parameters.useSignificance 
            title = [title '_withSignificance'];
        end
        title = [title '.fig'];
        
        % Input
        parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.results.filename = {'PLSR_results.mat'};
        parameters.loop_list.things_to_load.results.variable = {'PLSR_results'};
        parameters.loop_list.things_to_load.results.level = 'comparison';
        % significance matrix
        if parameters.useSignificance
        parameters.loop_list.things_to_load.significance.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\'], 'comparison', '\'};
        parameters.loop_list.things_to_load.significance.filename= {'PLSR_significance.mat'};
        parameters.loop_list.things_to_load.significance.variable= {'PLSR_significance.all'}; 
        parameters.loop_list.things_to_load.significance.level = 'comparison';
        end
        % average sigmas 
        parameters.loop_list.things_to_load.average_sigmas.dir = {[parameters.dir_exper 'PLSR\variable prep\datasets\level 2 continuous\optimized components\outliers removed\'], 'comparison','\'};
        parameters.loop_list.things_to_load.average_sigmas.filename= {'average_zscore_sigmas.mat'};
        parameters.loop_list.things_to_load.average_sigmas.variable= {'average_zscore_sigmas'}; 
        parameters.loop_list.things_to_load.average_sigmas.level = 'comparison';

        % Output
        parameters.loop_list.things_to_save.speed_fig.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\']};
        parameters.loop_list.things_to_save.speed_fig.filename = {['speed' title]};
        parameters.loop_list.things_to_save.speed_fig.variable = {'speed_betas'};
        parameters.loop_list.things_to_save.speed_fig.level = 'end';
        
        parameters.loop_list.things_to_save.accel_fig.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\']};
        parameters.loop_list.things_to_save.accel_fig.filename = {['accel' title]};
        parameters.loop_list.things_to_save.accel_fig.variable = {'accel_betas'};
        parameters.loop_list.things_to_save.accel_fig.level = 'end';
        
        parameters.loop_list.things_to_save.duration_fig.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\']};
        parameters.loop_list.things_to_save.duration_fig.filename = {['duration' title]};
        parameters.loop_list.things_to_save.duration_fig.variable = {'duration_betas'};
        parameters.loop_list.things_to_save.duration_fig.level = 'end';
        
        parameters.loop_list.things_to_save.pupil_diameter_fig.dir = {[parameters.dir_exper 'PLSR\results\level 2 continuous\optimized components\outliers removed\']};
        parameters.loop_list.things_to_save.pupil_diameter_fig.filename = {['pupil_diameter' title]};
        parameters.loop_list.things_to_save.pupil_diameter_fig.variable = {'pupil_diameter_betas'};
        parameters.loop_list.things_to_save.pupil_diameter_fig.level = 'end';
        
        RunAnalysis({@PlotBetasSecondLevel}, parameters);
    end 
end
clear i j true_false_vector;

%% Level 2 continuous across categories
% take difference of betas between 2 categories, concatenate, normalize
