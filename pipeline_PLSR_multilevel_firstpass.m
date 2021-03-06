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
parameters.mice_all = parameters.mice_all([1:6, 8]);

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

end

% Load comparisons for first level, if it exists yet.
if isfile([parameters.dir_exper 'PLSR\comparisons_firstlevel.mat'])
    load([parameters.dir_exper 'PLSR\comparisons_firstlevel.mat']);
    parameters.comparisons_firstlevel = comparisons;
end

% Change here if you only want to do some comparisons.
parameters.comparisons_firstlevel = parameters.comparisons_firstlevel;

% Put relevant variables into loop_variables.
parameters.loop_variables.mice_all = parameters.mice_all;
parameters.loop_variables.periods = periods.condition; 
parameters.loop_variables.conditions = {'motorized'; 'spontaneous'};
parameters.loop_variables.conditions_stack_locations = {'stacks'; 'spontaneous'};
parameters.loop_variables.variable_type = {'response variables', 'correlations'};
parameters.loop_variables.comparisons_firstlevel = parameters.comparisons_firstlevel;

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
end

%% Create comparisons_firstlevel.mat
% If it hasn't been created already.
if ~isfile([parameters.dir_exper 'PLSR\comparisons_firstlevel.mat'])
    create_periods_nametable_forPLSR
    load([parameters.dir_exper 'PLSR\comparisons_firstlevel.mat']);
    parameters.comparisons_firstlevel = comparisons; 
    parameters.loop_variables.comparisons_firstlevel = comparisons;
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
parameters.response_variable_names = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'transition_or_not_dummyvars_vector', 'speed_vector', 'accel_vector', 'duration_vector'};
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

% Output 
parameters.loop_list.things_to_save.response_variables.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_save.response_variables.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_save.response_variables.variable= {'response_variables'}; 
parameters.loop_list.things_to_save.response_variables.level = 'mouse';

RunAnalysis({@PopulateResponseVariables}, parameters);

%% Level 1, continuous

% Run a first-pass to see the results before you run any permutations.
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

% Do you want permutations?
parameters.permutationGeneration = false;

% Parameters for calculating best number of components. If
% "findBestNComponents" = false, just run the ncomponents_max
parameters.findBestNComponents = false;
parameters.ncomponents_max = 2; 
parameters.crossValidationReps = 10;
parameters.MonteCarloReps = 10;

% Input 
parameters.loop_list.things_to_load.response_variables.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.response_variables.filename= {'response_variables_table.mat'};
parameters.loop_list.things_to_load.response_variables.variable= {'response_variables'}; 
parameters.loop_list.things_to_load.response_variables.level = 'mouse';

parameters.loop_list.things_to_load.brain_data.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.brain_data.filename= {'values.mat'};
parameters.loop_list.things_to_load.brain_data.variable= {'values'}; 
parameters.loop_list.things_to_load.brain_data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'comparison';

parameters.loop_list.things_to_save.dataset.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.dataset.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_save.dataset.variable= {'dataset_info'}; 
parameters.loop_list.things_to_save.dataset.level = 'comparison';

profile off
profile on
RunAnalysis({@PLSR_forRunAnalysis}, parameters);  
profile viewer


%% Plot Betas
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 2; 

% Adjust beta values based on zscore sigmas?
parameters.adjust_beta = false;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results_withResub.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Also load in dataset values for the zscore sigma.
parameters.loop_list.things_to_load.dataset_info.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.dataset_info.filename= {'PLSR_dataset_info.mat'};
parameters.loop_list.things_to_load.dataset_info.variable= {'dataset_info'}; 
parameters.loop_list.things_to_load.dataset_info.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_betas.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotBetas}, parameters);

close all;

%% Plot weights
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };


parameters.ncomponents_max = 20; 

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_weights.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotWeights}, parameters);

close all;

%% Plot MSEPs per mouse, up to 20 components.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 20; 

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Output
parameters.loop_list.things_to_save.xfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.xfig.filename= {'PLSR_MSEPs_explanatory.fig'};
parameters.loop_list.things_to_save.xfig.variable= {'xfig'}; 
parameters.loop_list.things_to_save.xfig.level = 'mouse';

parameters.loop_list.things_to_save.yfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.yfig.filename= {'PLSR_MSEPs_response.fig'};
parameters.loop_list.things_to_save.yfig.variable= {'yfig'}; 
parameters.loop_list.things_to_save.yfig.level = 'mouse';

RunAnalysis({@PlotMSEPs}, parameters);

close all;

%% Plot XLs, up to 20 components
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 20; 


% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';


% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max)  ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_XLs.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'comparison';

RunAnalysis({@PlotXLs}, parameters);

close all;


%% Plot percent variance per mouse
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 20;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Output
parameters.loop_list.things_to_save.xfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.xfig.filename= {'PLSR_PCTVAR_explanatory.fig'};
parameters.loop_list.things_to_save.xfig.variable= {'xfig'}; 
parameters.loop_list.things_to_save.xfig.level = 'mouse';

parameters.loop_list.things_to_save.yfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.yfig.filename= {'PLSR_PCTVAR_response.fig'};
parameters.loop_list.things_to_save.yfig.variable= {'yfig'}; 
parameters.loop_list.things_to_save.yfig.level = 'mouse';

RunAnalysis({@PlotPCTVAR}, parameters);

close all;

%% Plot percent variance per mouse -- continuous vars only
% (need to switch the comparisons up top to 1:11 or whatever)
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 20;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Output
parameters.loop_list.things_to_save.xfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.xfig.filename= {'PLSR_PCTVAR_explanatory_continuousOnly.fig'};
parameters.loop_list.things_to_save.xfig.variable= {'xfig'}; 
parameters.loop_list.things_to_save.xfig.level = 'mouse';

parameters.loop_list.things_to_save.yfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.yfig.filename= {'PLSR_PCTVAR_response_conctinuousOnly.fig'};
parameters.loop_list.things_to_save.yfig.variable= {'yfig'}; 
parameters.loop_list.things_to_save.yfig.level = 'mouse';

RunAnalysis({@PlotPCTVAR}, parameters);

close all;

% Plot percent variance per mouse -- continuous vars only
% (need to switch the comparisons up top to 1:11 or whatever)
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'comparison', {'loop_variables.comparisons_firstlevel(:).name'}, 'comparison_iterator'     
               };

parameters.ncomponents_max = 20;

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\'], 'comparison', '\' 'mouse', '\'};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_load.results.level = 'comparison';

% Output
parameters.loop_list.things_to_save.xfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.xfig.filename= {'PLSR_MSEPS_explanatory_continuousOnly.fig'};
parameters.loop_list.things_to_save.xfig.variable= {'xfig'}; 
parameters.loop_list.things_to_save.xfig.level = 'mouse';

parameters.loop_list.things_to_save.yfig.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1 with max ' num2str(parameters.ncomponents_max) ' components\MSEPs to 20\'], 'mouse', '\'};
parameters.loop_list.things_to_save.yfig.filename= {'PLSR_MSEPS_response_continuousOnly.fig'};
parameters.loop_list.things_to_save.yfig.variable= {'yfig'}; 
parameters.loop_list.things_to_save.yfig.level = 'mouse';

RunAnalysis({@PlotMSEPs}, parameters);

close all;

%% Subtract continuous variables effects from each behavior type. 


%% Level 1 categorical. 


%% Level 2 continuous. 

%% Level 2 categorical.

%% Take differences between level 1 continuous 


%% Level 2 categorical against differences in continuous. 

