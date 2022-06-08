% pipeline_partial_least_squares.m
% Sarah West
% 6/4/22 
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
parameters.mice_all = parameters.mice_all([1:6 8]);

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

% Put relevant variables into loop_variables.
parameters.loop_variables.mice_all = parameters.mice_all;
parameters.loop_variables.periods = periods.condition; 
parameters.loop_variables.conditions = {'motorized'; 'spontaneous'};
parameters.loop_variables.conditions_stack_locations = {'stacks'; 'spontaneous'};
parameters.loop_variables.variable_type = {'response variables', 'correlations'};

%% Create periods_nametable_forPLSR.m
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

%% Put in response variables. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Variables to replicate
parameters.response_variable_names = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'speed_vector', 'accel_vector', 'duration_vector'};
parameters.variables_static = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'duration_vector'};
parameters.motorized_variables_static = {'speed_vector', 'accel_vector'}; % These are the ones that are static in motorized, not static in spontaneous

% Original order of spontaneous (for velocity & accel indexing)
parameters.spontaneous_periods_order = {'rest', 'walk', 'prewalk', 'startwalk', 'stopwalk', 'postwalk'};

parameters.concatenate_vertically = true;

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
parameters.loop_list.things_to_save.response_variables.filename= {'values.mat'};
parameters.loop_list.things_to_save.response_variables.variable= {'values'}; 
parameters.loop_list.things_to_save.response_variables.level = 'mouse';

RunAnalysis({@PopulateResponseVariables}, parameters);

%% Reshape & concatenate data across periods within mice.
% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'variable_type', {'loop_variables.variable_type'}, 'vaiable_type_iterator';
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'period', {'loop_variables.periods'}, 'period_iterator';            
               };

% Dimensions for reshaping, before removing data & before cnocatenation.
% Turning it into 2 dims. 
parameters.toReshape = {'parameters.data'};
parameters.reshapeDims = {'{size(parameters.data, 1), []}'};

% Concatenation dimension (post reshaping & removal)
parameters.concatDim = 2; 
parameters.concatenate_across_cells = false; 

% Input 
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\'] 'variable_type', '\', 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'values.mat'};
parameters.loop_list.things_to_load.data.variable= {'values{', 'period_iterator', '}'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.concatenated_data.dir = {[parameters.dir_exper 'PLSR\variable prep\'], 'variable_type', '\', 'mouse', '\'};
parameters.loop_list.things_to_save.concatenated_data.filename= {'values_all_concatenated.mat'};
parameters.loop_list.things_to_save.concatenated_data.variable= {'values_all_concatenated'}; 
parameters.loop_list.things_to_save.concatenated_data.level = 'mouse';

parameters.loop_list.things_to_save.concatenated_origin.dir = {[parameters.dir_exper 'PLSR\variable prep\'] 'variable_type', '\', 'mouse', '\'};
parameters.loop_list.things_to_save.concatenated_origin.filename= {'values_all_concatenated_origin.mat'};
parameters.loop_list.things_to_save.concatenated_origin.variable= {'values_origin'}; 
parameters.loop_list.things_to_save.concatenated_origin.level = 'mouse';

% Things to rename/reassign between the two functions (frist column renamed to
% second column. Pairs for each level. Each row is a level.
parameters.loop_list.things_to_rename = {{'data_reshaped', 'data'}};

RunAnalysis({@ReshapeData, @ConcatenateData}, parameters);  

%% For correlations: Transpose, remove mean by mouse, concatenate across mice.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Evaluate (transposing-> now observations x correlations)
parameters.evaluation_instructions = {'data_evaluated = transpose(parameters.data);'};

% Averaging (post transposing)
parameters.averageDim = 1; % row vector of mean of each column/each correlation

% Concatenation dimension (post reshaping, across observarions)
parameters.concatDim = 1; 

% Input
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\'], 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'values_all_concatenated.mat'};
parameters.loop_list.things_to_load.data.variable= {'values_all_concatenated'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.concatenated_data.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\concatenated across mice\']};
parameters.loop_list.things_to_save.concatenated_data.filename= {'correlations_all_concatenated.mat'};
parameters.loop_list.things_to_save.concatenated_data.variable= {'correlations_all_concatenated'}; 
parameters.loop_list.things_to_save.concatenated_data.level = 'end';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'}; % Evaluate to Average
                                         {'average', 'subtract_this'; 'data', 'subtract_from_this'}; % Average to subtraction. Are both on same function level.
                                         {'data_subtracted', 'data'}}; % Subtraction to concatenation
                                                                   
RunAnalysis({@EvaluateOnData, @AverageData, @SubtractData, @ConcatenateData}, parameters);

%% For response: Transpose, concatenate across mice.
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Evaluate (transposing-> now observations x correlations)
parameters.evaluation_instructions = {'data_evaluated = transpose(parameters.data);'};

% Concatenation dimension (post reshaping, across observarions)
parameters.concatDim = 1; 

% Input
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\'], 'mouse', '\'};
parameters.loop_list.things_to_load.data.filename= {'values_all_concatenated.mat'};
parameters.loop_list.things_to_load.data.variable= {'values_all_concatenated'}; 
parameters.loop_list.things_to_load.data.level = 'mouse';

% Output
parameters.loop_list.things_to_save.concatenated_data.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\concatenated across mice\']};
parameters.loop_list.things_to_save.concatenated_data.filename= {'responses_all_concatenated.mat'};
parameters.loop_list.things_to_save.concatenated_data.variable= {'responses_all_concatenated'}; 
parameters.loop_list.things_to_save.concatenated_data.level = 'end';

parameters.loop_list.things_to_rename = {{'data_evaluated', 'data'}}; % Evaluate to concatenations                                        
                                                                   
RunAnalysis({@EvaluateOnData, @ConcatenateData}, parameters);

%% 
% Take zscore. Run response & correlations separately so you have
% different variable names for loading into PLS regression, just in case.

%% Zscore correlations
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.zscoreDim = 1; % 1 = zscore by columns (variables) 

% Input 
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\concatenated across mice\']};
parameters.loop_list.things_to_load.data.filename= {'correlations_all_concatenated.mat'};
parameters.loop_list.things_to_load.data.variable= {'correlations_all_concatenated'}; 
parameters.loop_list.things_to_load.data.level = 'start';

% Output 
parameters.loop_list.things_to_save.data_zscored.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\concatenated across mice\']};
parameters.loop_list.things_to_save.data_zscored.filename= {'correlations_zscored.mat'};
parameters.loop_list.things_to_save.data_zscored.variable= {'correlations'}; 
parameters.loop_list.things_to_save.data_zscored.level = 'end';

RunAnalysis({@ZScoreData}, parameters);

%% Zscore response
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.zscoreDim = 1; % 1 = zscore by columns (variables) 

% Input 
parameters.loop_list.things_to_load.data.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\concatenated across mice\']};
parameters.loop_list.things_to_load.data.filename= {'responses_all_concatenated.mat'};
parameters.loop_list.things_to_load.data.variable= {'responses_all_concatenated'}; 
parameters.loop_list.things_to_load.data.level = 'start';

% Output 
parameters.loop_list.things_to_save.data_zscored.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\concatenated across mice\']};
parameters.loop_list.things_to_save.data_zscored.filename= {'responses_zscored.mat'};
parameters.loop_list.things_to_save.data_zscored.variable= {'responses'}; 
parameters.loop_list.things_to_save.data_zscored.level = 'end';

RunAnalysis({@ZScoreData}, parameters);

%% Run PLS regression

if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.numComponents = 30;

% Input 
% Response variables
parameters.loop_list.things_to_load.response.dir = {[parameters.dir_exper 'PLSR\variable prep\response variables\concatenated across mice\']};
parameters.loop_list.things_to_load.response.filename= {'responses_zscored.mat'};
parameters.loop_list.things_to_load.response.variable= {'responses'}; 
parameters.loop_list.things_to_load.response.level = 'start';
% Correlations 
parameters.loop_list.things_to_load.explanatory.dir = {[parameters.dir_exper 'PLSR\variable prep\correlations\concatenated across mice\']};
parameters.loop_list.things_to_load.explanatory.filename= {'correlations_zscored.mat'};
parameters.loop_list.things_to_load.explanatory.variable= {'correlations'}; 
parameters.loop_list.things_to_load.explanatory.level = 'start';

% Output 
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'results'}; 
parameters.loop_list.things_to_save.results.level = 'end';

RunAnalysis({@PLSR_forRunAnalysis}, parameters);

%% Plot some X loadings

if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.components_to_plot = 1:30; 
parameters.color_range = [-200 200];

% Input 
parameters.loop_list.things_to_load.XL.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_load.XL.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.XL.variable= {'results.XL'}; 
parameters.loop_list.things_to_load.XL.level = 'start';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_XLs.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'end';

RunAnalysis({@PlotXLs}, parameters);

%% Plot some Beta values
% First row of Betas is constant estimate.

if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.components_to_plot = {'motorized', 'spontaneous','rest', 'walk', 'start', 'stop', 'accel', 'decel', 'finished', 'speed', 'accel', 'duration'};
%parameters.color_range = [-0.03 0.03];

% Input 
parameters.loop_list.things_to_load.BETA.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_load.BETA.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.BETA.variable= {'results.BETA'}; 
parameters.loop_list.things_to_load.BETA.level = 'start';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_Betas.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'end';

RunAnalysis({@PlotBetas}, parameters);

%% Plot Projections
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = 'none';

parameters.components_to_plot = {'motorized', 'spontaneous','rest', 'walk', 'start', 'stop', 'accel', 'decel', 'finished', 'speed', 'acceleration rate', 'duration'};

% Input 
parameters.loop_list.things_to_load.results.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_load.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_load.results.variable= {'results'}; 
parameters.loop_list.things_to_load.results.level = 'start';

% Output
parameters.loop_list.things_to_save.fig.dir = {[parameters.dir_exper 'PLSR\results\']};
parameters.loop_list.things_to_save.fig.filename= {'PLSR_Projections.fig'};
parameters.loop_list.things_to_save.fig.variable= {'fig'}; 
parameters.loop_list.things_to_save.fig.level = 'end';

RunAnalysis({@PlotProjections}, parameters);

%% **********************************************************************
% Try this all again but with the multi-level approach.

%% %% Put in response variables, no vertical concatenation

if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'};

% Variables to replicate
parameters.response_variable_names = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'speed_vector', 'accel_vector', 'duration_vector'};
parameters.variables_static = {'motorized_vs_spon_dummyvars_vector', 'type_dummyvars_vector', 'duration_vector'};
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

%% Multi-level -- Level 1 (speed, accel rate, duration within each condition & type per mouse)
% need to make a special function for this

% Always clear loop list first. 
if isfield(parameters, 'loop_list')
parameters = rmfield(parameters,'loop_list');
end

% Iterators
parameters.loop_list.iterators = {
               'mouse', {'loop_variables.mice_all(:).name'}, 'mouse_iterator'; 
               'motorized_vs_spon', {'loop_variables.categories.motorized_vs_spon'}, 'motorized_vs_spon_iterator';
               'type', {'loop_variables.categories.type'}, 'type_iterator';        
               };

parameters.loop_variables.response_variables = parameters.response_variable_names;

% No regressions on rest by itself.
% Walk only regress on speed
% finished only regress on speed & duration.
% start, stop, accel, decel regress on all 

% The level of multi-level regression you're running.
parameters.PLSR_level = 1; 

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
parameters.loop_list.things_to_save.results.dir = {[parameters.dir_exper 'PLSR\multilevel results\level 1\'], 'motorized_vs_spon', '\', 'type', '\' 'mouse', '\'};
parameters.loop_list.things_to_save.results.filename= {'PLSR_results.mat'};
parameters.loop_list.things_to_save.results.variable= {'PLSR_results'}; 
parameters.loop_list.things_to_save.results.level = 'type';

RunAnalysis({@PLSR_Multilevel}, parameters);  

%% Plot the Betas, XLs, Projections for all level 1s

%% Multi-level -- Level 2 (effect of type on Betas from level 1)
% Betas from level 1 are now the predictors, not the brain data. 
% [Does that mean there are now  (# continuous response variables) * (496 + 1 (for intercept)) total predictors?]    
% Different mice = different observations
% Do for motorized & spontaneous each.


%% Multi-level-- Level 3 (ish)
% Effect of motorized vs spontaneous.
% only have 1 beta matrix for each of these, right? Can just take the
% difference...?
