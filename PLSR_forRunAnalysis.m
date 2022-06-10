% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress via RunAnalysis.m

function [parameters] = PLSR_forRunAnalysis(parameters)

    MessageToUser('PLS regressing ', parameters);

    % Tell user all the parameters they sent in (so they can cancel before 
    % it runs for a long time if they made a mistake).
    if isfield(parameters, 'findBestNComponents') && parameters.findBestNComponents
        message = ['Finding best number of components with maximum ' num2str(parameters.ncomponents_max) ...
            ' + 1 components, ' num2str(parameters.crossValidationReps) ' cross-validation reps, ' ...
             num2str(parameters.MonteCarloReps) , ' Monte Carlo repetitions'];
        disp(message);
    end

    if isfield(parameters, 'permutationGeneration') && parameters.permutationGeneration

        disp(['Will run ' num2str(parameters.n_permutaions) ' permutations.'])

    end

    % Find the location & value of the comparison iterator in
    % parameters.values
    iterator_location = find(cellfun(@strcmp, parameters.keywords, repmat({'comparison'}, size(parameters.keywords)))); 
    comparison_iterator = parameters.values{iterator_location};

    % Grab comparison name, indices, variables to use.
    %comparison_name = parameters.comparisons(comparison_iterator).name; % Might not need (is really only for saving files)
    comparison_indices = parameters.comparisons(comparison_iterator).indices;
    comparison_variablesToUse = parameters.comparisons(comparison_iterator).variablesToUse; 

    % Get the brain data period indices you're interested in. 
    brainData = parameters.brain_data(comparison_indices);

    % Get the response variables period indices & variables you're
    % interested in. 
    % responseVariables is now an array
    responseVariables = parameters.response_variables{comparison_indices, [comparison_variablesToUse{:}]}; 
   
    % Reshape brainData to make each roll into an instance, inside each period cell. 
    % The 2 reshape inputs 
    input1 =  repmat({size(brainData{1}, 1)}, size(brainData));
    input2 = repmat({[]}, size(brainData));
    brainData = cellfun(@reshape, brainData, input1, input2, 'UniformOutput', false);
 
    % For each response variable (each column of responseVariables),
    % reshape to make each roll into an instance, inside each period cell.
    for variablei = 1:numel(comparison_variablesToUse)
        
        % The 2 reshape inputs
        input1 =  repmat({size(responseVariables{1, variablei}, 1)}, size(brainData));
        input2 = repmat({[]}, size(brainData));
        
        % Reshape
        responseVariables(:, variablei) =  cellfun(@reshape, responseVariables(:, variablei), input1, input2, 'UniformOutput', false);

    end 

    % **Transpose
    brainData = cellfun(@transpose, brainData, 'UniformOutput', false);
    responseVariables = cellfun(@transpose, responseVariables, 'UniformOutput', false);

    % **Concatenate brain data & response variables vertically across periods. 

    % brainData has only one cell array column, so can be done all at once.
    brainData = vertcat(brainData{:});

    % Each variable in responseVariables is own cell array column, so have
    % to do each of those first before horizontally concatenating the
    % different variables into  same matrix. Keep the separated variables
    % for later.
    responseVariables_separateVariables = cell(1, numel(comparison_variablesToUse));
    for variablei =  1:numel(comparison_variablesToUse)
        responseVariables_separateVariables{variablei} = vertcat(responseVariables{:, variablei});
    end
   
    % **Remove unnecessary columns of variables.
    for variablei =  1:numel(comparison_variablesToUse)

        % Find if there are AREN'T any non-zero elements in each column.
        columns_to_remove = ~any(responseVariables_separateVariables{variablei});

        % If there are columns to remove, remove them.
        if ~isempty(columns_to_remove)
            responseVariables_separateVariables{variablei}(:, columns_to_remove) = [];  
        end
    end

    % Get number of remaining response columns per category, for permuting
    % independently later. 
    variable_category_column_numbers = cellfun(@size, responseVariables_separateVariables, 2);

    columns_to_use = cell(1, numel(comparison_variablesToUse)); 
    column_counter = 0;
    for variablei = 1:numel(comparison_variablesToUse)
        columns_to_use{variablei} = column_counter + 1:variable_category_column_numbers(variablei); % Defined this separately for clarity.
        column_counter = column_counter + variable_category_column_numbers(variablesi);
    end

    % Horizontally concatenate the different response variable categories.
    responseVariables = horzcat(responseVariables_separateVariables{:}); 

    % Zscore both variable sets. Keep mu & sigmas for better interprebility
    % of betas later.
    [brainData, mu_brain, sigma_brain] = zscore(brainData);
    [responseVariables, mu_response, sigma_response] = zscore(responseVariables);
    dataset.zscoring.brainData.mu = mu_brain;
    dataset.zscoring.brainData.sigma = sigma_brain;
    dataset.zscoring.responseVariables.mu = mu_response;
    dataset.zscoring.responseVariables.sigma = sigma_response;

    % For convenience, put both variable sets for this comparison into a stucture for saving. 
    %dataset.brainData = brainData;
    %dataset.responseVariables = responseVariables;
    dataset.responseVariables_separateVariables = responseVariables_separateVariables;
    dataset.variable_category_column_numbers = variable_category_column_numbers; % For telling which columns belong to which category later
     
    % Run plsregress to find the optimal number of components, using a maximal number of components (somewhat
    % arbitrary)

    % If user says so
    if isfield(parameters, 'findBestNComponents') && parameters.findBestNComponents
        [~, ~, ~, ~, ~, ~, MSEP_original, stats_original] ...
           = plsregress(brainData, responseVariables, parameters.ncomponents_max, 'cv', parameters.crossValidationReps, 'mcreps', parameters.MonteCarloReps, 'Options', statset('UseParallel',true) );
        
        % Save the original weights of Y for later (in case you want to look at
        % what those components look like later)
        W_original = stats_original.W; 
    
        % Find component with minimum response-variable MSEP
        [~ , ncomponents] = min(MSEP_original(2,:));
    
        % Put MSE_original, ncomponents, & W_original into the results.
        results.maximal_components.MSEP = MSEP_original;
        results.maximal_components.W = W_original;
        results.ncomponents_used = ncomponents;

    % Otherwise, just run with ncomponents as parameters.ncomponents_max.
    else
        ncomponents = parameters.ncomponents_max;
        results.ncomponents_used = ncomponents;
        
    end

    % Now run with optimal (or user given) number of components.

    disp(['Running PLSR with ' num2str(ncomponents) ' components.']);

    [results.XL, results.YL, results.XS, results.YS, results.BETA, results.PCTVAR, results.MSEP, results.stats] ...
       = plsregress(brainData, responseVariables, ncomponents); 

    % Run iterative permutations for permutation significance testing. Randomly
    % permute the order of the response variables. 

    % If user says so
    if isfield(parameters, 'permutationGeneration') && parameters.permutationGeneration

        disp('Running permutations'); 

        % Make a holding matrix for beta permutations.
        betas_permutations = NaN(size(results.BETA,1), size(restuls.BETA, 2), parameters.n_permutations); 

        parfor repi = 1:parameters.n_permutaions 

            % For each response variable being looked at (want the diffent 
            % categories to vary independently), make a mixing vector that's
            % made up of a random permutation of the number of periods
            % included.

            % Make a holder for mixed response variables
            responseVariables_mixed = NaN(size(responseVariables));

            for variablei = 1:numel(comparison_variablesToUse)
                
                % Randomize order
                vect_mix = randperm(1:size(responseVariables, 1));
    
                % Mix/permute response varables for this category's columns
                % only. 
                responseVariables_mixed(:, columns_to_use{variablei}) = responseVariables(vect_mix, columns_to_use{variablei});

            end

            % Run the plsregress on the mixed/permuted data.
            [~, ~, ~, ~, BETA] = plsregress(brainData, responseVariables_mixed, ncomponents);
            
            % Put into holding matrix.
            betas_permutations(:, :, repi) = BETA; 
            
        end 

        % Put betas_permutations into results structure.
        results.betas_permutations = betas_permutations;

    end 

    parameters.results = results; 
    parameters.dataset = dataset;

end 