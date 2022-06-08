% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress via RunAnalysis.m

function [parameters] = PLSR_forRunAnalysis(parameters)

    MessageToUser('Regressing ', parameters);

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

    % transpose

    % concatenate
    
    % zscore
     

    % Run plsregress, using a maximal number of components (somewhat
    % arbitrary -- start with 20)
    ncomponents_max = 20; 

    [~, ~, ~, ~, ~, ~, MSE_original, stats_original] ...
       = plsregress(brainData, responseVariables, ncomponents_max, 'cv', 10, 'mcreps', 10, 'Options', statset('UseParallel',true) );
    
    % Save the original weights of Y for later (in case you want to look at
    % what those components look like later)
    W_original = stats_original.W;

    % Find component with minimum MSE
    [ncomponents] = min(MSE_original);

    % Now run with optimal number of components.
    [results.XL, results.YL, results.XS, results.YS, results.BETA, results.PCTVAR, results.MSE, results.stats] ...
       = plsregress(brainData, responseVariables, ncomponents, 'cv', 10, 'mcreps', 10, 'Options', statset('UseParallel',true) );
    
    % Put MSE_original, ncomponents, & W_original into the results.
    results.maximal_components.MSE = MSE_original;
    results.maximal_components.W = W_original;
    results.best_ncomponents = ncomponents; 

    % Put into output structure;
    parameters.results = results; 

end 