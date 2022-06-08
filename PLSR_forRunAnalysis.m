% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress via RunAnalysis.m

function [parameters] = PLSR_forRunAnalysis(parameters)

    MessageToUser('Regressing ', parameters);

    % Run plsregress, using a maximal number of components (somewhat
    % arbitrary -- start with 20)
    ncomponents_original = 20; 

    [~, ~, ~, ~, ~, ~, MSE_original, stats_original] ...
       = plsregress(parameters.explanatory, parameters.response, ncomponents_original, 'cv', 5, 'Options', statset('UseParallel',true) );
    
    % Save the original weights of Y for later (in case you want to look at
    % what those components look like later)
    W_original = stats_original.W;

    % Find component with minimum MSE
    [ncomponents] = min(MSE_original);

    % Now run with optimal number of components.
    [results.XL, results.YL, results.XS, results.YS, results.BETA, results.PCTVAR, results.MSE, results.stats] ...
       = plsregress(parameters.explanatory, parameters.response, ncomponents, 'cv', 5, 'Options', statset('UseParallel',true) );
    
    % Put MSE_original, ncomponents, & W_original into the results.
    results.maximal_components.MSE = MSE_original;
    results.maximal_components.W = W_original;
    results.best_ncomponents = ncomponents; 

    % Put into output structure;
    parameters.results = results; 

end 