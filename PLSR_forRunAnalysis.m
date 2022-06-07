% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress via RunAnalysis.m

function [parameters] = PLSR_forRunAnalysis(parameters)

    MessageToUser('Regressing ', parameters);

    % Run plsregress
    [results.XL, results.YL, results.XS, results.YS, results.BETA, results.PCTVAR, results.MSE, results.stats] ...
       = plsregress(parameters.explanatory, parameters.response, parameters.numComponents, 'cv', 5, 'Options', statset('UseParallel',true) );
    
    % Put into output structure;
    parameters.results = results; 

end 