% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress via RunAnalysis.m

function [parameters] = PLSR_forRunAnalysis(parameters)

    MessageToUser('PLS regressing ', parameters);

    % Tell user all the parameters they sent in (so they can cancel before 
    % it runs for a long time if they made a mistake).
    if isfield(parameters, 'findBestNComponents') && parameters.findBestNComponents

        if isnumeric(parameters.crossValidationReps)
       
        message = ['Finding best number of components with maximum ' num2str(parameters.ncomponents_max) ...
            ' components, ' num2str(parameters.crossValidationReps) ' cross-validation reps, ' ...
             num2str(parameters.MonteCarloReps) , ' Monte Carlo repetitions'];
        disp(message);
        end

    end

    if isfield(parameters, 'permutationGeneration') && parameters.permutationGeneration

        disp(['Will run ' num2str(parameters.n_permutaions) ' permutations.'])

    end
     
    % Run plsregress to find the optimal number of components, using a maximal number of components (somewhat
    % arbitrary)

    explanatoryVariables = parameters.dataset.explanatoryVariables;
    responseVariables = parameters.dataset.responseVariables;

    % If user says so
    if isfield(parameters, 'findBestNComponents') && parameters.findBestNComponents
        [~, ~, ~, ~, ~, ~, MSEP_original, stats_original] ...
           = plsregress(explanatoryVariables, responseVariables, parameters.ncomponents_max, 'cv', parameters.crossValidationReps, 'mcreps', parameters.MonteCarloReps, 'Options', statset('UseParallel',true) );
        
        % Save the original weights of Y for later (in case you want to look at
        % what those components look like later)
        W_original = stats_original.W; 
    
        % Find component with minimum response-variable MSEP
        [~ , ncomponents] = min(MSEP_original(2,:));
        
        % MSEP is calculated for 0 components as well, so subtract 1 from
        % the index.
        if ncomponents ~= 1
            ncomponents = ncomponents - 1;
        end 

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
       = plsregress(explanatoryVariables, responseVariables, ncomponents); 

    % Run iterative permutations for permutation significance testing. Randomly
    % permute the order of the response variables. 

    % If user says so
    if isfield(parameters, 'permutationGeneration') && parameters.permutationGeneration

        disp('Running permutations'); 

        columns_to_use = parameters.dataset.columns_to_use;

        % Make a holding matrix for beta permutations.
        betas_permutations = NaN(size(results.BETA,1), size(results.BETA, 2), parameters.n_permutations); 

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
            [~, ~, ~, ~, BETA] = plsregress(explanatoryVariables, responseVariables_mixed, ncomponents);
            
            % Put into holding matrix.
            betas_permutations(:, :, repi) = BETA; 
            
        end 

        % Put betas_permutations into results structure.
        results.betas_permutations = betas_permutations;

    end 

    % Put results into output structure.
    parameters.results = results; 
end 