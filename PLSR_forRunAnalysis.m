% PLSR_forRunAnalysis.m
% Sarah West
% 6/6/22

% A wrapper that runs the Matlab function plsregress_fullcode via RunAnalysis.m

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

        disp(['Will run ' num2str(parameters.n_permutations) ' permutations.'])

    end
     
    % Run plsregress_fullcode to find the optimal number of components, using a maximal number of components (somewhat
    % arbitrary)

    explanatoryVariables = parameters.dataset.explanatoryVariables;
    responseVariables = parameters.dataset.responseVariables;

    % If there are fewer observations than asked-for components (happens in
    % 2nd level analyses), lower ncomponents to 1 less than number of rows.
    if size(responseVariables, 1) <= parameters.ncomponents_max
        ncomponents_max = size(responseVariables, 1) - 1;

    else
        ncomponents_max = parameters.ncomponents_max;
    end

    % If user says so
    if isfield(parameters, 'findBestNComponents') && parameters.findBestNComponents
        [~, ~, ~, ~, ~, ~, MSEP_original, stats_original, MSEP_byVars_original] ...
           = plsregress_fullcode(explanatoryVariables, responseVariables, ncomponents_max, 'cv', parameters.crossValidationReps, 'mcreps', parameters.MonteCarloReps, 'Options', statset('UseParallel',true) );
        
        % Save the original weights of Y for later (in case you want to look at
        % what those components look like later)
        W_original = stats_original.W; 
    
        % Find component with minimum response-variable MSEP
        [~ , ncomponents] = min(MSEP_original(2,:));
        
        % MSEP is calculated for 0 components as well, so subtract 1 from
        % the index.
%         if ncomponents ~= 1
%             ncomponents = ncomponents - 1;
%         end 
        ncomponents = ncomponents_max;
        % Put MSE_original, ncomponents, & W_original into the results.
        results.maximal_components.MSEP = MSEP_original;
        results.maximal_components.MSEP_byVars = MSEP_byVars_original;
        results.maximal_components.W = W_original;
        results.ncomponents_used = ncomponents;

    % Otherwise, just run with ncomponents as ncomponents_max.
    else
        ncomponents = ncomponents_max;
        results.ncomponents_used = ncomponents;
        
    end

    % Now run with optimal (or user given) number of components.

    disp(['Running PLSR with ' num2str(ncomponents) ' components.']);

    % If this isn't a second-level comparison being run on previously
    % calculated random permutations, run just the one asked-for
    % regression.
    if ~isfield(parameters, 'onPermutations') || (isfield(parameters, 'onPermutations') && ~parameters.onPermutations)
   
        [results.XL, results.YL, results.XS, results.YS, results.BETA, results.PCTVAR, results.MSEP, results.stats, results.MSEP_byVars] ...
          = plsregress_fullcode(explanatoryVariables, responseVariables, ncomponents); 
   
         % Put results into output structure.
         parameters.results = results; 

    % If this IS a second-level comparison being run on previously
    % calculated random permutations, 
    else

        % Set up a holder that will hold the newly generated betas. (number
        % of mice + 1 for intercept x number of correlations x number of permutations)
        betas_randomPermutations_2ndlevel = NaN(size(responseVariables,1) + 1, size(responseVariables,2), size(responseVariables,3));

        % For each permutation,
        parfor repi = 1:size(responseVariables, 3)

            % Run PLSR regression.
            [~, ~, ~, ~, BETA] = plsregress_fullcode(explanatoryVariables, responseVariables, ncomponents);

            % Put into holder.
            betas_randomPermutations_2ndlevel(:, :, repi) = BETA;

        end

        % Put all betas into output structure.
        parameters.betas_randomPermutations_2ndlevel = betas_randomPermutations_2ndlevel;
    end

    % Run iterative permutations for permutation significance testing. Randomly
    % permute the order of the response variables. 

    % If user says so
    if isfield(parameters, 'permutationGeneration') && parameters.permutationGeneration

        disp('Running permutations'); 
        
        if isfield(parameters, 'comparison_type') && strcmp(parameters.comparison_type, 'categorical')
            columns_to_use = 1;
        else 
            columns_to_use = 1:size(responseVariables, 2);
        end

        % Make a holding matrix for beta permutations.
        betas_permutations = NaN(size(results.BETA,1), numel(columns_to_use), parameters.n_permutations);  %size(results.BETA, 2)

        parfor repi = 1:parameters.n_permutations 

            % For each response variable being looked at (want the diffent 
            % categories to vary independently), make a mixing vector that's
            % made up of a random permutation of the number of periods
            % included.

            % Make a holder for mixed response variables
            responseVariables_mixed = NaN(size(responseVariables, 1), numel(columns_to_use));

            for variablei = 1:numel(columns_to_use)     %(responseVariables, 2)
                column = columns_to_use(variablei);

                % Randomize order
                vect_mix = randperm(size(responseVariables, 1));
    
                % Mix/permute response varables for this category's columns
                % only. 
                responseVariables_mixed(:, column) = responseVariables(vect_mix, column);

            end

            % Run the plsregress_fullcode on the mixed/permuted data.
            [~, ~, ~, ~, BETA] = plsregress_fullcode(explanatoryVariables, responseVariables_mixed, ncomponents);
            
            % Put into holding matrix.
            betas_permutations(:, :, repi) = BETA; 
            
        end 

        % Put betas_permutations into output structure
        parameters.betas_randomPermutations = betas_permutations;

    end 
end 