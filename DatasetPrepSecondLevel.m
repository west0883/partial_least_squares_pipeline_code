% DatasetPrepSecondLevel.m
% Sarah West
% 6/17/22

% Reshapes betas of first-level comparisons, concatenates across mice.
% Creates explanatory dummy variables for mice. 

function [parameters] = DatasetPrepSecondLevel(parameters)


    MessageToUser('Prepping ' , parameters);
    
    % Find the location & value of the comparison iterator in
    % parameters.values
    iterator_location = strcmp(parameters.keywords, 'comparison_iterator'); 
    comparison_iterator = parameters.values{iterator_location};

    % *** Deal with mice that should be skipped for this comparison ***
    
    % If "mice_not_to_use" is a field in this comparison set,
    if isfield(parameters.this_comparison_set(comparison_iterator), 'mice_not_to_use')
        
        % Get the list of mice not to use in this comparison (if any)
        mice_not_to_use = parameters.this_comparison_set(comparison_iterator).mice_not_to_use;
    
        % Get the mouse of this iteration
        mouse_location = strcmp(parameters.keywords, 'mouse'); 
        mouse = parameters.values{mouse_location};
    
        % If mice_not_to_use is not empty
        if ~isempty(mice_not_to_use)
    
            % Check if current mouse is included in the list of mice not to
            % use. If it is, exit function.
            if any(cellfun(@strcmp, mice_not_to_use, repmat({mouse}, size(mice_not_to_use)))) 
                
                % Tell RunAnalysis not to save anything this iteration.
                parameters.dont_save = true;
    
                % Leave this function-- will move on to next comparison
                return
            end 
        end
    end
    
    % *** Handle the response variables (betas)*** 

    % Do differently if it's on a regular first-level set vs if it's on a
    % random permutation set. 

    % Pull out of parameters structure for easier/safer use. Don't include 
    % the first row, which were interceps.
    % If the first-level comparison was categorical, use only the first
    % variable's betas
    if isfield(parameters, 'firstLevelCategorical') && parameters.firstLevelCategorical
        
         % Have ":" in 3rd dimension in case there are shuffles.
         responseVariables = parameters.response(2:end, 1, :); 
    else
         % Have ":" in 3rd dimension in case there are shuffles.
         responseVariables = parameters.response(2:end, :, :); 
    end

    % If there was more than one response variable at level 1 (will have
    % more than one column, before transposing)
    if size(responseVariables, 2) > 1
        
        % Reshape so its all one big row vector (shuffles in 3rd dim,
        % if applicable)
        responseVariables = reshape(responseVariables, [], 1, size(responseVariables,3));
    end

    % Transpose/permute so each beta is its own variable. (Permute in
    % case there are shuffles in 3rd dim.
    responseVariables = permute(responseVariables, [2 1 3]);

    % ***Handle the explanatory variables (mouse dummy variables)***

    % Get the mouse iterator value
    mouse_level = find(strcmp(parameters.loop_list.iterators(:,1), 'mouse'));
    current_mouse_iterator = parameters.values{numel(parameters.values)/2 + mouse_level};
    
    explanatoryVariables = zeros(1, parameters.max_mice); 
    explanatoryVariables(current_mouse_iterator) = 1;

    % *** Concatenate ***

    % Check the concatenation level.
    % Get the current iterator value for that level (which should be mice)
    iterator_level = find(strcmp(parameters.loop_list.iterators(:,1), parameters.concatenation_level));
    current_iterator = parameters.values{numel(parameters.values)/2 + iterator_level};

    % If the current iterator is 1, that means you're starting a new
    % concatenation, clear any previously concatenated data.
    if current_iterator == 1 && isfield(parameters, 'responseVariables_concatenated')
        parameters = rmfield(parameters, 'responseVariables_concatenated'); 
        parameters = rmfield(parameters, 'explanatoryVariables_concatenated'); 
    end 
   
    % Create concatenation arrays, if hasn't already been done for this
    % comparison.
    if ~isfield(parameters, 'explanatoryVariables_concatenated')
        parameters.explanatoryVariables_concatenated = [];
    end
    if ~isfield(parameters, 'responseVariables_concatenated')
       parameters.responseVariables_concatenated = [];
    end
    
    % Concatenate.
    % Always concatenate across 1st dimension (rows)
    explanatoryVariables_concatenated = cat(1, parameters.explanatoryVariables_concatenated, explanatoryVariables);
    responseVariables_concatenated = cat(1, parameters.responseVariables_concatenated, responseVariables);

    % For explanatory variables (mice), remove any columns that don't have
    % a 1 in it (will happen if one mouse is not used in this comparison).

    % Only do if the mouse iterator is the maximum number of mice,
    if current_iterator == parameters.max_mice
        columns_to_remove = ~any(explanatoryVariables_concatenated);

        % If there are columns to remove, remove them.
        if ~isempty(columns_to_remove)
            explanatoryVariables_concatenated(:, columns_to_remove) = [];  
        end
    end
    % Put into parameters structure for next iteration
    parameters.explanatoryVariables_concatenated = explanatoryVariables_concatenated;
    parameters.responseVariables_concatenated = responseVariables_concatenated;

    % *** Normalize ***
    % (does this for each concatenation, but will only save the last)
%    [explanatoryVariables_normalized, mu_explanatory, sigma_explanatory] = zscore(parameters.explanatoryVariables_concatenated);
%    [responseVariables_normalized, mu_response, sigma_response] = zscore(parameters.responseVariables_concatenated);
% 
%    % *** Put into output structure ***
% 
%    % Zscoring info.
%    dataset.zscoring.explanatoryVariables.mu = mu_explanatory;
%    dataset.zscoring.explanatoryVariables.sigma = sigma_explanatory;
%    dataset.zscoring.responseVariables.mu = mu_response;
%    dataset.zscoring.responseVariables.sigma = sigma_response;

   % Put both variable sets for this comparison into a stucture for saving. 
%    dataset.explanatoryVariables = explanatoryVariables_normalized;
%    dataset.responseVariables = responseVariables_normalized;

   dataset.explanatoryVariables = explanatoryVariables_concatenated;
   dataset.responseVariables = responseVariables_concatenated;

   % Put into parameters
   parameters.dataset = dataset;
   
end
