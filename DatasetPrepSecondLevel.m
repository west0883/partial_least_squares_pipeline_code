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

    % *** Handle the response variables (betas)*** 

    % Transpose so each beta is its own variable 
    responseVariables = parameters.response';

    % If there was more than one response variable at level 1 (will have
    % more than one row, after transposing)
    if size(responseVariables, 1) > 1
        
        % Reshape so its all one big row vector
        % (have to un-transpose first, re-transpose after)
        responseVariables = responseVariables';
        responseVariables = reshape(responseVariables, [], 1);
        responseVariables = responseVariables'; 

    end

    % ***Handle the explanatory variables (mouse dummy variables)***
    explanatoryVariables = zeros(parameters.max_mice, 1); 
    explanatoryVariables(mouse_location) = 1;

    % *** Concatenate ***

    % Check the concatenation level.
    % Get the current iterator value for that level
    iterator_level = find(strcmp(parameters.loop_list.iterators(:,1), parameters.concatenation_level));
    current_iterator = parameters.values{numel(parameters.values)/2 + iterator_level};

    % If the current iterator is 1, that means you're starting a new
    % concatenation, clear any previously concatenated data.
    if current_iterator == 1 && isfield(parameters, 'responseVariables_concatenated')
        parameters = rmfield(parameters, 'responseVariables_concatenated'); 
        parameters = rmfield(parameters, 'explanatorVariables_concatenated'); 
    end 
   
    % Create concatenation arrays, if hasn't already been done for this
    % comparison.
    if ~isfield(parameters, 'explanatoryVariables_concatenated')
        parameters.explanatoryVariables_concatenated = [];
    end
    if ~isfield(parameters, 'responseVariables_concatenated')
       parameters.responseVariables_concatenated = [];
    end
    
    % Concatenate
    explanatoryVariables_concatenated = [parameters.explanatoryVariables_concatenated; explanatoryVariables];
    responseVariables_concatenated = [parameters.responseVariables_concatenated; responseVariables];

    % For explanatory variables (mice), remove any columns that don't have
    % a 1 in it (will happen if one mouse is not used in this comparison).
    columns_to_remove = ~any(explanatoryVariables_concatenated);

    % If there are columns to remove, remove them.
    if ~isempty(columns_to_remove)
        explanatoryVariables_concatenated(:, columns_to_remove) = [];  
    end

    % Put into parameters structure for next iteration
    parameters.explanatoryVariables_concatenated = explanatoryVariables_concatenated;
    parameters.responseVariables_concatenated = responseVariables_concatenated;

    % *** Normalize ***
    % (does this for each concatenation, but will only save the last)
   [explanatoryVariables_normalized, mu_explanatory, sigma_explanatory] = zscore(parameters.explanatoryVariables_concatenated);
   [responseVariables_normalized, mu_response, sigma_response] = zscore(parameters.responseVariables_concatenated);

   % *** Put into output structure ***

   % Zscoring info.
   dataset.zscoring.explanatoryVariables.mu = mu_explanatory;
   dataset.zscoring.explanatoryVariables.sigma = sigma_explanatory;
   dataset.zscoring.responseVariables.mu = mu_response;
   dataset.zscoring.responseVariables.sigma = sigma_response;

   % Put both variable sets for this comparison into a stucture for saving. 
   dataset.explanatoryVariables = explanatoryVariables_normalized;
   dataset.responseVariables = responseVariables_normalized;

   parameters.dataset = dataset;
end
