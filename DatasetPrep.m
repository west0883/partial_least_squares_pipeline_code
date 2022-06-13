
function [parameters] = DatasetPrep(parameters)

    % Find the location & value of the comparison iterator in
    % parameters.values
    iterator_location = find(cellfun(@strcmp, parameters.keywords, repmat({'comparison_iterator'}, size(parameters.keywords)))); 
    comparison_iterator = parameters.values{iterator_location};

    % Grab comparison name, indices, variables to use.
    %comparison_name = parameters.comparisons_firstlevel(comparison_iterator).name; % Might not need (is really only for saving files)
    comparison_indices = parameters.comparisons_firstlevel(comparison_iterator).indices;
    comparison_variablesToUse = parameters.comparisons_firstlevel(comparison_iterator).variablesToUse; 

    % Get the brain data period indices you're interested in. 
    explanatoryVariables = parameters.explanatory(comparison_indices);

    % Get the response variables period indices & variables you're
    % interested in. 
    % responseVariables is now an array
    holder = cell(1, numel(comparison_variablesToUse));
    for i = 1:numel(comparison_variablesToUse)
        holder{i} = parameters.response_variables{comparison_indices, comparison_variablesToUse{i}}; 

    end
    responseVariables = horzcat(holder{:}); 

    % Reshape explanatoryVariables to make each roll into an instance, inside each period cell. 
    % The 2 reshape inputs 
    input1 =  repmat({size(explanatoryVariables{1}, 1)}, size(explanatoryVariables));
    input2 = repmat({[]}, size(explanatoryVariables));
    explanatoryVariables = cellfun(@reshape, explanatoryVariables, input1, input2, 'UniformOutput', false);
 
    % For each response variable (each column of responseVariables),
    % reshape to make each roll into an instance, inside each period cell.
    for variablei = 1:numel(comparison_variablesToUse)
        
        % The 2 reshape inputs
        input1 =  repmat({size(responseVariables{1, variablei}, 1)}, size(explanatoryVariables));
        input2 = repmat({[]}, size(explanatoryVariables));
        
        % Reshape
        responseVariables(:, variablei) =  cellfun(@reshape, responseVariables(:, variablei), input1, input2, 'UniformOutput', false);

    end 

    % **Transpose
    explanatoryVariables = cellfun(@transpose, explanatoryVariables, 'UniformOutput', false);
    responseVariables = cellfun(@transpose, responseVariables, 'UniformOutput', false);

    % **Concatenate brain data & response variables vertically across periods. 

    % explanatoryVariables has only one cell array column, so can be done all at once.
    explanatoryVariables = vertcat(explanatoryVariables{:});

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
    variable_category_column_numbers = cellfun('size', responseVariables_separateVariables, 2);

    columns_to_use = cell(1, numel(comparison_variablesToUse)); 
    column_counter = 0;
    for variablei = 1:numel(comparison_variablesToUse)
        columns_to_use{variablei} = column_counter + 1:variable_category_column_numbers(variablei); % Defined this separately for clarity.
        column_counter = column_counter + variable_category_column_numbers(variablei);
    end

    % Horizontally concatenate the different response variable categories.
    responseVariables = horzcat(responseVariables_separateVariables{:}); 

    % Zscore both variable sets. Keep mu & sigmas for better interprebility
    % of betas later.
    [explanatoryVariables, mu_brain, sigma_brain] = zscore(explanatoryVariables);
    [responseVariables, mu_response, sigma_response] = zscore(responseVariables);
    dataset.zscoring.explanatoryVariables.mu = mu_brain;
    dataset.zscoring.explanatoryVariables.sigma = sigma_brain;
    dataset.zscoring.responseVariables.mu = mu_response;
    dataset.zscoring.responseVariables.sigma = sigma_response;

    % For convenience, put both variable sets for this comparison into a stucture for saving. 
    dataset.explanatoryVariables = explanatoryVariables;
    dataset.responseVariables = responseVariables;
    %dataset.responseVariables_separateVariables = responseVariables_separateVariables;
    dataset.variable_category_column_numbers = variable_category_column_numbers; % For telling which columns belong to which category later

    % Put dataset into output structure.
    parameters.dataset = dataset;


end