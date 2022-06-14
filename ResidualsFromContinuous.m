% SubtractContinuousVariables.m
% Sarah West
% 6/13/22

% Removes Beta * continuous Y response variable from X explanatory variables
% after continuous variable regressions.

function [parameters] = ResidualsFromContinuous(parameters)

    MessageToUser('Residuals from ', parameters);

    % Find the location & value of the type iterator in
    % parameters.values
    iterator_location = logical(cellfun(@strcmp, parameters.keywords, repmat({'comparison_iterator'}, size(parameters.keywords)))); 
    comparison_iterator = parameters.values{iterator_location};

    % Get the comparison & variables used for regression from comparison 
    indicesToUse = parameters.comparisons_continuous(comparison_iterator).indices;

    % Grab the X residuals from the continuous variables regression.
    Xnew = parameters.PLSR_results.stats.Xresiduals;
   
    % Un-normalize (because will need to be normalized with other behavior
    % types in across-category comparisons later).
    Xnew = Xnew .* parameters.dataset.zscoring.explanatoryVariables.sigma + parameters.dataset.zscoring.explanatoryVariables.mu;

%     % Put Xnew into output.
%     parameters.explanatory_new = Xnew; 

    % Start putting Xnew back into the big correlations matrix

    % Transpose 
    Xnew = Xnew'; 

    % If there isn't a field for values_new yet, make it & fill it with the
    % old explanatory values matrix.
    if ~isfield(parameters, 'values_new')
        parameters.values_new = parameters.values_old;
    end

    % Set up a counter to hold your place in new values. Can do this
    % because you concatenated only these indices the first time in
    % DatasetPrep.m
    counter = 1;
   
    % Find each cell entry of old values that corresponds to this behavior
    % type.
    for indexi = 1:numel(indicesToUse)
        index = indicesToUse(indexi);

        % Get the roll number & instances from this 
        rollnumber = size(parameters.values_old{index}, 2);
        instancesnumber = size(parameters.values_old{index}, 3);

        % Pull out the matrices that belong to this cell/period
        subset = Xnew(:, counter : (counter + rollnumber * instancesnumber - 1));

        % Reshape to match this cell/period, put into place.
        parameters.values_new{index} = reshape(subset, size(subset,1), rollnumber, instancesnumber);

        % Update counter by number of matrices included in that cell
        counter = counter + rollnumber * instancesnumber;
    end

    % Counter should be 1 more than the size of Xnew by the end
    if counter ~= size(Xnew, 2) + 1
        error('counter should equal 1 more than size of the new explanatory value');
    end

end 
