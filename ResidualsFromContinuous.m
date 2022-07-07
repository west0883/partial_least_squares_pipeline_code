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

    % Get out response variables (for outlier replacement imputation).
    responseVariables = parameters.responseVariables; s

    % Make a version of Xnew that won't have any outliers removed (for
    % counting where to find instances that were entirely removed as
    % outliers)
    Xnew_old = Xnew; 

    % Un-normalize (because will need to be normalized with other behavior
    % types in across-category comparisons later).
    Xnew = Xnew .* parameters.dataset.zscoring.explanatoryVariables.sigma + parameters.dataset.zscoring.explanatoryVariables.mu;

    % Remove any outliers, if user says so.
    if isfield (parameters, 'removeOutliers') && parameters.removeOutliers
        outliers = isoutlier(Xnew, 1);

        % Replace outliers with NaN
        Xnew(outliers) = NaN;

        % Put important info into output structures.
        dataset.outliers.indices = find(outliers);
        if ~isempty(outliers)
            [row, column] =  ind2sub([size(Xnew)], find(outliers));
            dataset.outliers.indices_rowcolumn = [row, column];
        else
            dataset.outliers.indices = [];
        end
        dataset.outliers.values_old = Xnew_old(dataset.outliers.indices);

        % If any rows (observations) have more than 1/10 the explanatory values
        % as outliers, remove that observation.
        summation = sum(outliers,2);
        holder = find(summation > size(Xnew,2) * 1/10);
        dataset.outliers.removed_indices = holder;

        % If there are observations with this, remove. 
        if ~isempty(holder)
            Xnew(holder, :) = [];
            responseVariables(holder, :) = [];
        end
    end

    % If user says to (default is not to), impute missing values. MUST
    % impute missing if outliers were removed.
    if (isfield(parameters, 'imputeMissing') && parameters.imputeMissing) || (isfield(parameters, 'removeOutliers') && parameters.removeOutliers)

        disp('Imputing missing data.')

        % Run the modified code for trimmed square regression (TSR) for PLS
        [Xnew, ~, iterations_needed, tolerance_reached, components_needed] = plsmbtsr1_TSRonly(Xnew, responseVariables, parameters.imputation_components_variance_explained);% parameters.imputation_ncomponents); 

        % Put iterations needed and tolerance reached into dataset
        % structure.
        dataset.missing_data_imputation.iterations_needed = iterations_needed;
        dataset.missing_data_imputation.tolerance_reached = tolerance_reached;
        dataset.missing_data_imputation.components_needed = components_needed;
    end

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

        % Get a vector of the instances of Xnew that correspond to this
        % period/index of values_old.
        subset_instances_vector = counter : (counter + rollnumber * instancesnumber - 1);

        % Deal with any instances removed because had too many outliers.

        % If outliers were found, 
        if isfield (parameters, 'removeOutliers') && parameters.removeOutliers

            % See if holder (has the observations that were thrown out for
            % too many outliers) intersects with subset_instances_vector.
            if ~isempty(holder) && ~isempty(instersect(holder, subset_instances_vector))
                
                number_instances_removed = numel(instersect(holder, subset_instances_vector));

                % Remove the instances that should be removed. 
                subset_instances_vector = subset_instances_vector(1:end - number_instances_removed); 

                % Update instances number.
                instancesnumber = instancesnumber - number_instances_removed;
            end
        end
 
        % Pull out the matrices that belong to this cell/period
        subset = Xnew(:, subset_instances_vector);

        % Reshape to match this cell/period, put into place.
        parameters.values_new{index} = reshape(subset, size(subset,1), rollnumber, instancesnumber);

        % Update counter by number of matrices included in that cell
        counter = counter + rollnumber * instancesnumber;
    end

    % Counter should be 1 more than the size of Xnew by the end
    if counter ~= size(Xnew, 2) + 1
        error('counter should equal 1 more than size of the new explanatory value');
    end

    % Put dataset info into output structure
    parameters.dataset = dataset;
end 
