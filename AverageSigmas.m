% AverageSigmas.m
% Sarah West
% 7/7/22

% Average sigmas across mice per comparison, removing outliers if
% necessary/asked for.

function [parameters] = AverageSigmas(parameters)

    MessageToUser('Averaging sigmas for ', parameters);
    
    % Don't include this mouse if this mouse is in list of mice not to use.
    if strcmp(parameters.values(2), parameters.this_comparison_set(parameters.values{3}).mice_not_to_use)
        % Skip
        return
    else

       ysig = parameters.dataset.zscoring.responseVariables.sigma;

       % Keep only the first response variable if comparison type is
       % categorical.
       if strcmp(parameters.comparison_type, 'categorical')

           ysig = ysig(1);

       end

       % Get x sigma
       % If using just the x zscore, make xsigma = 1.
       if isfield(parameters, 'use_xZscore') && parameters.use_xZscore
           xsig_single = repmat(1, size(parameters.dataset.zscoring.explanatoryVariables.sigma));
       else
           xsig_single = parameters.dataset.zscoring.explanatoryVariables.sigma;
       end

       % Make dimensions match (replicate xsig so there's a set for each response varaible.
       xsig = repmat(xsig_single, size(ysig,2),1); 

       % Calculate sigmas
       sigmas = reshape(transpose(transpose(ysig)./xsig), 1, []);
    end

    % If the user gave a concatenation level value field (from
    % ConcatenateData.m code)
    if isfield(parameters,'concatenation_level')

        % Get the current iterator value for that level
        iterator_level = find(strcmp(parameters.loop_list.iterators(:,1), parameters.concatenation_level));
        current_iterator = parameters.values{numel(parameters.values)/2 + iterator_level};

        % If the current iterator is 1, that means you're starting a new
        % concatenation, clear any previously concatenated data.
        if current_iterator == 1 && isfield(parameters, 'sigmas_concatenated')
            parameters = rmfield(parameters, 'sigmas_concatenated'); 
        end 
    end 

    % Concatenate across mice, if sigmas_concatenated exists
    if isfield(parameters, 'sigmas_concatenated') 
        sigmas_concatenated = cat(parameters.concatDim, parameters.sigmas_concatenated, sigmas);
    % Else create sigmas_concatenated for first time. 
    else
       sigmas_concatenated = sigmas;
    end

    % Put concatenation into parameters for next iteration. 
    parameters.sigmas_concatenated = sigmas_concatenated;

    % Remove outliers
    if isfield(parameters, 'removeOutliers') && parameters.removeOutliers

        outliers = isoutlier(sigmas_concatenated, parameters.concatDim);
        
        % Replace outliers with NaNs.
        sigmas_concatenated(outliers) = NaN;

        % Put list of outliers into an output field of parameters.
        parameters.sigma_outliers = outliers;
    end

    % Take average
    average_sigmas = mean(sigmas_concatenated, parameters.concatDim, 'omitnan');

    % Put average into output field of parameters.
    parameters.average_sigmas = average_sigmas;

end 