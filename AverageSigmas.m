% AverageSigmas.m
% Sarah West
% 7/7/22

% Average sigmas across mice per comparison, removing outliers if
% necessary/asked for.

function [parameters] = AverageSigmas(parameters)
    
    % Don't include this mouse if this mouse is in list of mice not to use.
    if strcmp(parameters.values(2), parameters.this_comparison_set(parameters.values{3}).mice_not_to_use)
        % Skip
    else
       ysig = parameters.dataset.zscoring.responseVariables.sigma;

       % Make dimensions match (replicate corrs so there's a set for each response varaible.
       xsig = repmat(parameters.dataset.zscoring.explanatoryVariables.sigma, size(ysig,2),1); 

       % Calculate sigmas
       sigmas = reshape(transpose(transpose(ysig)./xsig), 1, []);
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