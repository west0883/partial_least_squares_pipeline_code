% PlotBetas.m
% Sarah West 
% 6/6/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBetas(parameters)
    
    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.results.Cov, 2),4/5);

    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjust_beta') && parameters.adjust_beta

        % *** Double check sigma multiplication***
        betas_adjusted = parameters.results.Cov .* parameters.dataset.zscoring.explanatoryVariables.sigma' ./  parameters.dataset.zscoring.responseVariables.sigma; 
    else
        betas_adjusted = parameters.results.Cov;
    end
    
    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:size(betas_adjusted, 2)

         if parameters.isCorrelationMatrix
    
            holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
    
            % First row is constant estimate
            holder(parameters.indices) = betas_adjusted(:, componenti);
        else
            holder = betas_adjusted(:, componenti);
        end 
        
        if isfield(parameters, 'caxis') 
            color_range = parameters.caxis;
        else
            extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
            color_range = [-extreme extreme]; 
        end

        %color_range = [-0.06 0.06];
        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar; 
        caxis(color_range);
        title(['Variable ' num2str(componenti)]); axis square;

    end
    title_string = ['Covariances ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')];
    title_string = strrep(title_string, '_', ' ');
    sgtitle(title_string);

    % Put into output structure.
    parameters.fig = fig;
end 
