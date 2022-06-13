% PlotBetas.m
% Sarah West 
% 6/6/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBetas(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.results.BETA, 2),4/5);

    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjust_beta') && parameters.adjust_beta
        betas_adjusted = parameters.results.BETA(2:end, :) ./ parameters.dataset_info.zscoring.brainData.sigma' .*  parameters.dataset_info.zscoring.responseVariables.sigma; 
    else
        betas_adjusted = parameters.results.BETA(2:end, :);
    end
    
    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:size(betas_adjusted, 2)

        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);

        
        holder(parameters.indices) = betas_adjusted(:, componenti);

        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar; 
        caxis(color_range);
        title(['Variable ' num2str(componenti)]); axis square;

    end
    title_string = ['Betas ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')];
    title_string = strrep(title_string, '_', ' ');
    sgtitle(title_string);

    % Put into output structure.
    parameters.fig = fig;
end 
