% PlotWeights.m
% Sarah West 
% 6/9/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotWeights(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(numel(parameters.components_to_plot),4/5);

    indices = logical(tril(ones(parameters.number_of_sources), -1));

    weights = parameters.results.stats.W; 

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = parameters.components_to_plot

        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);

        % First row is constant estimate
        holder(indices) = weights(:, componenti);

        % Get most negative or positive value for color range, center
        % around 0.
        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar; caxis(color_range)
        title(num2str(componenti)); axis square;

    end
    sgtitle(['Weights' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')])

    % Put into output structure.
    parameters.fig = fig;
end 
