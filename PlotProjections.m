% PlotProjections.m
% Sarah West 
% 6/6/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotProjections(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(numel(parameters.components_to_plot),4/5);

    indices = logical(tril(ones(parameters.number_of_sources), -1));

    projections = parameters.results.XL * parameters.results.YL'; 

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:numel(parameters.components_to_plot)

        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);

        % First row is constant estimate
        holder(indices) = projections(:, componenti);

        % Get most negative or positive value for color range, center
        % around 0.
        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar; caxis(color_range)
        title([(parameters.components_to_plot{componenti})]); axis square;

    end
    sgtitle(['Projections ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')])

    % Put into output structure.
    parameters.fig = fig;
end 
