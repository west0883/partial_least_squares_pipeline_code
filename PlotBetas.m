% PlotBetas.m
% Sarah West 
% 6/6/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotXLs(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(numel(parameters.components_to_plot),4/5);

    indices = logical(tril(ones(parameters.number_of_sources), -1));

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:numel(parameters.components_to_plot)

        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);

        % First row is constant estimate
        holder(indices) = parameters.BETA(2:end, componenti);

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar; %caxis(parameters.color_range)
        title(['Beta ' (parameters.components_to_plot{componenti})]); axis square;

    end
    sgtitle(['Betas ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')])

    % Put into output structure.
    parameters.fig = fig;
end 
