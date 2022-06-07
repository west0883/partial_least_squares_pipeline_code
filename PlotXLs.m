% PlotXLs.m
% Sarah West 
% 6/6/22

% Plot a number of X loadings from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotXLs(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(numel(parameters.components_to_plot),4/5);

    indices = logical(tril(ones(parameters.number_of_sources), -1));

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:numel(parameters.components_to_plot)
        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
        holder(indices) = parameters.XL(:, parameters.components_to_plot(componenti));
        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); caxis(parameters.color_range)
        title(['XL ' num2str(parameters.components_to_plot(componenti))]); axis square;
    end
    sgtitle(['X loadings ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')])

    % Put into output structure.
    parameters.fig = fig;
end 
