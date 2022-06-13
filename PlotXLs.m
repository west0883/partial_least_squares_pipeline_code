% PlotXLs.m
% Sarah West 
% 6/6/22

% Plot a number of X loadings from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotXLs(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.results.XL, 2),4/5);

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:size(parameters.results.XL, 2)
        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
        holder(parameters.indices) = parameters.results.XL(:, componenti);

        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); 
        colorbar; caxis(color_range);

        title(['XL ' num2str(componenti)]); axis square;
    end
    title_string = ['X loadings ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')];
    title_string = strrep(title_string, '_', ' ');
    sgtitle(title_string);
 
    % Put into output structure.
    parameters.fig = fig;
end 
