% PlotWeights.m
% Sarah West 
% 6/9/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotWeights(parameters)

     % If this is a second level analysis, use the maximal components
      % YLs. 
      if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
          weights = parameters.results.maximal_components.YL;
      else
        weights = parameters.results.maximal_components.stats.W; 
      end

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(weights, 2),4/5);

    fig = figure;
    fig.WindowState = 'maximized';
    for componenti = 1:size(weights, 2)

        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);

        % First row is constant estimate
        holder(parameters.indices) = weights(:, componenti);

        % Get most negative or positive value for color range, center
        % around 0.
        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

        subplot(subplot_rows, subplot_columns, componenti); imagesc(holder); colorbar;  caxis(color_range)
        title(num2str(componenti)); axis square;

    end
    title_string = ['Weights ' strjoin(parameters.values(1:numel(parameters.values)/2), ', ')];
    title_string = strrep(title_string, '_', ' ');
    sgtitle(title_string);

    % Put into output structure.
    parameters.fig = fig;
end 
