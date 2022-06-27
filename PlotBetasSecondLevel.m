% PlotBetasSecondLevel.m
% Sarah West 
% 6/26/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBetasSecondLevel(parameters)

    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.this_comparison_set, 2),4/5);

    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjust_beta') && parameters.adjust_beta
        betas_adjusted = parameters.results.BETA(1, :) ./ parameters.dataset_info.zscoring.brainData.sigma' .*  parameters.dataset_info.zscoring.responseVariables.sigma; 
    else
        betas_adjusted = parameters.results.BETA(1, :);
    end
    
    % If there isn't a figure for this yet, make one.
    if ~isfield(parameters, 'fig')
        fig = figure;
        fig.WindowState = 'maximized';
        
        % Put into output structure.
        parameters.fig = fig;
        hold on;
    end
  
    % Get comparison iterator for subplot location. 
    iterator_location = strcmp(parameters.keywords, {'comparison_iterator'});
    comparison_iterator = parameters.values{iterator_location};

    % Get comparison name for subplot title. 
    name_location = strcmp(parameters.keywords, {'comparison'});
    comparison = parameters.values{name_location};

    holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
    holder(parameters.indices) = betas_adjusted;

    extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
    color_range = [-extreme extreme]; 

    %color_range = [-0.06 0.06];
    subplot(subplot_rows, subplot_columns, comparison_iterator); imagesc(holder); colorbar; 
    caxis(color_range);

    % Make subplot title.
    title_string = erase(comparison, parameters.comparison_type); 
    title(strrep(title_string, '_', ' ')); axis square;

    title_string = ['Betas, second level ' parameters.comparison_type ', ' num2str(parameters.ncomponents_from_first_level) ' 1st level & ' num2str(parameters.ncomponents_max) ' 2nd level components'];
    title_string = strrep(title_string, '_', ' ');
    sgtitle(title_string);

end 
