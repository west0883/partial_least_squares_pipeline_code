% PlotBetas.m
% Sarah West 
% 6/6/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBetas(parameters)

    % Define number of sources (if is different for each mouse)
    if isfield(parameters, 'define_number_of_sources') && parameters.define_number_of_sources
        corr_num = size(parameters.results.Cov, 1);
        
        % (found this with a quadratic equation)
        parameters.number_of_sources = 0.5 * (1 + sqrt(8 * corr_num + 1 ));
        
        parameters.indices = find(tril(ones(parameters.number_of_sources), -1));
    end 
    
    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.results.Cov, 2),4/5);

    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjust_beta') && parameters.adjust_beta
        betas_adjusted = parameters.results.Cov ./ parameters.dataset_info.zscoring.explanatoryVariables.sigma' .*  parameters.dataset_info.zscoring.responseVariables.sigma; 
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
        
        holder(parameters.indices) = betas_adjusted(:, componenti);

        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

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
