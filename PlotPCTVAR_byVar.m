% PlotPCTVAR.m
% Sarah West 
% 6/9/22

% Plot a percent variance from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotPCTVAR_byVar(parameters)

    % Get number of subplots (number of response variables + 1 for the
    % total)
    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(parameters.max_response_vars + 1, 4/5);

    if ~isfield(parameters,'yfig')

        parameters.yfig = figure;
        parameters.yfig.WindowState = 'maximized';

        set(0, 'CurrentFigure', parameters.yfig); 

        % Make a subplot for each variable.
        for subploti = 1:size(parameters.results.YL, 1) + 1

             subplot(subplot_rows, subplot_columns, subploti);
             xlabel('component number');
             ylabel('PCTVAR');
             hold on; 
      
        end
      
        title_string = ['Percent variance for response variables ' parameters.values(1)];
        sgtitle(title_string);
    end

    % Calculate percent variance by variable
    [variance_byvariable] = PercentvarByVariable(parameters.results.YL, parameters.dataset.responseVariables);

    set(0, 'CurrentFigure', parameters.yfig); 
    % for each variable,
    
    for variablei = 1:size(variance_byvariable, 1)

        subplot(subplot_rows, subplot_columns, variablei + 1);

        % If cumulative, 
        if isfield(parameters, 'percent_variance_cumulative') && parameters.percent_variance_cumulative
            plot(cumsum(variance_byvariable(variablei,:))); 
        else
            plot(variance_byvariable(variablei,:)); 
        end
        % legend(parameters.this_comparison_set(:).name);
        axis tight;
        title(['variable ' num2str(variablei)]);
        hold on;

    end 

    % Do total in FIRST plot
    subplot(subplot_rows, subplot_columns, 1);
    % If cumulative, 
    if isfield(parameters, 'percent_variance_cumulative') && parameters.percent_variance_cumulative
        plot(cumsum(sum(variance_byvariable,1))); 
    else
        plot(sum(variance_byvariable, 1)); 
    end
    legend(parameters.this_comparison_set(:).name);
    axis tight;
    title('total'); 
    hold on;

end 
