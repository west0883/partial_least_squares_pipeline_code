% PlotBICs.m
% Sarah West 
% 6/9/22

% Plot a number of BICs from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBICs(parameters)

    if ~isfield(parameters,'xfig')
        parameters.xfig = figure;
        parameters.xfig.WindowState = 'maximized';
        parameters.x_axis = gca;
        set(0, 'CurrentFigure', parameters.xfig); 
        
        % Only put in the iterator names if it's a level 1 analysis
        if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
            title_string = ['BICs for explanatory variables'];
        else
            title_string = ['BICs for explanatory variables ' parameters.values(1)];
        end
       
        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('BIC');

        hold on; 
    end

    set(0, 'CurrentFigure', parameters.xfig); 
    hold on; 
    component_vector = 1:size(parameters.results.maximal_components.BIC,2);

    % Normalize height of BICs.
    BICs_normalized = parameters.results.maximal_components.BIC(1,:);
    %ratio = BICs_normalized(1)/size(parameters.dataset.explanatoryVariables, 2);
    BICs_normalized = BICs_normalized/BICs_normalized(1);

    plot(component_vector, BICs_normalized);
    legend(parameters.this_comparison_set(:).name);

    % Plot responses. Don't plot if user says not to.
    if isfield(parameters, 'plot_BICs_response') && ~parameters.plot_BICs_response
        % Do nothing.
    else
        if ~isfield(parameters,'yfig')
            parameters.yfig = figure;
            parameters.yfig.WindowState = 'maximized';
            parameters.y_axis = gca;

            % Only put in the iterator names if it's a level 1 analysis
            if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
                title_string = ['BICs for response variables'];
            else
                title_string = ['BICs for response variables ' parameters.values(1)];
            end

            title(title_string);
            axis tight;
            xlabel('component number');
            ylabel('BIC');
    
            hold on; 
        end
        set(0, 'CurrentFigure', parameters.yfig);
        hold on;
        
        % Normalize height of BICs.
        BICs_normalized = parameters.results.maximal_components.BIC(2,:);
        %ratio = BICs_normalized(1)/size(parameters.dataset.responseVariables, 2);
        BICs_normalized = BICs_normalized/BICs_normalized(1);
   
        plot(component_vector, BICs_normalized);
        legend(parameters.this_comparison_set(:).name);
    end
    
end 
