% PlotMSEPs.m
% Sarah West 
% 6/9/22

% Plot a number of MSEPs from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotMSEPs(parameters)

    if ~isfield(parameters,'xfig')
        parameters.xfig = figure;
        parameters.xfig.WindowState = 'maximized';
        parameters.x_axis = gca;
        set(0, 'CurrentFigure', parameters.xfig); 
        
        % Only put in the iterator names if it's a level 1 analysis
        if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
            title_string = ['MSEPs for explanatory variables'];
        else
            title_string = ['MSEPs for explanatory variables ' parameters.values(1)];
        end
       
        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('MSEP');

        hold on; 
    end

    set(0, 'CurrentFigure', parameters.xfig); 
    hold on; 
    component_vector = 0:size(parameters.results.maximal_components.MSEP,2) - 1;

    % Normalize height of MSEPs.
    MSEPs_normalized = parameters.results.maximal_components.MSEP(1,:);
    %ratio = MSEPs_normalized(1)/size(parameters.dataset.explanatoryVariables, 2);
    MSEPs_normalized = MSEPs_normalized/MSEPs_normalized(1);

    plot(component_vector, MSEPs_normalized);
    if isfield(parameters, 'this_comparison_set')
        legend(parameters.this_comparison_set(:).name);
    end

    % Plot responses. Don't plot if user says not to.
    if isfield(parameters, 'plot_MSEPs_response') && ~parameters.plot_MSEPs_response
        % Do nothing.
    else
        if ~isfield(parameters,'yfig')
            parameters.yfig = figure;
            parameters.yfig.WindowState = 'maximized';
            parameters.y_axis = gca;

            % Only put in the iterator names if it's a level 1 analysis
            if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
                title_string = ['MSEPs for response variables'];
            else
                title_string = ['MSEPs for response variables ' parameters.values(1)];
            end

            title(title_string);
            axis tight;
            xlabel('component number');
            ylabel('MSEP');
    
            hold on; 
        end
        set(0, 'CurrentFigure', parameters.yfig); 
        hold on;
        
        % Normalize height of MSEPs.
        MSEPs_normalized = parameters.results.maximal_components.MSEP(2,:);
        %ratio = MSEPs_normalized(1)/size(parameters.dataset.responseVariables, 2);
        MSEPs_normalized = MSEPs_normalized/MSEPs_normalized(1);
    
        plot(component_vector, MSEPs_normalized);
        if isfield(parameters, 'this_comparison_set')
            legend(parameters.this_comparison_set(:).name);
        end
    end
    
end 
