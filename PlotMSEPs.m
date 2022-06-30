% PlotWeights.m
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
            title_string = ['MSEPS for explanatory variables'];
        else
            title_string = ['MSEPS for explanatory variables ' parameters.values(1)];
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

    % Normalize height of MSEPS.
    MSEP_normalized = parameters.results.maximal_components.MSEP(1,:);
    %ratio = MSEP_normalized(1)/size(parameters.dataset.explanatoryVariables, 2);
    MSEP_normalized = MSEP_normalized/MSEP_normalized(1);

    plot(component_vector, MSEP_normalized);
    legend(parameters.this_comparison_set(:).name);

    % Plot responses only if user says so.
    if isfield(parameters, 'plot_MSEPs_response') && parameters.plot_MSEPs_response
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
        hold on;
        
        % Normalize height of MSEPS.
        MSEP_normalized = parameters.results.maximal_components.MSEP(2,:);
        %ratio = MSEP_normalized(1)/size(parameters.dataset.responseVariables, 2);
        MSEP_normalized = MSEP_normalized/MSEP_normalized(1);
    
        plot(parameters.y_axis, component_vector, MSEP_normalized);
        legend(parameters.this_comparison_set(:).name);
    end
end 
