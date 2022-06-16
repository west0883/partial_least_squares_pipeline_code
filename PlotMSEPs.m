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
        
        title_string = ['MSEPs for explanatory variables ' parameters.values(1)];
        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('MSEP');

        hold on; 
    end

    set(0, 'CurrentFigure', parameters.xfig); 
    hold on; 
    component_vector = 0:size(parameters.results.maximal_components.MSEP,2) - 1;
    plot(component_vector, parameters.results.maximal_components.MSEP(1,:));
    legend(parameters.this_comparison_set(:).name);

    % Plot responses only if user says so.
    if isfield(parameters, 'plot_MSEPs_response') && parameters.plot_MSEPs_response
        if ~isfield(parameters,'yfig')
            parameters.yfig = figure;
            parameters.yfig.WindowState = 'maximized';
          
            parameters.y_axis = gca;
            title_string = ['MSEPs for response variables ' parameters.values(1)];
            title(title_string);
            axis tight;
            xlabel('component number');
            ylabel('MSEP');
    
            hold on; 
        end
        hold on;
        
    
        plot(parameters.y_axis, component_vector, parameters.results.maximal_components.MSEP(2,:));
        legend(parameters.this_comparison_set(:).name);
    end
end 
