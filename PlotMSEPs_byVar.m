% PlotMSEPs_byVar.m
% Sarah West 
% 6/9/22

% Plot a percent variance from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotMSEPs_byVar(parameters)

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
             ylabel('MSEP');
             hold on; 
      
        end
      
        title_string = ['MSEP for response variables ' parameters.values(1)];
        sgtitle(title_string);
    end

    set(0, 'CurrentFigure', parameters.yfig); 
   
    % Get out MSEPs by variable.
    MSEPs_byVar = parameters.results.maximal_components.MSEP_byVars;
    
    % for each variable,

    for variablei = 1:size(MSEPs_byVar, 1)

        subplot(subplot_rows, subplot_columns, variablei + 1);
        plot(MSEPs_byVar(variablei,:)); 
        % legend(parameters.this_comparison_set(:).name); 
        axis tight;
        title(['variable ' num2str(variablei)]);
        hold on;

    end 

    % Do total in FIRST plot
    subplot(subplot_rows, subplot_columns, 1);
    plot(sum(MSEPs_byVar, 1)); 
    legend(parameters.this_comparison_set(:).name);
    % axis tight;
    title('total'); 
    hold on;

end 
