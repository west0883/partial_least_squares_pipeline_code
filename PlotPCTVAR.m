% PlotPCTVAR.m
% Sarah West 
% 6/9/22

% Plot a percent variance from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotPCTVAR(parameters)

 

    if ~isfield(parameters,'xfig')
        parameters.xfig = figure;
        parameters.xfig.WindowState = 'maximized';
        set(0, 'CurrentFigure', parameters.xfig); 
        
        % Only put in the iterator names if it's a level 1 analysis
        if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
            title_string = ['Percent variance for explanatory variables'];
        else
            title_string = ['Percent variance for explanatory variables ' parameters.values(1)];
        end

        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('PCTVAR');
        ylim([0 1]);
        hold on; 
    end

% 
%     % Find the comparison name for legend. 
%     name_location = find(cellfun(@strcmp, parameters.keywords, repmat({'comparison'}, size(parameters.keywords)))); 
%     comparison_name = parameters.values{name_location};
%     legend_array = [legend_array; {comparison_name}]; 

    set(0, 'CurrentFigure', parameters.xfig); 
    hold on; 
    plot(cumsum(parameters.results.PCTVAR(1,:)));
    legend(parameters.this_comparison_set(:).name);


    % Plot responses only if user says so.
    if isfield(parameters, 'plot_PCTVAR_response') && parameters.plot_PCTVAR_response

        if ~isfield(parameters,'yfig')
            parameters.yfig = figure;
            parameters.yfig.WindowState = 'maximized';
          
            set(0, 'CurrentFigure', parameters.yfig); 
            % Only put in the iterator names if it's a level 1 analysis
            if isfield(parameters, 'analysis_level') && parameters.analysis_level == 2
                title_string = ['Percent variance for response variables'];
            else
                title_string = ['Percent variance for response variables ' parameters.values(1)];
            end
       
            title(title_string);
            axis tight;
            xlabel('component number');
            ylabel('PCTVAR');
            ylim([0 1]);
    
            hold on; 
          
        end
        set(0, 'CurrentFigure', parameters.yfig); 
        hold on;
        plot (cumsum(parameters.results.PCTVAR(2,:)));
        legend(parameters.this_comparison_set(:).name);

    end
    
end 
