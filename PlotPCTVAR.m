% PlotPCTVAR.m
% Sarah West 
% 6/9/22

% Plot a percent variance from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotPCTVAR(parameters)

 

    if ~isfield(parameters,'xfig')
        parameters.xfig = figure;
        parameters.xfig.WindowState = 'maximized';
        parameters.x_axis = gca;
        title_string = ['Percent variance for explanatory variables ' parameters.values(1)];
        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('PCTVAR');

        hold on; 
    end

    hold on; 


    if ~isfield(parameters,'yfig')
        parameters.yfig = figure;
        parameters.yfig.WindowState = 'maximized';
      
        parameters.y_axis = gca;
        title_string = ['Percent variance for response variables ' parameters.values(1)];
        title(title_string);
        axis tight;
        xlabel('component number');
        ylabel('PCTVAR');

        hold on; 
    end
    hold on;
    
    plot(parameters.x_axis, cumsum(parameters.results.PCTVAR(1,:)));
    plot(parameters.y_axis, cumsum(parameters.results.PCTVAR(2,:)));

    legend;
end 
