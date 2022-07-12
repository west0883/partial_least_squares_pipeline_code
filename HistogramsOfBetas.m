% HistogramsOfBetas.m
% Sarah West
% 7/12/22

function [parameters] = HistogramsOfBetas(parameters)

    % Pull out data
    data = parameters.data;

    % Get the xlimits
    extreme = max(max(data, [], 'all', 'omitnan'), abs(min(data, [], 'all', 'omitnan')));
    x_limits = [-extreme extreme];

    fig = figure; 
    fig.WindowState = 'maximized';
    for mousei = 1:size(data, 1)

        subplot(2,4, mousei); 
        histogram(data(mousei,:));
        xlim(x_limits);

        title(parameters.mice_all(mousei).name);
    end

    sgtitle(parameters.values{1});
    parameters.fig = fig; 
end 