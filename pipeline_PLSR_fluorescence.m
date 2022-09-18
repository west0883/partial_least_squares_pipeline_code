% pipeline_PLSR_fluorescence.m
% Sarah West
% 9/18/22

% Pipeline of PLSR on fluorescence traces. 

% Fluorescence timeseries are already segmented, grouped by behavior,
% rolled
% Initial set-up



%% Average fluorescence within roll 
% (so you have 1 value per roll instead of 20)


%% Average fluorescence across matching nodes
% (across hemispheres, so you'll have half the values as nodes)

%% Permute fluorescence data to match formatting of correlations


%% Run the PLSR pipeline