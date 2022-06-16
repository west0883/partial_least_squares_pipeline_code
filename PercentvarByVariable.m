% PercentvarByVariable
% Sarah West
% 6/16/22

% Calculates the percent variance explained by each PLSR component for each
% variable asked for. (Do for response variables only, you don't want a
% plot for every correlation value).

% Inputs: 
% YL -- is the loadings of each component/latent variable for the response
% variables.
% Y0 -- the normalized response variable dataset.
function [variance_byvariable] = PercentvarByVariable(YL, Y0)

       variance_byvariable = abs(YL).^2 ./ sum(abs(Y0).^2,1)';

end